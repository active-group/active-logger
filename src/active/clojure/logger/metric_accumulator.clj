(ns active.clojure.logger.metric-accumulator
  "Metrics."
  (:require [active.clojure.logger.time :as time]

            [active.clojure.logger.metric-store :as metric-store]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-types :as metric-types]

            [active.clojure.logger.event :as event]

            [clojure.spec.alpha :as s]))

(s/def ::metric-store (partial instance? clojure.lang.Ref))

(s/fdef fresh-metric-store
  :ret ::metric-store)
(defn ^:no-doc fresh-metric-store
  []
  (ref (metric-store/fresh-metric-store)))

(defonce metric-store (fresh-metric-store))

(s/fdef reset-global-metric-store!
  :args (s/cat)
  :ret nil)
(defn reset-global-metric-store!
  []
  (dosync (ref-set metric-store (metric-store/fresh-metric-store)))
  nil)

;; -----------------------------------------------------------------

;; small reflection api (for backwards compatibility)

(def metric-name metric-types/metric-name)
(def metric-type metric-types/metric-type)
(def metric-help metric-types/metric-help)

;; -----------------------------------------------------------------

(s/fdef record-metric!
  :args (s/cat :optional-1  (s/? (s/cat :a-metric-store ::metric-store))
               :metric      ::metric-types/metric
               :labels      ::metric-types/metric-labels
               :value-value ::metric-types/metric-value
               :optional-2  (s/? (s/cat :last-update (s/nilable ::metric-types/metric-last-update-time-ms))))
  :ret nil)
(defn record-metric!
  "Record a metric."
  ([metric labels value-value]
   (record-metric! metric-store metric labels value-value nil))
  ([metric labels value-value last-update]
   (record-metric! metric-store metric labels value-value last-update))
  ([a-metric-store metric labels value-value last-update]
   (let [time-ms (or last-update (time/get-milli-time!))]
     (dosync
      (let [metric-store (ensure a-metric-store)]
        ;; Note: most of the time (and likely when performance matters), we already have the metric and the labels in the maps;
        ;; so we can make a direct alter on the values ref inside, without writing the same map back.
        (when-not (metric-store/maybe-record-metric! metric-store metric labels value-value time-ms)
          (ref-set a-metric-store (metric-store/record-metric metric-store metric labels value-value time-ms))))))
   nil))

(declare metric-name)

(s/fdef get-metric-samples!
  :args (s/cat :optional (s/? (s/cat :a-metric-store ::metric-store))
               :metric         ::metric-types/metric
               :labels         ::metric-types/metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn get-metric-samples!
  "Return all metric-samples for a given metric within the given
  metric-store with the given labels."
  ([metric labels]
   (get-metric-samples! metric-store metric labels))
  ([a-metric-store metric labels]
   (if-let [snapshot (metric-store/get-metric-snapshot @a-metric-store metric labels)]
     (metric-samples/snapshot->metric-samples metric snapshot labels)
     [])))

(defn ^:no-doc all-snapshots->all-metric-sample-sets
  [all-snapshots]
  (map (fn [[metric snapshots]]
          (metric-samples/make-metric-sample-set
           (metric-types/metric-name metric)
           (metric-types/metric-type metric)
           (metric-types/metric-help metric)
           (metric-samples/all-snapshots->all-metric-samples metric snapshots)))
        all-snapshots))

(s/fdef get-all-metric-sample-sets!
  :args (s/cat :optional (s/? (s/cat :a-metric-store ::metric-store)))
  :ret (s/coll-of (s/coll-of ::metric-samples/metric-sample-set)))
(defn get-all-metric-sample-sets!
  "Return all metric-samples-sets within the given metric-store.
  If no metric-store is given, use the global metric store."
  ([]
   (get-all-metric-sample-sets! metric-store))
  ([a-metric-store]
   (all-snapshots->all-metric-sample-sets (metric-store/get-all-snapshots @a-metric-store))))

(s/fdef prune-stale-metrics!
  :args (s/cat :optional (s/? (s/cat :a-metric-store ::metric-store))
               :time-ms        ::metric-types/metric-last-update-time-ms)
  :ret nil)
(defn prune-stale-metrics!
  "Prune all metrics in the `a-metric-store` that are older than `time-ms`. That is,
  the last update time in ms of the metric value is smaller than `time-ms`.
  If no metric-store is given, use the global metric store."
  ([time-ms]
   (prune-stale-metrics! metric-store time-ms))
  ([a-metric-store time-ms]
   ;; find candidates outside transactions first
   (doseq [[metric labels last-update-time-ms] (metric-store/find-stale-metrics @a-metric-store time-ms)]
     ;; then remove one candidate after the other, re-checking the timestamp (does nothing if timestamp changed in the meantime)
     (dosync
      (let [metric-store (ensure a-metric-store)]
        (when-let [new-store (metric-store/maybe-remove-metric metric-store metric labels last-update-time-ms)]
          (ref-set a-metric-store new-store)))))
   ;; Note: this scheme makes pruning slower of course, but the intention is to cause less contention on the metric-store.
   ;; Maybe it would be even better to check if there are just any first; but then remove all in one go.
   ))

(defn ^:no-doc run-prune-stale-metrics-once! [metric-store earlier-time-ms]
  (let [[n-metrics n-labels] (metric-store/get-metrics-and-labels-count @metric-store)]
    (event/log-event! :debug (str "Start pruning stale metrics, from a store of " n-metrics " metrics with " n-labels " labels.")))
  ;; Seen 'Transaction failed after reaching retry limit' in the wild; cannot reproduce how, though.
  (try
    (prune-stale-metrics! metric-store earlier-time-ms)
    (event/log-event! :debug "Done pruning stale metrics.")
    (catch Exception e
      ;; Note: might have pruned a few event if an exception is thrown.
      (event/log-exception-event :warn "Error in pruning stale metrics thread. Will try again after timeout."
                                 e))))

(defn start-prune-stale-metrics-thread!
  "Start a thread that prunes stale metrics older than `stale-seconds` seconds
  every `every-seconds` seconds.  If called without an argument, it prunes
  metrics older than 24h every 5m."
  [& [stale-seconds every-seconds]]
  (let [stale-milliseconds (* (or stale-seconds (* 24 60 60)) 1000)
        every-milliseconds (* (or every-seconds (* 5 60)) 1000)]
    (doto (Thread.
           (fn []
             (loop []
               (let [now (time/get-milli-time!)
                     earlier (- now stale-milliseconds)]
                 (run-prune-stale-metrics-once! metric-store earlier)
                 (Thread/sleep ^long every-milliseconds)
                 (recur)))))
      (.setDaemon true)
      (.start))))

(s/fdef record-and-get!
  :args (s/cat :optional-1 (s/? (s/cat :a-metric-store ::metric-store))
               :metric ::metric-types/metric
               :labels ::metric-types/metric-labels
               :value  ::metric-types/metric-value
               :optional-2 (s/? (s/cat :last-update (s/nilable ::metric-types/metric-last-update-time-ms))))
  :ret  (s/coll-of ::metric-samples/metric-sample))
(defn record-and-get!
  "Records a metric and returns a recent sample, which may or not reflect the metric just recorded."
  ([metric labels value]
   (record-and-get! metric-store metric labels value nil))
  ([metric labels value last-update]
   (record-and-get! metric-store metric labels value last-update))
  ([a-metric-store metric labels value last-update]
   (record-metric! a-metric-store metric labels value last-update)
   (get-metric-samples! a-metric-store metric labels)))
