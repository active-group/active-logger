(ns active.clojure.logger.metric-accumulator
  "Metrics."
  (:require [active.clojure.logger.time :as time]

            [active.clojure.logger.metric-store :as metric-store]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-types :as metric-types]

            [clojure.spec.alpha :as s]))

(s/def ::metric-store (partial instance? clojure.lang.Atom))

(s/fdef fresh-metric-store
  :ret ::metric-store)
(defn ^:no-doc fresh-metric-store
  []
  (atom (metric-store/fresh-metric-store)))

(defonce metric-store (fresh-metric-store))

(s/fdef reset-global-metric-store!
  :args (s/cat)
  :ret nil)
(defn reset-global-metric-store!
  []
  (reset! metric-store (metric-store/fresh-metric-store))
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
   (swap! a-metric-store metric-store/record-metric metric labels value-value (or last-update (time/get-milli-time!)))
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

(s/fdef get-all-metric-sample-sets!
  :args (s/cat :optional (s/? (s/cat :a-metric-store ::metric-store)))
  :ret (s/coll-of (s/coll-of ::metric-samples/metric-sample-set)))
(defn get-all-metric-sample-sets!
  "Return all metric-samples-sets within the given metric-store.
  If no metric-store is given, use the global metric store."
  ([]
   (get-all-metric-sample-sets! metric-store))
  ([a-metric-store]
   (map (fn [[metric snapshots]]
          (metric-samples/make-metric-sample-set
           (metric-types/metric-name metric)
           (metric-types/metric-type metric)
           (metric-types/metric-help metric)
           (metric-samples/all-snapshots->all-metric-samples metric snapshots)))
        (metric-store/get-all-snapshots @a-metric-store))))

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
   (swap! a-metric-store
          (fn [old-metric-store]
            (metric-store/prune-stale-metrics old-metric-store time-ms)))
   nil))

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
                 (prune-stale-metrics! earlier)
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
