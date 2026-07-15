(ns ^:no-doc active.clojure.logger.metric-store
  "Internal.

  metric-store-maps stores contains all known metrics and their stored values"
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]
            
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-values :as metric-values]
            
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

;; Note: a private type; callers should not make any assumption about how the values are stored.
(define-record-type ^:private MetricStore
  (make-metric-store map)
  metric-store?
  [map metric-store-map])

(s/def ::metric-store
  (s/spec
   metric-store?))

(s/fdef fresh-metric-store
  :ret ::metric-store)
(defn ^:no-doc fresh-metric-store
  []
  (make-metric-store {}))

;; -----------------------------------------------------------------

(s/fdef record-metric
  :args (s/cat :metric-store ::metric-store
               :metric       ::metric-types/metric
               :labels       ::metric-types/metric-labels
               :value        ::metric-types/metric-value
               :time-ms      ::metric-types/metric-last-update-time-ms)
  :ret ::metric-store)
(defn record-metric
  "Record a new value for the given metric. Must be called inside a transaction."
  [metric-store metric labels value time-ms]
  (lens/overhaul metric-store metric-store-map
                 #(update % metric metric-values/update-or-make-stored-values metric labels value time-ms)))

(defn maybe-record-metric!
  "Record a new value for the given metric and labels, if it is already in the store. Returns falsy otherwise. Must be called inside a transaction."
  [metric-store metric labels value time-ms]
  (when-let [values (get (metric-store-map metric-store) metric)]
    (metric-values/maybe-update-stored-values! values metric labels value time-ms)))

(s/fdef find-stale-metrics
  :args (s/cat :metric-store ::metric-store
               :time-ms       ::metric-types/metric-last-update-time-ms)
  :ret (s/coll-of (s/tuple ::metric-types/metric ::metric-types/metric-labels ::metric-types/metric-last-update-time-ms)))
(defn find-stale-metrics
  "Returns a list of [metric labels last-update-time-ms] tuples, which were not updated since the given time-ms"
  [metric-store time-ms]
  (mapcat (fn [[metric stored-values]]
            (map (fn [[labels time-ms]]
                   [metric labels time-ms])
                 (metric-values/find-stale-labels stored-values time-ms)))
          (metric-store-map metric-store)))

(s/fdef maybe-remove-metric
  :args (s/cat :metric-store ::metric-store
               :metric       ::metric-types/metric
               :labels       ::metric-types/metric-labels
               :check-time-ms ::metric-types/metric-last-update-time-ms)
  :ret (s/or :some ::metric-store :none nil?))
(defn maybe-remove-metric
  "Remove the given metric from the store, if it's last update-time equals the given check-time. Return nil otherwise. Must be called inside a transaction."
  [metric-store metric labels check-time-ms]
  (when-let [stored-values (get (metric-store-map metric-store) metric)]
    (when-let [new-stored-values (metric-values/maybe-remove-values stored-values labels check-time-ms)]
      (cond
        (metric-values/empty-stored-values? new-stored-values)
        (lens/overhaul metric-store
                       metric-store-map
                       dissoc metric)

        :else (lens/overhaul metric-store
                             metric-store-map
                             assoc metric new-stored-values)))))

(defn get-metrics-and-labels-count
  [metric-store]
  (let [m (metric-store-map metric-store)]
    [(count m)
     (reduce + 0 (map metric-values/get-labels-count (vals m)))]))

;; -----------------------------------------------------------------

(s/fdef get-metric-snapshot
  :args (s/cat :metric-store  ::metric-store
               :metric        ::metric-types/metric
               :metric-labels ::metric-types/metric-labels)
  :ret (s/or :some ::metric-values/snapshot :none nil?))
(defn get-metric-snapshot
  [metric-store metric metric-labels]
  (if-let [stored-value (get (metric-store-map metric-store) metric)]
    (metric-values/get-stored-values-snapshot stored-value metric-labels)
    nil))

(s/fdef get-all-metric-snapshots
  :args (s/cat :metric-store ::metric-store
               :metric       ::metric-types/metric)
  :ret (s/map-of ::metric-types/metric-labels ::metric-values/snapshot))
(defn get-all-metric-snapshots
  [metric-store metric]
  (if-let [stored-value (get (metric-store-map metric-store) metric)]
    (metric-values/get-all-stored-values-snapshot stored-value)
    {}))

(s/fdef get-all-snapshots
  :args (s/cat :metric-store ::metric-store)
  :ret (s/map-of ::metric-types/metric (s/map-of ::metric-types/metric-labels ::metric-values/snapshot)))
(defn get-all-snapshots
  [metric-store]
  (into {}
        (map (fn [[metric stored-value]]
               [metric (metric-values/get-all-stored-values-snapshot stored-value)])
             (metric-store-map metric-store))))
