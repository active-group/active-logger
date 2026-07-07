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
   metric-store?
   #_#_:gen (fn []
          (sgen/fmap make-metric-store
                     ;; FIXME: metric and stored-values should match; it's not arbitrary.
                     (s/gen (s/map-of ::metric-types/metric ::metric-values/stored-values))))))

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

(s/fdef prune-stale-metrics
  :args (s/cat :metric-store ::metric-store
               :time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::metric-store)
(defn prune-stale-metrics [metric-store time-ms]
  (lens/overhaul metric-store
                 metric-store-map
                 (fn [metric-store]
                   (reduce-kv (fn [new-metric-store metric stored-values]
                                (let [new-stored-values (metric-values/prune-stale-stored-values stored-values time-ms)]
                                  (if (metric-values/empty-stored-values? new-stored-values)
                                    new-metric-store
                                    (assoc new-metric-store metric new-stored-values))))
                              (empty metric-store)
                              metric-store))))

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
