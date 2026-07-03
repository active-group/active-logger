(ns ^:no-doc active.clojure.logger.metric-store
  "Internal.

  metric-store-maps stores contains all known metrics and their stored values"
  (:require [active.clojure.record :refer [define-record-type]]
            
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-values :as metric-values]
            [active.clojure.logger.metric-samples :as metric-samples]
            
            [clojure.spec.alpha :as s]))

(s/def ::metric-store-map (s/map-of ::metric-types/metric ::metric-values/stored-values))

(s/fdef fresh-metric-store-map
  :ret ::metric-store-map)
(defn ^:no-doc fresh-metric-store-map
  []
  {})

(s/fdef assoc-metric-store
  :args (s/cat :metric-store  ::metric-store-map
               :metric        ::metric-types/metric
               :stored-values ::metric-values/stored-values)
  :ret ::metric-store-map)
(defn assoc-metric-store
  [metric-store metric stored-values]
  (assoc metric-store metric stored-values))

(s/fdef update-metric-store
  :args (s/cat :metric-store ::metric-store-map
               :metric       ::metric-types/metric
               :f            fn?
               :args         (s/* any?))
  :ret ::metric-store-map)
(defn update-metric-store
  [metric-store metric f & args]
  (apply update metric-store metric f args))


;; -----------------------------------------------------------------

(defn- record-metric-value
  [metric-store metric metric-labels metric-value]
  (update-metric-store metric-store metric metric-values/update-or-make-stored-values metric metric-labels metric-value))

(s/fdef record-metric
  :args (s/cat :metric-store ::metric-store-map
               :metric       ::metric-types/metric
               :labels       ::metric-types/metric-labels
               :value        ::metric-types/metric-value
               :time-ms      ::metric-types/metric-last-update-time-ms)
  :ret ::metric-store-map)
(defn record-metric
  [metric-store metric labels value time-ms]
  (record-metric-value metric-store metric labels (metric-values/make-metric-value value time-ms)))

(s/fdef prune-stale-metrics
  :args (s/cat :metric-store ::metric-store-map
               :time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::metric-store-map)
(defn prune-stale-metrics [metric-store time-ms]
  (reduce-kv (fn [new-metric-store metric stored-values]
               (let [new-stored-values (metric-values/prune-stale-stored-values stored-values time-ms)]
                 (if (metric-values/empty-stored-values? new-stored-values)
                   new-metric-store
                   (assoc-metric-store new-metric-store metric new-stored-values))))
             (empty metric-store)
             metric-store))

;; -----------------------------------------------------------------

(s/fdef get-metric-samples
  :args (s/cat :metric-store  ::metric-store-map
               :metric        ::metric-types/metric
               :metric-labels ::metric-types/metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn get-metric-samples
  [metric-store metric metric-labels]
  (if-let [stored-value (get metric-store metric)]
    (metric-values/stored-value->metric-samples metric stored-value metric-labels)
    []))

(s/fdef get-metric-sample-set
  :args (s/cat :metric-store ::metric-store-map
               :metric       ::metric-types/metric)
  :ret ::metric-samples/metric-sample-set)
(defn get-metric-sample-set
  [metric-store metric]
  (if-let [stored-value (get metric-store metric)]
    (metric-samples/make-metric-sample-set (metric-types/metric-name metric)
                                           (metric-types/metric-type metric)
                                           (metric-types/metric-help metric)
                                           (metric-values/stored-value->all-metric-samples metric stored-value))
    []))

(s/fdef get-all-metric-sample-sets
  :args (s/cat :metric-store ::metric-store-map)
  :ret (s/coll-of (s/coll-of ::metric-samples/metric-sample-set)))
(defn get-all-metric-sample-sets
  "Return all metric-samples-sets within the given metric-store."
  [metric-store]
  (mapv (fn [metric] (get-metric-sample-set metric-store metric))
        (keys metric-store)))

