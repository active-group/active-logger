(ns ^:no-doc active.clojure.logger.metric-values
  "Internal.

  Functions to store and access data suitable for a metric, per labels."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]

            [active.clojure.logger.metric-singular-value :as singular]
            [active.clojure.logger.metric-histogram-value :as histogram]

            [active.clojure.logger.metric-types :as metric-types]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

;; -----------------------------------------------------------------

;; Maps from labels to value hold the current state of the metric

(s/def ::labels-metric-value-map (s/map-of ::metric-types/metric-labels ::singular/metric-value))

(s/def ::labels-histogram-metric-value-map (s/map-of ::metric-types/metric-labels ::histogram/histogram-metric-values))

(s/def ::labels-value-map (s/or :labels-metric-value-map ::labels-metric-value-map
                                :labels-histogram-metric-value-map ::labels-histogram-metric-value-map))

(def ^{:const true :private true} empty-values-map {})


(s/def ::metric-value-value (s/and number? #(not (Double/isNaN %))))
(s/def ::metric-value-value-double (s/and double? #(not (Double/isNaN %))))

(s/def ::metric-value-last-update-time-ms nat-int?)



;; -----------------------------------------------------------------

(s/fdef stale?
  :args (s/cat :last-update-time-ms ::metric-types/metric-last-update-time-ms
               :time-ms ::metric-types/metric-last-update-time-ms)
  :ret boolean?)
(defn- stale?
  [last-update-time-ms time-ms]
  (< last-update-time-ms time-ms))

(s/fdef stale-metric-value?
  :args (s/cat :metric-value ::singular/metric-value
               :time-ms      ::metric-types/metric-last-update-time-ms)
  :ret boolean?)
(defn- stale-metric-value?
  [metric-value time-ms]
  (stale? (singular/metric-value-last-update-time-ms metric-value) time-ms))

(s/fdef prune-stale-metric-value
  :args (s/cat :labels-value-map ::labels-metric-value-map
               :time-ms          ::metric-types/metric-last-update-time-ms)
  :ret ::labels-metric-value-map)
(defn- prune-stale-metric-value
  [labels-value-map time-ms]
  (reduce-kv (fn [new-labels-value-map metric-labels metric-value]
               (if (stale-metric-value? metric-value time-ms)
                 new-labels-value-map
                 (assoc new-labels-value-map metric-labels metric-value)))
             (empty labels-value-map)
             labels-value-map))

;; -----------------------------------------------------------------

(define-record-type ^{:private true :doc "Stored Singular values, i.e. a map from labels to
  metric-values."}
  SingularValues
  ^:private really-make-singular-values
  singular-values?
  [map singular-values-map])

(s/def ::singular-values
  (s/spec
   (partial instance? SingularValues)
   :gen (fn []
          (sgen/fmap really-make-singular-values
                     (s/gen ::labels-metric-value-map)))))

(s/fdef make-singular-values
  :args (s/cat)
  :ret ::singular-values)
(defn- make-singular-values
  []
  (really-make-singular-values empty-values-map))

(def ^:private empty-singular-values (make-singular-values))

(s/fdef inc-singular-values
  :args (s/cat :singular-values ::singular-values
               :metric-labels   ::metric-types/metric-labels
               :metric-value    ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::singular-values)
(defn- inc-singular-values
  "Updates a `SingularMetric`."
  [singular-values metric-labels metric-value last-update-time-ms]
  (lens/overhaul singular-values
                 singular-values-map
                 (fn [labels-values-map]
                   (update labels-values-map
                           metric-labels
                           singular/inc-metric-value
                           metric-value last-update-time-ms))))

(s/fdef set-singular-values
  :args (s/cat :singular-values ::singular-values
               :metric-labels   ::metric-types/metric-labels
               :metric-value    ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::singular-values)
(defn- set-singular-values
  "Updates a `SingularMetric`."
  [singular-values metric-labels metric-value last-update-time-ms]
  (lens/overhaul singular-values
                 singular-values-map
                 (fn [labels-values-map]
                   (assoc labels-values-map
                          metric-labels
                          (singular/make-metric-value metric-value last-update-time-ms)))))

(s/fdef prune-stale-singular-values
  :args (s/cat :singular-values ::singular-values
               :time-ms         ::metric-types/metric-last-update-time-ms)
  :ret ::singular-values)
(defn- prune-stale-singular-values
  [singular-values time-ms]
  (lens/overhaul singular-values singular-values-map prune-stale-metric-value time-ms))

(s/fdef empty-singular-values?
  :args (s/cat :singular-values ::singular-values)
  :ret boolean?)
(defn- empty-singular-values?
  [singular-values]
  (empty? (singular-values-map singular-values)))

;; -----------------------------------------------------------------

(define-record-type ^{:doc "Stored Histogram values, i.e. a threshold and a map from labels to HistogramMetricValues."}
  HistogramValues
  ^:private really-make-histogram-values
  histogram-values?
  [thresholds histogram-values-thresholds
   map histogram-values-map])

(s/def ::histogram-values
  (s/spec
   (partial instance? HistogramValues)
   :gen (fn []
          (sgen/fmap (fn [[histogram-values-thresholds histogram-values-map]]
                       (really-make-histogram-values histogram-values-thresholds histogram-values-map))
                     (s/gen (s/tuple ::metric-types/thresholds ::labels-histogram-metric-value-map))))))

(s/fdef make-histogram-values
  :args (s/cat :thresholds ::metric-types/thresholds)
  :ret ::histogram-values)
(defn- make-histogram-values
  [thresholds]
  (really-make-histogram-values thresholds empty-values-map))

(s/fdef update-histogram-values-map
  :args (s/cat :histogram-values-map ::labels-histogram-metric-value-map
               :thresholds ::metric-types/thresholds
               :metric-labels    ::metric-types/metric-labels
               :metric-value     ::metric-types/metric-value
               :time-ms          ::metric-types/metric-last-update-time-ms)
  :ret ::labels-histogram-metric-value-map)
(defn- update-histogram-values-map
  [histogram-values-map thresholds metric-labels metric-value time-ms]
  (update histogram-values-map metric-labels
          histogram/update-histogram-metric-values thresholds metric-value time-ms))

(s/fdef update-histogram-values
  :args (s/cat :histogram-values ::histogram-values
               :metric-labels    ::metric-types/metric-labels
               :metric-value     ::metric-types/metric-value
               :time-ms          ::metric-types/metric-last-update-time-ms)
  :ret ::histogram-values)
(defn- update-histogram-values
  "Updates a `HistogramMetric`."
  [histogram-values metric-labels metric-value time-ms]
  (let [thresholds          (histogram-values-thresholds histogram-values)]
    (-> histogram-values
        (lens/overhaul histogram-values-map
                       update-histogram-values-map thresholds metric-labels metric-value time-ms))))

(s/fdef prune-stale-histogram-metric-value
  :args (s/cat :histogram-values-map ::labels-histogram-metric-value-map
               :time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::labels-histogram-metric-value-map)
(defn- prune-stale-histogram-metric-value
  [histogram-values-map time-ms]
  (reduce-kv (fn [new-labels-value-map metric-labels histogram-metric-values]
               (if (stale? (histogram/histogram-metric-values-last-update-time-ms histogram-metric-values) time-ms)
                 new-labels-value-map
                 (assoc new-labels-value-map metric-labels histogram-metric-values)))
             (empty histogram-values-map)
             histogram-values-map))

(s/fdef prune-stale-histogram-values
  :args (s/cat :histogram-values ::histogram-values
               :time-ms          ::metric-types/metric-last-update-time-ms)
  :ret ::histogram-values)
(defn- prune-stale-histogram-values
  [histogram-values time-ms]
  (-> histogram-values
      (lens/overhaul histogram-values-map prune-stale-histogram-metric-value time-ms)))

(s/fdef empty-histogram-value?
  :args (s/cat :histogram-values ::histogram-values)
  :ret boolean?)
(defn- empty-histogram-values?
  [histogram-values]
  (empty? (histogram-values-map histogram-values)))

;; -----------------------------------------------------------------

(s/def ::stored-values (s/or :singular  ::singular-values
                             :histogram ::histogram-values))

(s/fdef update-stored-values
  :args (s/cat :stored-values ::stored-values
               :metric        ::metric-types/metric
               :labels ::metric-types/metric-labels
               :value  ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn- update-stored-values
  [stored-values metric labels value last-update-time-ms]
  (cond
    (metric-types/gauge-metric?     metric) (set-singular-values stored-values labels value last-update-time-ms)
    (metric-types/counter-metric?   metric) (if (metric-types/counter-metric-set-value? metric)
                                              (set-singular-values stored-values labels value last-update-time-ms)
                                              (inc-singular-values stored-values labels value last-update-time-ms))
    (metric-types/histogram-metric? metric) (update-histogram-values stored-values labels value last-update-time-ms)))

(s/fdef make-empty-stored-values
  :args (s/cat :metric        ::metric-types/metric)
  :ret ::stored-values)
(defn make-empty-stored-values
  [metric]
  (cond
    (metric-types/gauge-metric?     metric) empty-singular-values
    (metric-types/counter-metric?   metric) empty-singular-values
    (metric-types/histogram-metric? metric) (make-histogram-values (metric-types/histogram-metric-thresholds metric))))

(s/fdef update-or-make-stored-values
  :args (s/cat :maybe-stored-values (s/nilable ::stored-values)
               :metric ::metric-types/metric
               :labels ::metric-types/metric-labels
               :value ::metric-types/metric-value
               :update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn update-or-make-stored-values
  [maybe-stored-values metric labels value last-update-time-ms]
  ;; Note: updated thresholds in a histogram metric are ignored.
  (update-stored-values (if (and (some? maybe-stored-values)
                                 (cond
                                   (metric-types/gauge-metric?     metric) (singular-values? maybe-stored-values)
                                   (metric-types/counter-metric?   metric) (singular-values? maybe-stored-values)
                                   (metric-types/histogram-metric? metric) (histogram-values? maybe-stored-values)))
                          maybe-stored-values
                          (make-empty-stored-values metric))
                        metric labels value last-update-time-ms))

(s/fdef prune-stale-stored-values
  :args (s/cat :stored-values ::stored-values
               :time-ms       ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn prune-stale-stored-values
  [stored-values time-ms]
  (cond
    (singular-values? stored-values)
    (prune-stale-singular-values stored-values time-ms)

    (histogram-values? stored-values)
    (prune-stale-histogram-values stored-values time-ms)))

(s/fdef empty-stored-values?
  :args (s/cat :stored-values ::stored-values)
  :ret boolean?)
(defn empty-stored-values?
  [stored-values]
  (cond
    (singular-values? stored-values)
    (empty-singular-values? stored-values)

    (histogram-values? stored-values)
    (empty-histogram-values? stored-values)))

(s/def ::snapshot (s/or :singular  ::singular/metric-value
                        :histogram ::histogram/histogram-metric-values))

(s/fdef get-stored-values-snapshot
  :args (s/cat :stored-values ::stored-values
               :labels ::metric-types/metric-labels)
  :ret (s/or :some ::snapshot :none nil?))
(defn get-stored-values-snapshot
  "Returns a singular or histogram value for the given labels, or nil.

  For tests only."
  [stored-values labels]
  (cond
    (singular-values? stored-values)
    (get (singular-values-map stored-values) labels)

    (histogram-values? stored-values)
    (get (histogram-values-map stored-values) labels)))

(s/fdef get-all-stored-values-snapshot
  :args (s/cat :stored-values ::stored-values)
  :ret (s/map-of ::metric-types/metric-labels ::snapshot))
(defn get-all-stored-values-snapshot
  "Returns a map from labels to snapshots of the values (singular or histogram).

  For tests only."
  [stored-values]
  (cond
    (singular-values? stored-values)
    (singular-values-map stored-values)

    (histogram-values? stored-values)
    (histogram-values-map stored-values)))
