(ns ^:no-doc active.clojure.logger.metric-values
  "Internal.

  Datatypes to hold values of metrics per labels."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]

            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-samples :as metric-samples]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

;; -----------------------------------------------------------------

;; Maps from labels to value hold the current state of the metric

(s/def ::labels-metric-value-map (s/map-of ::metric-types/metric-labels ::metric-value))

(s/def ::labels-histogram-metric-value-map (s/map-of ::metric-types/metric-labels ::histogram-metric-values))

(s/def ::labels-value-map (s/or :labels-metric-value-map ::labels-metric-value-map
                                :labels-histogram-metric-value-map ::labels-histogram-metric-value-map))

(def ^:const empty-values-map {})


(define-record-type ^{:doc "Metric value is a combination of the `value` itself
  and the `last-update-time-ms` of the value."}
  MetricValue
  ^:private really-make-metric-value
  metric-value?
  [value               metric-value-value
   last-update-time-ms metric-value-last-update-time-ms])

(s/def ::metric-value-value (s/and number? #(not (Double/isNaN %))))
(s/def ::metric-value-value-double (s/and double? #(not (Double/isNaN %))))

(s/def ::metric-value-last-update-time-ms nat-int?)

;; https://prometheus.io/docs/instrumenting/writing_exporters/
;; "You should not set timestamps on the metrics you expose, let Prometheus
;; take care of that."

;; Note: metric-value is both the type of new values coming in, and the internal storage type for singular values. (TODO: that's not good)
(s/def ::metric-value
  (s/spec
   (partial instance? MetricValue)
   :gen (fn []
          (sgen/fmap (fn [[metric-value-value-double metric-value-last-update-time-ms]]
                       (really-make-metric-value metric-value-value-double metric-value-last-update-time-ms))
                     (s/gen (s/tuple ::metric-value-value-double ::metric-types/metric-last-update-time-ms))))))

;; stores value as double
(s/fdef make-metric-value
  :args (s/cat :value       ::metric-types/metric-value
               :update-time ::metric-types/metric-last-update-time-ms)
  :ret  ::metric-value)
(defn make-metric-value
  [value update-time]
  (really-make-metric-value (double value) update-time))

;; Primitives on `labels-value-map`s

(s/fdef assoc-labels-value-map
  :args (s/cat :labels-value-map ::labels-value-map
               :metric-labels    ::metric-types/metric-labels
               :metric-value     (s/or :metric-value ::metric-value
                                       :histogram-metric-values ::histogram-metric-values))
  :ret ::labels-value-map)
(defn assoc-labels-value-map
  [labels-value-map metric-labels metric-value]
  (assoc labels-value-map metric-labels metric-value))

(s/fdef set-metric-value
  :args (s/cat :labels-value-map ::labels-metric-value-map
               :metric-labels    ::metric-types/metric-labels
               :metric-value     ::metric-value)
  :ret ::labels-value-map)
(defn set-metric-value
  "Set a metric-value within a labels-value-map."
  [labels-value-map metric-labels metric-value]
  (assoc-labels-value-map labels-value-map metric-labels metric-value))

(s/fdef update-metric-value
  :args (s/cat
         :f              (s/fspec
                          :args (s/cat :metric-value-value-1 ::metric-types/metric-value
                                       :metric-value-value-2 ::metric-types/metric-value)
                          :ret number?)
         :metric-value-1 (s/nilable ::metric-value)
         :metric-value-2 ::metric-value)
  :ret ::metric-value)
(defn update-metric-value
  "Update a `MetricValue` by applying a function `f` to the `value`s of the old
  and the new `MetricValue` and setting the `timestamp` to the new timestamp. If
  the old-metric-value` is `nil` take the new-metric-value."
  [f metric-value-1 metric-value-2]
  (if metric-value-1
    (-> metric-value-1
        (lens/overhaul metric-value-value f (metric-value-value metric-value-2))
        (metric-value-last-update-time-ms (metric-value-last-update-time-ms metric-value-2)))
    metric-value-2))

(s/fdef update-labels-value-map
  :args (s/cat
         :labels-value-map ::labels-value-map
         :metric-labels    ::metric-types/metric-labels
         :f                (s/fspec
                            :args (s/cat :metric-value-1 ::metric-value)
                            :ret ::metric-value))
  :ret ::labels-value-map)
(defn update-labels-value-map
  [labels-value-map metric-labels f]
  (update labels-value-map metric-labels f))

(s/fdef inc-metric-value
  :args (s/cat :labels-value-map ::labels-metric-value-map
               :metric-labels            ::metric-types/metric-labels
               :metric-value             ::metric-value)
  :ret ::labels-metric-value-map)
(defn inc-metric-value
  "Increment a metric-value within a labels-value-map."
  [labels-value-map metric-labels metric-value-2]
  (update-labels-value-map labels-value-map metric-labels
                           (fn [metric-value-1]
                             (update-metric-value + metric-value-1 metric-value-2))))

;; -----------------------------------------------------------------

(s/fdef stale?
  :args (s/cat :last-update-time-ms ::metric-types/metric-last-update-time-ms
               :time-ms ::metric-types/metric-last-update-time-ms)
  :ret boolean?)
(defn stale?
  [last-update-time-ms time-ms]
  (< last-update-time-ms time-ms))

(s/fdef stale-metric-value?
  :args (s/cat :metric-value ::metric-value
               :time-ms      ::metric-types/metric-last-update-time-ms)
  :ret boolean?)
(defn stale-metric-value?
  [metric-value time-ms]
  (stale? (metric-value-last-update-time-ms metric-value) time-ms))

(s/fdef prune-stale-metric-value
  :args (s/cat :labels-value-map ::labels-metric-value-map
               :time-ms          ::metric-types/metric-last-update-time-ms)
  :ret ::labels-metric-value-map)
(defn prune-stale-metric-value
  [labels-value-map time-ms]
  (reduce-kv (fn [new-labels-value-map metric-labels metric-value]
               (if (stale-metric-value? metric-value time-ms)
                 new-labels-value-map
                 (assoc-labels-value-map new-labels-value-map metric-labels metric-value)))
             (empty labels-value-map)
             labels-value-map))

;; -----------------------------------------------------------------

(define-record-type ^{:doc "Stored Singular values, i.e. a map from labels to
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
(defn make-singular-values
  []
  (really-make-singular-values empty-values-map))

(def empty-singular-values (make-singular-values))

(s/fdef inc-singular-values
  :args (s/cat :singular-values ::singular-values
               :metric-labels   ::metric-types/metric-labels
               :metric-value    ::metric-value)
  :ret ::singular-values)
(defn inc-singular-values
  "Updates a `SingularMetric`."
  [singular-values metric-labels metric-value]
  (lens/overhaul singular-values
                 singular-values-map
                 (fn [labels-values-map]
                   (inc-metric-value labels-values-map
                                     metric-labels
                                     metric-value))))

(s/fdef set-singular-values
  :args (s/cat :singular-values ::singular-values
               :metric-labels   ::metric-types/metric-labels
               :metric-value    ::metric-value)
  :ret ::singular-values)
(defn set-singular-values
  "Updates a `SingularMetric`."
  [singular-values metric-labels metric-value]
  (lens/overhaul singular-values
                 singular-values-map
                 (fn [labels-values-map]
                   (set-metric-value labels-values-map
                                     metric-labels
                                     metric-value))))

(s/fdef singular-values->metric-samples
  :args (s/cat :name           ::metric-types/metric-name
               :singular-values ::singular-values
               :metric-labels  ::metric-types/metric-labels)
  :ret (s/coll-of ::metric-samples/metric-sample))
(defn singular-values->metric-samples
  [name singular-values metric-labels]
  (if-let [metric-value (get (singular-values-map singular-values) metric-labels)]
    [(metric-samples/make-metric-sample name
                                        metric-labels
                                        (metric-value-value               metric-value)
                                        (metric-value-last-update-time-ms metric-value))]
    []))

(s/fdef prune-stale-singular-values
  :args (s/cat :singular-values ::singular-values
               :time-ms         ::metric-types/metric-last-update-time-ms)
  :ret ::singular-values)
(defn prune-stale-singular-values
  [singular-values time-ms]
  (lens/overhaul singular-values singular-values-map prune-stale-metric-value time-ms))

;; TODO: if the api is exposing the inner map anyway, then this does not need to exist.
(s/fdef empty-singular-values?
  :args (s/cat :singular-values ::singular-values)
  :ret boolean?)
(defn empty-singular-values?
  [singular-values]
  (empty? (singular-values-map singular-values)))

;; -----------------------------------------------------------------

(define-record-type ^{:doc "Stored Histogram metric values."}
  HistogramMetricValues
  really-make-histogram-metric-values
  histogram-metric-values?
  [last-update-time-ms histogram-metric-values-last-update-time-ms
   sum-value histogram-metric-values-sum-value
   count-value histogram-metric-values-count-value
   bucket-values histogram-metric-values-bucket-values])

(s/def ::bucket-values (s/coll-of ::metric-types/metric-value))

(s/def ::histogram-metric-values
  (s/spec
   (partial instance? HistogramMetricValues)
   :gen (fn []
          (sgen/fmap (fn [[histogram-metric-values-last-update-time-ms
                           histogram-metric-values-sum-value
                           histogram-metric-values-count-value
                           histogram-metric-values-bucket-values]]
                       (really-make-histogram-metric-values histogram-metric-values-last-update-time-ms
                                                            histogram-metric-values-sum-value
                                                            histogram-metric-values-count-value
                                                            histogram-metric-values-bucket-values))
                     (s/gen (s/tuple ::metric-types/metric-last-update-time-ms ::metric-types/metric-value ::metric-types/metric-value
                                     ::bucket-values))))))

(s/fdef make-histogram-metric-values
  :args (s/cat :last-update-time-ms ::metric-types/metric-last-update-time-ms
               :sum-value ::metric-types/metric-value
               :count-value ::metric-types/metric-value ;; TODO: should ne pos-int?
               :bucket-values ::bucket-values)
  :ret ::histogram-metric-values)
(defn make-histogram-metric-values
  [last-update-time-ms sum-value count-value bucket-values]
  (really-make-histogram-metric-values last-update-time-ms sum-value count-value bucket-values))

(s/fdef make-empty-histogram-metric-values
  :args (s/cat :thresholds ::metric-types/thresholds)
  :ret ::histogram-metric-values)
(defn make-empty-histogram-metric-values
  [thresholds]
  (make-histogram-metric-values 0 0.0 0.0 (vec (repeat (count thresholds) 0.0))))

(s/fdef update-histogram-values
  :args (s/cat :histogram-metric-values ::histogram-metric-values
               :thresholds ::metric-types/thresholds
               :metric-value     ::metric-value)
  :ret ::histogram-metric-values)
(defn update-histogram-metric-values
  [histogram-metric-values thresholds metric-value]
  (let [value-value (metric-value-value metric-value)
        last-update-time-ms (metric-value-last-update-time-ms metric-value)]
    ;; OPT: construct a new record only once.
    (-> (or histogram-metric-values (make-empty-histogram-metric-values thresholds))
        (histogram-metric-values-last-update-time-ms last-update-time-ms)
        (lens/overhaul histogram-metric-values-sum-value + value-value)
        (lens/overhaul histogram-metric-values-count-value + 1.0) ;; counts are doubles?
        (lens/overhaul histogram-metric-values-bucket-values
                       #(mapv (fn [threshold bucket-value]
                                (if (<= value-value threshold)
                                  (+ bucket-value 1.0)
                                  bucket-value))
                              thresholds %)))))

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
                     (s/gen (s/tuple ::metric-types/thresholds ::histogram-metric-values))))))

(s/fdef make-histogram-values
  :args (s/cat :thresholds ::metric-types/thresholds)
  :ret ::histogram-values)
(defn make-histogram-values
  [thresholds]
  (really-make-histogram-values thresholds empty-values-map))

(s/fdef really-update-histogram-values-map
  :args (s/cat :histogram-values-map ::labels-histogram-metric-value-map
               :metric-labels ::metric-types/metric-labels
               :thresholds ::metric-types/thresholds
               :metric-value     ::metric-value)
  :ret ::histogram-metric-values)
(defn really-update-histogram-values-map
  [histogram-values-map metric-labels thresholds metric-value]
  (update histogram-values-map metric-labels update-histogram-metric-values
          thresholds metric-value))

(s/fdef update-histogram-values-map
  :args (s/cat :histogram-values-map ::labels-histogram-metric-value-map
               :thresholds ::metric-types/thresholds
               :metric-labels    ::metric-types/metric-labels
               :metric-value     ::metric-value)
  :ret ::labels-histogram-metric-value-map)
(defn update-histogram-values-map
  [histogram-values-map thresholds metric-labels metric-value]
  (really-update-histogram-values-map histogram-values-map metric-labels
                                      thresholds metric-value))

(s/fdef update-histogram-values
  :args (s/cat :histogram-values ::histogram-values
               :metric-labels    ::metric-types/metric-labels
               :metric-value     ::metric-value)
  :ret ::histogram-values)
(defn update-histogram-values
  "Updates a `HistogramMetric`."
  [histogram-values metric-labels metric-value]
  (let [thresholds          (histogram-values-thresholds histogram-values)]
    (-> histogram-values
        (lens/overhaul histogram-values-map
                       update-histogram-values-map thresholds metric-labels metric-value))))

(s/fdef histogram-values->metric-samples
  :args (s/cat :basename ::metric-types/metric-name
               :histogram-values ::histogram-values
               :metric-labels ::metric-types/metric-labels)
  :ret (s/coll-of ::metric-samples/metric-sample))
(defn histogram-values->metric-samples
  "Return all metric-samples with the given labels within this histogram-metric."
  [basename histogram-values metric-labels]
  (let [thresholds  (histogram-values-thresholds histogram-values)
        histogram-metric-values (get (histogram-values-map histogram-values) metric-labels)]
    (if histogram-metric-values
      (let [last-update-time-ms (histogram-metric-values-last-update-time-ms histogram-metric-values)
            sum (histogram-metric-values-sum-value histogram-metric-values)
            count (histogram-metric-values-count-value histogram-metric-values)
            buckets (histogram-metric-values-bucket-values histogram-metric-values)]
        (concat
         [(metric-samples/make-metric-sample (str basename "_sum")
                                             metric-labels
                                             sum
                                             last-update-time-ms)
          (metric-samples/make-metric-sample (str basename "_count")
                                             metric-labels
                                             count
                                             last-update-time-ms)
          (metric-samples/make-metric-sample (str basename "_bucket")
                                             (assoc metric-labels :le "+Inf")
                                             count
                                             last-update-time-ms)]
         (mapcat (fn [threshold bucket]
                   [(metric-samples/make-metric-sample (str basename "_bucket")
                                                       (assoc metric-labels :le (str threshold))
                                                       bucket
                                                       last-update-time-ms)])
                 thresholds buckets)))
      [])))

(s/fdef prune-stale-histogram-metric-value
  :args (s/cat :histogram-values-map ::labels-histogram-metric-value-map
               :time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::labels-histogram-metric-value-map)
(defn prune-stale-histogram-metric-value
  [histogram-values-map time-ms]
  (reduce-kv (fn [new-labels-value-map metric-labels histogram-metric-values]
               (if (stale? (histogram-metric-values-last-update-time-ms histogram-metric-values) time-ms)
                 new-labels-value-map
                 (assoc-labels-value-map new-labels-value-map metric-labels histogram-metric-values)))
             (empty histogram-values-map)
             histogram-values-map))

(s/fdef prune-stale-histogram-values
  :args (s/cat :histogram-values ::histogram-values
               :time-ms          ::metric-types/metric-last-update-time-ms)
  :ret ::histogram-values)
(defn prune-stale-histogram-values
  [histogram-values time-ms]
  (-> histogram-values
      (lens/overhaul histogram-values-map prune-stale-histogram-metric-value time-ms)))

(s/fdef empty-histogram-value?
  :args (s/cat :histogram-values ::histogram-values)
  :ret boolean?)
(defn empty-histogram-values?
  [histogram-values]
  (empty? (histogram-values-map histogram-values)))

;; -----------------------------------------------------------------

(s/def ::stored-values (s/or :singular  ::singular-values
                             :histogram ::histogram-values))

(s/fdef update-stored-values
  :args (s/cat :stored-values ::stored-values
               :metric        ::metric-types/metric
               :metric-labels ::metric-types/metric-labels
               :metric-value  ::metric-value)
  :ret ::stored-values)
(defn- update-stored-values
  [stored-values metric metric-labels metric-value]
  (cond
    (metric-types/gauge-metric?     metric) (set-singular-values stored-values metric-labels metric-value)
    (metric-types/counter-metric?   metric) (if (metric-types/counter-metric-set-value? metric)
                                              (set-singular-values stored-values metric-labels metric-value)
                                              (inc-singular-values stored-values metric-labels metric-value))
    (metric-types/histogram-metric? metric) (update-histogram-values stored-values metric-labels metric-value)))

(s/fdef make-stored-values
  :args (s/cat :metric        ::metric-types/metric
               :metric-labels ::metric-types/metric-labels
               :metric-value  ::metric-value)
  :ret ::stored-values)
(defn make-stored-values
  [metric metric-labels metric-value]
  (update-stored-values
   (cond
     (metric-types/gauge-metric?     metric) empty-singular-values
     (metric-types/counter-metric?   metric) empty-singular-values
     (metric-types/histogram-metric? metric) (make-histogram-values (metric-types/histogram-metric-thresholds metric)))
   metric metric-labels metric-value))

(s/fdef update-or-make-stored-values
  :args (s/cat :maybe-stored-values (s/nilable ::stored-values)
               :metric ::metric-types/metric
               :metric-labels ::metric-types/metric-labels
               :metric-value ::metric-value)
  :ret ::stored-values)
(defn update-or-make-stored-values
  [maybe-stored-values metric metric-labels metric-value]
  ;; Note: updated thresholds in a histogram metric are ignored.
  (if (and (some? maybe-stored-values)
           (cond
             (metric-types/gauge-metric?     metric) (singular-values? maybe-stored-values)
             (metric-types/counter-metric?   metric) (singular-values? maybe-stored-values)
             (metric-types/histogram-metric? metric) (histogram-values? maybe-stored-values)))
    (update-stored-values maybe-stored-values metric metric-labels metric-value)
    (make-stored-values metric metric-labels metric-value)))

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

;; -----------------------------------------------------------------

(s/fdef stored-value->metric-samples
  :args (s/cat :metric        ::metric-types/metric
               :stored-value  ::stored-values
               :metric-labels ::metric-types/metric-labels)
  :ret (s/coll-of ::metric-samples/metric-sample))
(defn stored-value->metric-samples
  [metric stored-value metric-labels]
  (cond
    (singular-values? stored-value)
    (singular-values->metric-samples (metric-types/metric-name metric) stored-value metric-labels)
    (histogram-values? stored-value)
    (histogram-values->metric-samples (metric-types/metric-name metric) stored-value metric-labels)))

(s/fdef stored-value->all-metric-samples
  :args (s/cat :metric       ::metric-types/metric
               :stored-value ::stored-values)
  :ret (s/coll-of ::metric-samples/metric-sample))
(defn stored-value->all-metric-samples
  [metric stored-value]
  (cond
    (singular-values? stored-value)
    (mapcat (fn [metric-labels] (singular-values->metric-samples (metric-types/metric-name metric) stored-value metric-labels))
            (keys (singular-values-map stored-value)))
    (histogram-values? stored-value)
    (mapcat (fn [metric-labels] (histogram-values->metric-samples (metric-types/metric-name metric) stored-value metric-labels))
            (keys (histogram-values-map stored-value)))))

