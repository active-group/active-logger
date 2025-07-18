(ns active.clojure.logger.metric-accumulator
  "Metrics."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]
            [active.clojure.monad :as monad]

            [active.clojure.logger.time :as time]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(s/def ::metric (s/or :gauge-metric     ::gauge-metric
                      :counter-metric   ::counter-metric
                      :histogram-metric ::histogram-metric))

(s/def ::stored-values (s/or :gauge     ::gauge-values
                             :counter   ::counter-values
                             :histogram ::histogram-values))

;; ::metric-store-maps stores contains all known metrics and their stored values

(s/def ::metric-store-map (s/map-of ::metric ::stored-values))

(s/fdef fresh-metric-store-map
  :ret ::metric-store-map)
(defn ^:no-doc fresh-metric-store-map
  []
  {})

(s/fdef assoc-metric-store
  :args (s/cat :metric-store  ::metric-store-map
               :metric        ::metric
               :stored-values ::stored-values)
  :ret ::metric-store-map)
(defn assoc-metric-store
  [metric-store metric stored-values]
  (assoc metric-store metric stored-values))

(s/fdef update-metric-store
  :args (s/cat :metric-store ::metric-store-map
               :metric       ::metric
               :f            fn?
               :args         (s/* any?))
  :ret ::metric-store-map)
(defn update-metric-store
  [metric-store metric f & args]
  (apply update metric-store metric f args))

;; TODO: Can we improve the type of the metric-store?
(s/def ::metric-store (partial instance? clojure.lang.Atom))

(s/fdef fresh-metric-store
  :ret ::metric-store)
(defn ^:no-doc fresh-metric-store
  []
  (atom (fresh-metric-store-map)))

(defonce metric-store (fresh-metric-store))

(s/fdef set-global-metric-store!
  :args (s/cat :fresh-metric-store-map ::metric-store-map)
  :ret nil)
(defn set-global-metric-store!
  [fresh-metric-store-map]
  (reset! metric-store fresh-metric-store-map)
  nil)

(s/fdef reset-global-metric-store!
  :args (s/cat)
  :ret nil)
(defn reset-global-metric-store!
  []
  (reset! metric-store (fresh-metric-store-map))
  nil)

;; -----------------------------------------------------------------

;; Maps from labels to value hold the current state of the metric
;; These maps are stored in ::stored-values

(s/def ::labels-metric-value-map (s/map-of ::metric-labels ::metric-value))

(s/def ::labels-histogram-metric-value-map (s/map-of ::metric-labels ::histogram-metric-values))

(s/def ::labels-value-map (s/or :labels-metric-value-map ::labels-metric-value-map
                                :labels-histogram-metric-value-map ::labels-histogram-metric-value-map))

(def ^:const empty-values-map {})

(s/def ::metric-labels (s/map-of keyword? any?))

(s/def ::metric-name string?)
(s/def ::metric-help string?)

(define-record-type ^{:doc "Metric value is a combination of the `value` itself
  and the `last-update-time-ms` of the value."}
  MetricValue
  ^:private really-make-metric-value
  metric-value?
  [value               metric-value-value
   last-update-time-ms metric-value-last-update-time-ms])

;; TODO: maybe better counter-metric-value and gauge-metric-value?
(s/def ::metric-value-value (s/and number? #(not (Double/isNaN %))))
(s/def ::metric-value-value-double (s/and double? #(not (Double/isNaN %))))
;; https://prometheus.io/docs/instrumenting/writing_exporters/
;; "You should not set timestamps on the metrics you expose, let Prometheus
;; take care of that."
(s/def ::metric-value-last-update-time-ms nat-int?)

(s/def ::metric-value
  (s/spec
   (partial instance? MetricValue)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-value-value-double metric-value-last-update-time-ms]}]
                       (really-make-metric-value metric-value-value-double metric-value-last-update-time-ms))
                     (s/gen (s/keys :req-un [::metric-value-value-double ::metric-value-last-update-time-ms]))))))

;; stores value as double
(s/fdef make-metric-value
  :args (s/cat :value       ::metric-value-value
               :update-time ::metric-value-last-update-time-ms)
  :ret  ::metric-value)
(defn make-metric-value
  [value update-time]
  (really-make-metric-value (double value) update-time))

;; Primitives on `labels-value-map`s

(s/fdef assoc-labels-value-map
  :args (s/cat :labels-value-map ::labels-value-map
               :metric-labels    ::metric-labels
               :metric-value     (s/or :metric-value ::metric-value
                                       :histogram-metric-values ::histogram-metric-values))
  :ret ::labels-value-map)
(defn assoc-labels-value-map
  [labels-value-map metric-labels metric-value]
  (assoc labels-value-map metric-labels metric-value))

(s/fdef set-metric-value
  :args (s/cat :labels-value-map ::labels-metric-value-map
               :metric-labels    ::metric-labels
               :metric-value     ::metric-value)
  :ret ::labels-value-map)
(defn set-metric-value
  "Set a metric-value within a labels-value-map."
  [labels-value-map metric-labels metric-value]
  (assoc-labels-value-map labels-value-map metric-labels metric-value))

(s/fdef update-metric-value
  :args (s/cat
         :f              (s/fspec
                          :args (s/cat :metric-value-value-1 ::metric-value-value
                                       :metric-value-value-2 ::metric-value-value)
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
         :metric-labels    ::metric-labels
         :f                (s/fspec
                            :args (s/cat :metric-value-1 ::metric-value)
                            :ret ::metric-value))
  :ret ::labels-value-map)
(defn update-labels-value-map
  [labels-value-map metric-labels f]
  (update labels-value-map metric-labels f))

(s/fdef inc-metric-value
  :args (s/cat :labels-value-map ::labels-metric-value-map
               :metric-labels            ::metric-labels
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
  :args (s/cat :last-update-time-ms ::metric-value-last-update-time-ms
               :time-ms ::metric-value-last-update-time-ms)
  :ret boolean?)
(defn stale?
  [last-update-time-ms time-ms]
  (< last-update-time-ms time-ms))

(s/fdef stale-metric-value?
  :args (s/cat :metric-value ::metric-value
               :time-ms      ::metric-value-last-update-time-ms)
  :ret boolean?)
(defn stale-metric-value?
  [metric-value time-ms]
  (stale? (metric-value-last-update-time-ms metric-value) time-ms))

(s/fdef prune-stale-metric-value
  :args (s/cat :labels-value-map ::labels-metric-value-map
               :time-ms          ::metric-value-last-update-time-ms)
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

;; Metrics samples and sample sets

(define-record-type ^{:doc "Metric sample."}
  MetricSample
  ^:private really-make-metric-sample
  metric-sample?
  [name      metric-sample-name
   labels    metric-sample-labels
   value     metric-sample-value
   timestamp metric-sample-timestamp])

(s/def ::metric-sample
  (s/spec
   (partial instance? MetricSample)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name
                                  metric-labels
                                  metric-value-value
                                  metric-value-last-update-time-ms]}]
                       (really-make-metric-sample metric-name
                                                  metric-labels
                                                  metric-value-value
                                                  metric-value-last-update-time-ms))
                     (s/gen (s/keys :req-un [::metric-name
                                             ::metric-labels
                                             ::metric-value-value
                                             ::metric-value-last-update-time-ms]))))))

(s/fdef make-metric-sample
  :args (s/cat :name      ::metric-name
               :labels    ::metric-labels
               :value     ::metric-value-value
               :timestamp ::metric-value-last-update-time-ms)
  :ret ::metric-sample)
(defn make-metric-sample
  [name labels value timestamp]
  (really-make-metric-sample name labels value timestamp))

;; -----------------------------------------------------------------

;; 1. Gauges

(define-record-type ^{:doc "Gauge metric"}
  GaugeMetric
  ^:private really-make-gauge-metric
  gauge-metric?
  [name gauge-metric-name
   help gauge-metric-help])

(s/def ::gauge-metric
  (s/spec
   (partial instance? GaugeMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name metric-help]}]
                       (really-make-gauge-metric metric-name metric-help))
                     (s/gen (s/keys :req-un [::metric-name ::metric-help]))))))

(s/fdef make-gauge-metric
  :args (s/cat :metric-name ::metric-name
               :metric-help ::metric-help)
  :ret ::gauge-metric)
(defn make-gauge-metric
  [metric-name metric-help]
  (really-make-gauge-metric metric-name metric-help))

(define-record-type ^{:doc "Stored Gauge values, i.e. a map from labels to
  metric-values."}
  GaugeValues
  ^:private really-make-gauge-values
  gauge-values?
  [map gauge-values-map])

(s/def ::gauge-values
  (s/spec
   (partial instance? GaugeValues)
   :gen (fn []
          (sgen/fmap (fn [{:keys [gauge-values-map]}]
                       (really-make-gauge-values gauge-values-map))
                     (s/gen (s/keys :req-un [::labels-metric-value-map]))))))

(s/fdef make-gauge-values
  :args (s/cat)
  :ret ::gauge-values)
(defn make-gauge-values
  []
  (really-make-gauge-values empty-values-map))

(s/fdef update-gauge-values
  :args (s/cat :gauge-values  ::gauge-values
               :metric-labels ::metric-labels
               :metric-value  ::metric-value)
  :ret ::gauge-values)
(defn update-gauge-values
  "Updates a `GaugeValues`"
  [gauge-values metric-labels metric-value]
  (lens/overhaul gauge-values
                 gauge-values-map
                 (fn [labels-values-map]
                   (set-metric-value labels-values-map
                                     metric-labels
                                     metric-value))))

(s/fdef gauge-values->metric-samples
  :args (s/cat :name          ::metric-name
               :gauge-values  ::gauge-values
               :metric-labels ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn gauge-values->metric-samples
  [name gauge-values metric-labels]
  (if-let [metric-value (get (gauge-values-map gauge-values) metric-labels)]
    [(make-metric-sample name
                         metric-labels
                         (metric-value-value               metric-value)
                         (metric-value-last-update-time-ms metric-value))]
    []))

(s/fdef prune-stale-gauge-values
  :args (s/cat :gauge-values ::gauge-values
               :time-ms      ::metric-value-last-update-time-ms)
  :ret ::gauge-values)
(defn prune-stale-gauge-values
  [gauge-values time-ms]
  (lens/overhaul gauge-values gauge-values-map prune-stale-metric-value time-ms))

(s/fdef empty-gauge-values?
  :args (s/cat :gauge-values ::gauge-values)
  :ret boolean?)
(defn empty-gauge-values?
  [gauge-values]
  (empty? (gauge-values-map gauge-values)))

;; 2. Counters

(define-record-type ^{:doc "Counter metric"}
  CounterMetric
  ^:private really-make-counter-metric
  counter-metric?
  [name counter-metric-name
   help counter-metric-help
   set-value? counter-metric-set-value?])

(s/def ::counter-metric
  (s/spec
   (partial instance? CounterMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name metric-help]}]
                       (really-make-counter-metric metric-name metric-help false))
                     (s/gen (s/keys :req-un [::metric-name ::metric-help]))))))

(s/fdef make-counter-metric
  :args (s/cat :metric-name ::metric-name
               :metric-help ::metric-help
               :optional (s/? (s/cat :set-value? boolean?)))
  :ret ::counter-metric)
(defn make-counter-metric
  [metric-name metric-help & [set-value?]]
  (really-make-counter-metric metric-name metric-help set-value?))

(define-record-type ^{:doc "Stored Counter values, i.e. a map from labels to
  metric-values."}
  CounterValues
  ^:private really-make-counter-values
  counter-values?
  [map counter-values-map])

(s/def ::counter-values
  (s/spec
   (partial instance? CounterValues)
   :gen (fn []
          (sgen/fmap (fn [{:keys [counter-values-map]}]
                       (really-make-counter-values counter-values-map))
                     (s/gen (s/keys :req-un [::labels-metric-value-map]))))))

(s/fdef make-counter-values
  :args (s/cat)
  :ret ::counter-values)
(defn make-counter-values
  []
  (really-make-counter-values empty-values-map))

(s/fdef update-counter-values
  :args (s/cat :counter-values ::counter-values
               :metric-labels  ::metric-labels
               :metric-value   ::metric-value)
  :ret ::counter-values)
(defn update-counter-values
  "Updates a `CounterMetric`."
  [counter-values metric-labels metric-value]
  (lens/overhaul counter-values
                 counter-values-map
                 (fn [labels-values-map]
                   (inc-metric-value labels-values-map
                                     metric-labels
                                     metric-value))))

(s/fdef counter-values->metric-samples
  :args (s/cat :name           ::metric-name
               :counter-values ::counter-values
               :metric-labels  ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn counter-values->metric-samples
  [name counter-values metric-labels]
  (if-let [metric-value (get (counter-values-map counter-values) metric-labels)]
    [(make-metric-sample name
                         metric-labels
                         (metric-value-value               metric-value)
                         (metric-value-last-update-time-ms metric-value))]
    []))

(s/fdef prune-stale-counter-values
  :args (s/cat :counter-values ::counter-values
               :time-ms        ::metric-value-last-update-time-ms)
  :ret ::counter-values)
(defn prune-stale-counter-values
  [counter-values time-ms]
  (lens/overhaul counter-values counter-values-map prune-stale-metric-value time-ms))

(s/fdef empty-counter-values?
  :args (s/cat :counter-values ::counter-values)
  :ret boolean?)
(defn empty-counter-values?
  [counter-values]
  (empty? (counter-values-map counter-values)))

;; 3. Histograms

(s/def ::threshold (s/get-spec ::metric-value-value))
(s/def ::thresholds (s/coll-of ::threshold))

(define-record-type ^{:doc "Histogram metric"}
  HistogramMetric
  ^:private really-make-histogram-metric
  histogram-metric?
  [name       histogram-metric-name
   help       histogram-metric-help
   thresholds histogram-metric-thresholds])

(s/def ::histogram-metric
  (s/spec
   (partial instance? HistogramMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name metric-help thresholds]}]
                       (really-make-histogram-metric metric-name metric-help thresholds))
                     (s/gen (s/keys :req-un [::metric-name ::metric-help ::thresholds]))))))

(s/fdef make-histogram-metric
  :args (s/cat :metric-name ::metric-name
               :metric-help ::metric-help
               :thresholds  ::thresholds)
  :ret ::histogram-metric)
(defn make-histogram-metric
  [metric-name metric-help thresholds]
  (really-make-histogram-metric metric-name metric-help thresholds))

(define-record-type ^{:doc "Stored Histogram metric values."}
  HistogramMetricValues
  really-make-histogram-metric-values
  histogram-metric-values?
  [last-update-time-ms histogram-metric-values-last-update-time-ms
   sum-value histogram-metric-values-sum-value
   count-value histogram-metric-values-count-value
   bucket-values histogram-metric-values-bucket-values])

(s/def ::bucket-values (s/coll-of ::metric-value-value))

(s/def ::histogram-metric-values
  (s/spec
   (partial instance? HistogramMetricValues)
   :gen (fn []
          (sgen/fmap (fn [{:keys [histogram-metric-values-last-update-time-ms
                                  histogram-metric-values-sum-value
                                  histogram-metric-values-count-value
                                  histogram-metric-values-bucket-values]}]
                       (really-make-histogram-metric-values histogram-metric-values-last-update-time-ms
                                                            histogram-metric-values-sum-value
                                                            histogram-metric-values-count-value
                                                            histogram-metric-values-bucket-values))
                     (s/gen (s/keys :req-un [::metric-value-last-update-time-ms ::metric-value-value ::metric-value-value
                                             ::bucket-values]))))))

(s/fdef make-histogram-metric-values
  :args (s/cat :last-update-time-ms ::metric-value-last-update-time-ms
               :sum-value ::metric-value-value
               :count-value ::metric-value-value
               :bucket-values ::bucket-values)
  :ret ::histogram-metric)
(defn make-histogram-metric-values
  [last-update-time-ms sum-value count-value bucket-values]
  (really-make-histogram-metric-values last-update-time-ms sum-value count-value bucket-values))

(s/fdef make-empty-histogram-metric-values
  :args (s/cat :thresholds ::thresholds)
  :ret ::histogram-metric-values)
(defn make-empty-histogram-metric-values
  [thresholds]
  (make-histogram-metric-values 0 0.0 0.0 (vec (repeat (count thresholds) 0.0))))

(s/fdef update-histogram-values
  :args (s/cat :histogram-metric-values ::histogram-metric-values
               :thresholds ::thresholds
               :metric-value     ::metric-value
               :metric-value-0   ::metric-value
               :metric-value-1   ::metric-value)
  :ret ::histogram-metric-values)
(defn update-histogram-metric-values
  [histogram-metric-values thresholds metric-value]
  (let [value-value (metric-value-value metric-value)
        last-update-time-ms (metric-value-last-update-time-ms metric-value)]
    (-> (or histogram-metric-values (make-empty-histogram-metric-values thresholds))
        (histogram-metric-values-last-update-time-ms last-update-time-ms)
        (lens/overhaul histogram-metric-values-sum-value + value-value)
        (lens/overhaul histogram-metric-values-count-value + 1.0)
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
          (sgen/fmap (fn [{:keys [histogram-values-thresholds histogram-values-map]}]
                       (really-make-histogram-values histogram-values-thresholds histogram-values-map))
                     (s/gen (s/keys :req-un [::thresholds ::histogram-metric-values]))))))

(s/fdef make-histogram-values
  :args (s/cat :thresholds ::thresholds)
  :ret ::histogram-values)
(defn make-histogram-values
  [thresholds]
  (really-make-histogram-values thresholds empty-values-map))

(s/fdef really-update-histogram-values-map
  :args (s/cat :histogram-values-map ::labels-histogram-metric-value-map
               :metric-labels ::metric-labels
               :thresholds ::thresholds
               :metric-value     ::metric-value)
  :ret ::histogram-metric-values)
(defn really-update-histogram-values-map
  [histogram-values-map metric-labels thresholds metric-value]
  (update histogram-values-map metric-labels update-histogram-metric-values
          thresholds metric-value))

(s/fdef update-histogram-values-map
  :args (s/cat :histogram-values-map ::labels-histogram-metric-value-map
               :thresholds ::thresholds
               :metric-labels    ::metric-labels
               :metric-value     ::metric-value)
  :ret ::labels-histogram-metric-value-map)
(defn update-histogram-values-map
  [histogram-values-map thresholds metric-labels metric-value]
  (really-update-histogram-values-map histogram-values-map metric-labels
                                      thresholds metric-value))

(s/fdef update-histogram-values
  :args (s/cat :histogram-values ::histogram-values
               :metric-labels    ::metric-labels
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
  :args (s/cat :basename ::metric-name
               :histogram-values ::histogram-values
               :metric-labels ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn histogram-values->metric-samples
  "Return all metric-samples with the given labels within this histogram-metric."
  [basename histogram-values metric-labels]
  (let [thresholds  (histogram-values-thresholds histogram-values)
        histogram-metric-values (get (histogram-values-map histogram-values) metric-labels)]
    (if (histogram-metric-values? histogram-metric-values)
      (let [last-update-time-ms (histogram-metric-values-last-update-time-ms histogram-metric-values)
            sum (histogram-metric-values-sum-value histogram-metric-values)
            count (histogram-metric-values-count-value histogram-metric-values)
            buckets (histogram-metric-values-bucket-values histogram-metric-values)]
        (concat
         [(make-metric-sample (str basename "_sum")
                              metric-labels
                              sum
                              last-update-time-ms)
          (make-metric-sample (str basename "_count")
                              metric-labels
                              count
                              last-update-time-ms)
          (make-metric-sample (str basename "_bucket")
                              (assoc metric-labels :le "+Inf")
                              count
                              last-update-time-ms)]
         (mapcat (fn [threshold bucket]
                   [(make-metric-sample (str basename "_bucket")
                                        (assoc metric-labels :le (str threshold))
                                        bucket
                                        last-update-time-ms)])
                 thresholds buckets)))
      [])))

(s/fdef prune-stale-histogram-metric-value
  :args (s/cat :histogram-values-map ::labels-histogram-metric-value-map
               :time-ms ::metric-value-last-update-time-ms)
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
               :time-ms          ::metric-value-last-update-time-ms)
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

;; Primitives on stored values

(s/fdef update-stored-values
  :args (s/cat :stored-values ::stored-values
               :metric-labels ::metric-labels
               :metric-value  ::metric-value)
  :ret ::stored-values)
(defn update-stored-values
  [stored-values metric-labels metric-value]
  (cond
    (gauge-values? stored-values)
    (update-gauge-values stored-values metric-labels metric-value)

    (counter-values? stored-values)
    (update-counter-values stored-values metric-labels metric-value)

    (histogram-values? stored-values)
    (update-histogram-values stored-values metric-labels metric-value)))

(s/fdef make-stored-values
  :args (s/cat :metric        ::metric
               :metric-labels ::metric-labels
               :metric-value  ::metric-value)
  :ret ::stored-values)
(defn make-stored-values
  [metric metric-labels metric-value]
  (update-stored-values
   (cond
     (gauge-metric?     metric) (make-gauge-values)
     (counter-metric?   metric) (if (counter-metric-set-value? metric) (make-gauge-values) (make-counter-values))
     (histogram-metric? metric) (make-histogram-values (histogram-metric-thresholds metric)))
   metric-labels metric-value))

(s/fdef update-or-make-stored-values
  :args (s/cat :maybe-stored-values (s/nilable ::stored-values)
               :metric ::metric
               :metric-labels ::metric-labels
               :metric-value ::metric-value)
  :ret ::stored-values)
(defn update-or-make-stored-values
  [maybe-stored-values metric metric-labels metric-value]
  (if (some? maybe-stored-values)
    (update-stored-values maybe-stored-values metric-labels metric-value)
    (make-stored-values metric metric-labels metric-value)))

(s/fdef prune-stale-stored-values
  :args (s/cat :stored-values ::stored-values
               :time-ms       ::metric-value-last-update-time-ms)
  :ret ::stored-values)
(defn prune-stale-stored-values
  [stored-values time-ms]
  (cond
    (gauge-values? stored-values)
    (prune-stale-gauge-values stored-values time-ms)

    (counter-values? stored-values)
    (prune-stale-counter-values stored-values time-ms)

    (histogram-values? stored-values)
    (prune-stale-histogram-values stored-values time-ms)))

(s/fdef empty-stored-values?
  :args (s/cat :stored-values ::stored-values)
  :ret boolean?)
(defn empty-stored-values?
  [stored-values]
  (cond
    (gauge-values? stored-values)
    (empty-gauge-values? stored-values)

    (counter-values? stored-values)
    (empty-counter-values? stored-values)

    (histogram-values? stored-values)
    (empty-histogram-values? stored-values)))

;; -----------------------------------------------------------------

(s/fdef record-metric-1
  :args (s/cat :metric-store ::metric-store-map
               :metric       ::metric
               :labels       ::metric-labels
               :value        ::metric-value)
  :ret ::metric-store-map)
(defn record-metric-1
  [metric-store metric metric-labels metric-value]
  (update-metric-store metric-store metric update-or-make-stored-values metric metric-labels metric-value))

(s/fdef record-metric!
  :args (s/cat :optional-1  (s/? (s/cat :a-metric-store ::metric-store))
               :metric      ::metric
               :labels      ::metric-labels
               :value-value ::metric-value-value
               :optional-2  (s/? (s/cat :last-update (s/nilable ::metric-value-last-update-time-ms))))
  :ret nil)
(defn record-metric!
  "Record a metric."
  ([metric labels value-value]
   (record-metric! metric-store metric labels value-value nil))
  ([metric labels value-value last-update]
   (record-metric! metric-store metric labels value-value last-update))
  ([a-metric-store metric labels value-value last-update]
   (let [last-update (or last-update (time/get-milli-time!))
         metric-value (make-metric-value value-value last-update)]
     (swap! a-metric-store record-metric-1 metric labels metric-value))
   nil))

(declare metric-name)

(s/fdef stored-value->metric-samples
  :args (s/cat :metric        ::metric
               :stored-value  ::stored-values
               :metric-labels ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn stored-value->metric-samples
  [metric stored-value metric-labels]
  (cond
    (gauge-values? stored-value)
    (gauge-values->metric-samples (metric-name metric) stored-value metric-labels)
    (counter-values? stored-value)
    (counter-values->metric-samples (metric-name metric) stored-value metric-labels)
    (histogram-values? stored-value)
    (histogram-values->metric-samples (metric-name metric) stored-value metric-labels)))

(s/fdef stored-value->all-metric-samples
  :args (s/cat :metric       ::metric
               :stored-value ::stored-values)
  :ret (s/coll-of ::metric-sample))
(defn stored-value->all-metric-samples
  [metric stored-value]
  (cond
    (gauge-values? stored-value)
    (mapcat (fn [metric-labels] (gauge-values->metric-samples (metric-name metric) stored-value metric-labels))
            (keys (gauge-values-map stored-value)))
    (counter-values? stored-value)
    (mapcat (fn [metric-labels] (counter-values->metric-samples (metric-name metric) stored-value metric-labels))
            (keys (counter-values-map stored-value)))
    (histogram-values? stored-value)
    (mapcat (fn [metric-labels] (histogram-values->metric-samples (metric-name metric) stored-value metric-labels))
            (keys (histogram-values-map stored-value)))))

(s/fdef get-metric-samples-1
  :args (s/cat :metric-store  ::metric-store-map
               :metric        ::metric
               :metric-labels ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn get-metric-samples-1
  [metric-store metric metric-labels]
  (if-let [stored-value (get metric-store metric)]
    (stored-value->metric-samples metric stored-value metric-labels)
    []))

(s/fdef get-metric-samples!
  :args (s/cat :a-metric-store ::metric-store
               :metric         ::metric
               :labels         ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn get-metric-samples!
  "Return all metric-samples for a given metric within the given
  metric-store with the given labels."
  [a-metric-store metric labels]
  (get-metric-samples-1 @a-metric-store metric labels))

(define-record-type ^{:doc "Metric sample set."}
  MetricSampleSet
  ^:private really-make-metric-sample-set
  metric-sample-set?
  [name metric-sample-set-name
   type metric-sample-set-type
   help metric-sample-set-help
   samples metric-sample-set-samples])

(s/def ::metric-sample-set (s/spec (partial instance? MetricSampleSet)))
(s/def ::metric-type #{:gauge :counter :histogram})

(s/fdef make-metric-sample-set
  :args (s/cat :name        ::metric-name
               :type ::metric-type
               :help        ::metric-help
               :samples     (s/coll-of ::metric-sample))
  :ret ::metric-sample-set)
(defn make-metric-sample-set
  [name type help samples]
  (really-make-metric-sample-set name type help samples))

(s/fdef metric-type
  :args (s/cat :metric ::metric)
  :ret ::metric-type)
(defn metric-type
  [metric]
  (cond
    (gauge-metric?     metric) :gauge
    (counter-metric?   metric) :counter
    (histogram-metric? metric) :histogram))

(s/fdef metric-name
  :args (s/cat :metric ::metric)
  :ret ::metric-name)
(defn metric-name
  [metric]
  (cond
    (gauge-metric?     metric) (gauge-metric-name     metric)
    (counter-metric?   metric) (counter-metric-name   metric)
    (histogram-metric? metric) (histogram-metric-name metric)))

(s/fdef metric-help
  :args (s/cat :metric ::metric)
  :ret ::metric-help)
(defn metric-help
  [metric]
  (cond
    (gauge-metric?     metric) (gauge-metric-help     metric)
    (counter-metric?   metric) (counter-metric-help   metric)
    (histogram-metric? metric) (histogram-metric-help metric)))

(s/fdef get-metric-sample-set-1
  :args (s/cat :metric-store ::metric-store-map
               :metric       ::metric)
  :ret ::metric-sample-set)
(defn get-metric-sample-set-1
  [metric-store metric]
  (if-let [stored-value (get metric-store metric)]
    (make-metric-sample-set (metric-name metric)
                            (metric-type metric)
                            (metric-help metric)
                            (stored-value->all-metric-samples metric stored-value))
    []))

(s/fdef get-all-metric-sample-sets-1
  :args (s/cat :metric-store ::metric-store-map)
  :ret (s/coll-of (s/coll-of ::metric-sample-set)))
(defn get-all-metric-sample-sets-1
  "Return all metric-samples-sets within the given metric-store."
  [metric-store]
  (mapv (fn [metric] (get-metric-sample-set-1 metric-store metric))
        (keys metric-store)))

(s/fdef get-all-metric-sample-sets!
  :args (s/cat :optional (s/? (s/cat :a-metric-store ::metric-store)))
  :ret (s/coll-of (s/coll-of ::metric-sample-set)))
(defn get-all-metric-sample-sets!
  "Return all metric-samples-sets within the given metric-store.
  If no metric-store is given, use the global metric store."
  ([]
   (get-all-metric-sample-sets! metric-store))
  ([a-metric-store]
   (get-all-metric-sample-sets-1 @a-metric-store)))

(s/fdef prune-stale-metrics!
  :args (s/cat :optional (s/? (s/cat :a-metric-store ::metric-store))
               :time-ms        ::metric-value-last-update-time-ms)
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
            (reduce-kv (fn [new-metric-store metric stored-values]
                         (let [new-stored-values (prune-stale-stored-values stored-values time-ms)]
                           (if (empty-stored-values? new-stored-values)
                             new-metric-store
                             (assoc-metric-store new-metric-store metric new-stored-values))))
                       (empty old-metric-store)
                       old-metric-store)))
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

;; COMMANDS on raw metrics

(define-record-type ^{:doc "Monadic command for recording a metric."}
  RecordMetric
  ^:private really-record-metric
  record-metric?
  [metric      record-metric-metric
   labels      record-metric-labels
   value       record-metric-value
   last-update record-metric-last-update])

(s/def ::record-metric
  (s/spec
   (partial instance? RecordMetric)))

(s/fdef record-metric
  :args (s/cat :metric ::metric
               :labels ::metric-labels
               :value  ::metric-value-value
               :optional (s/? (s/cat :last-update (s/nilable ::metric-value-last-update-time-ms))))
  :ret ::record-metric)
(defn record-metric
  [metric labels value & [last-update]]
  (really-record-metric metric labels value last-update))

(define-record-type ^{:doc "Monadic command for pruning stale metrics."}
  PruneStaleMetrics
  ^:private really-prune-stale-metrics
  prune-stale-metrics?
  [time-ms prune-stale-metrics-time-ms])

(s/def ::prune-stale-metrics
  (s/spec
   (partial instance? PruneStaleMetrics)))

(s/fdef prune-stale-metrics
  :args (s/cat :time-ms ::metric-value-last-update-time-ms)
  :ret ::prune-stale-metrics)
(defn prune-stale-metrics
  [time-ms]
  (really-prune-stale-metrics time-ms))

(define-record-type ^{:doc "Monadic command for getting metric samples for one metric and label pair."}
  GetMetricSamples
  ^:private really-get-metric-samples
  get-metric-samples?
  [metric get-metric-samples-metric
   labels get-metric-samples-labels])

(s/def ::get-metric-samples
  (s/spec
   (partial instance? GetMetricSamples)))

(s/fdef get-metric-samples
  :args (s/cat :metric ::metric
               :labels ::metric-labels)
  :ret ::get-metric-samples)
(defn get-metric-samples
  [metric labels]
  (really-get-metric-samples metric labels))

(define-record-type ^{:doc "Monadic command for getting metric samples."}
  GetAllMetricSampleSets
  ^:private really-get-all-metric-sample-sets
  get-all-metric-sample-sets?
  [])

(s/def ::get-all-metric-sample-sets
  (s/spec
   (partial instance? GetAllMetricSampleSets)))

(s/fdef get-all-metric-sample-sets
  :args (s/cat)
  :ret ::get-all-metric-sample-sets)
(defn get-all-metric-sample-sets
  []
  (really-get-all-metric-sample-sets))

(defn run-metrics
  [run-any env state m]
  (let [a-metric-store metric-store]
    (cond
      (record-metric? m)
      (let [[milli-time state'] (if-let [maybe-last-update (record-metric-last-update m)]
                                  [maybe-last-update state]
                                  (run-any env state time/get-milli-time))]
        [(record-metric! a-metric-store
                         (record-metric-metric m)
                         (record-metric-labels m)
                         (record-metric-value m)
                         milli-time)
         state'])

      (get-metric-samples? m)
      [(get-metric-samples! a-metric-store
                            (get-metric-samples-metric m)
                            (get-metric-samples-labels m))
       state]

      (get-all-metric-sample-sets? m)
      [(get-all-metric-sample-sets-1 @a-metric-store)
       state]

      (prune-stale-metrics? m)
      [(prune-stale-metrics! a-metric-store
                             (prune-stale-metrics-time-ms m))
       state]

      :else
      monad/unknown-command)))

(s/fdef record-and-get!
  :args (s/cat :optional-1 (s/? (s/cat :a-metric-store ::metric-store))
               :metric ::metric
               :labels ::metric-labels
               :value  ::metric-value-value
               :optional-2 (s/? (s/cat :last-update (s/nilable ::metric-value-last-update-time-ms))))
  :ret  (s/coll-of ::metric-sample))
(defn record-and-get!
  ([metric labels value]
   (record-and-get! metric-store metric labels value nil))
  ([metric labels value last-update]
   (record-and-get! metric-store metric labels value last-update))
  ([a-metric-store metric labels value last-update]
   (record-metric! a-metric-store metric labels value last-update)
   (get-metric-samples! a-metric-store metric labels)))

;; TODO: Return -- monad coll-of ::metric-sample
(s/fdef record-and-get
  :args (s/cat :metric ::metric
               :labels ::metric-labels
               :value  ::metric-value-value
               :optional (s/? (s/cat :last-update (s/nilable ::metric-value-last-update-time-ms))))
  :ret  (s/coll-of ::metric-sample))
(defn record-and-get
  [metric labels value & [last-update]]
  (monad/monadic
   (record-metric metric labels value last-update)
   (get-metric-samples metric labels)))

(def monad-command-config
  (monad/combine-monad-command-configs
   (monad/make-monad-command-config
    run-metrics
    {} {})
   time/monad-command-config))
