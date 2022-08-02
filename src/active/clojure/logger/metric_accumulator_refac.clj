(ns ^:no-doc active.clojure.logger.metric-accumulator_refac
  "Metrics."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]
            [active.clojure.monad :as monad]

            [active.clojure.logger.time :as time]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(s/check-asserts true)

;; DATA: raw metrics

(s/def ::metric-store-map (s/map-of ::metric-name ::metric))

(s/fdef fresh-metric-store-map
  :ret ::metric-store-map)
(defn ^:no-doc fresh-metric-store-map
  []
  {})

;; TODO: Can we improve the type of the metric-store?
(s/def ::metric-store (partial instance? clojure.lang.Atom))

(s/fdef fresh-raw-metric-store
  :ret ::metric-store)
(defn ^:no-doc fresh-raw-metric-store
  []
  (atom (fresh-metric-store-map)))

(defonce raw-metric-store (fresh-raw-metric-store))

(defn set-global-raw-metric-store!
  [fresh-metric-store-map]
  (reset! raw-metric-store fresh-metric-store-map))

(defn reset-global-raw-metric-store!
  []
  (reset! raw-metric-store (fresh-metric-store-map)))

(s/def ::metric-name string?)
(s/def ::help        string?)
(s/def ::metric-labels (s/map-of keyword? any?))
(s/def ::metric-labels-values-map (s/map-of ::metric-labels ::metric-values))

(define-record-type ^{:doc "Metric value is a combination of the `value` itself
  and the `last-update-time-ms` of the value."}
  MetricValue
  ^:private really-make-metric-value
  metric-value?
  [value               metric-value-value
   last-update-time-ms metric-value-last-update-time-ms])

;; TODO: maybe better counter-metric-value and gauge-metric-value?
(s/def ::metric-value-value (s/or :gauge-metric-value-value   ::gauge-metric-value-value
                                  :counter-metric-value-value ::counter-metric-value-value))
(s/def ::gauge-metric-value-value number?)
;; By accepting only non negative numbers we make sure that counters can only be
;; incremented when using `update-metric-value`.
;; We accept 0 to initialize counters (e.g. histogram empty bucket).
(s/def ::counter-metric-value-value (s/or :zero     zero?
                                          :positive pos?))
;; https://prometheus.io/docs/instrumenting/writing_exporters/
;; "You should not set timestamps on the metrics you expose, let Prometheus
;; take care of that."
(s/def ::metric-value-last-update-time-ms  number?)

(declare make-metric-value)
(s/def ::metric-value
  (s/spec
   (partial instance? MetricValue)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-value-value
                                  metric-value-last-update-time-ms]}]
                       (make-metric-value metric-value-value
                                          metric-value-last-update-time-ms))
                     (s/gen (s/keys :req-un [::metric-value-value
                                             ::metric-value-last-update-time-ms]))))))

(s/fdef make-metric-value
  :args (s/cat :value       ::metric-value-value
               :update-time ::metric-value-last-update-time-ms)
  :ret  ::metric-value)
(defn make-metric-value
  [value update-time]
  (really-make-metric-value value update-time))

(s/def ::metric (s/or :gauge-metric     ::gauge-metric
                      :counter-metric   ::counter-metric
                      :histogram-metric ::histogram-metric))

(define-record-type ^{:doc "Gauge metric"}
  GaugeMetric
  ^:private really-make-gauge-metric
  gauge-metric?
  [name              gauge-metric-name
   help              gauge-metric-help
   labels-values-map gauge-metric-labels-values-map])

(declare make-gauge-metric)
(s/def ::gauge-metric
  (s/spec
   (partial instance? GaugeMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name
                                  help
                                  metric-labels-values-map]}]
                       (make-gauge-metric metric-name
                                          help
                                          metric-labels-values-map))
                     (s/gen (s/keys :req-un [::metric-name
                                             ::help
                                             ::metric-labels-values-map]))))))
(s/fdef make-gauge-metric
  :args (s/cat :metric-name              ::metric-name
               :help                     ::help
               :metric-labels-values-map ::metric-labels-values-map)
  :ret ::gauge-metric)
(defn make-gauge-metric
  [metric-name help metric-labels-values-map]
  (really-make-gauge-metric metric-name help metric-labels-values-map))

(define-record-type ^{:doc "Counter metric"}
  CounterMetric
  ^:private really-make-counter-metric
  counter-metric?
  [name              counter-metric-name
   help              counter-metric-help
   labels-values-map counter-metric-labels-values-map])

(declare make-counter-metric)
(s/def ::counter-metric
  (s/spec
   (partial instance? CounterMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name
                                  help
                                  metric-labels-values-map]}]
                       (make-counter-metric metric-name
                                          help
                                          metric-labels-values-map))
                     (s/gen (s/keys :req-un [::metric-name
                                             ::help
                                             ::metric-labels-values-map]))))))
(s/fdef make-counter-metric
  :args (s/cat :metric-name              ::metric-name
               :help                     ::help
               :metric-labels-values-map ::metric-labels-values-map)
  :ret ::counter-metric)
(defn make-counter-metric
  [metric-name help metric-labels-values-map]
  (really-make-counter-metric metric-name help metric-labels-values-map))


(define-record-type ^{:doc "Histogram metric"}
  HistogramMetric
  ^:private really-make-histogram-metric
  histogram-metric?
  [name                     histogram-metric-name
   help                     histogram-metric-help
   threshold                histogram-metric-threshold
   labels-values-map-sum    histogram-metric-labels-values-map-sum
   labels-values-map-count  histogram-metric-labels-values-map-count
   labels-values-map-bucket histogram-metric-labels-values-map-bucket])


(declare make-histogram-metric)
(s/def ::histogram-metric
  (s/spec
   (partial instance? HistogramMetric)
   :gen (fn []
          ;; TODO: Does this work? mapping s/gen keys / arguments --- names?
          (sgen/fmap (fn [{:keys [metric-name
                                  help
                                  metric-value-value
                                  metric-labels-values-map-sum
                                  metric-labels-values-map-count
                                  metric-labels-values-map-bucket]}]
                       (make-histogram-metric metric-name
                                              help
                                              metric-value-value
                                              metric-labels-values-map-sum
                                              metric-labels-values-map-count
                                              metric-labels-values-map-bucket))
                     (s/gen (s/keys :req-un [::metric-name
                                             ::help
                                             ::metric-value-value
                                             ::metric-labels-values-map
                                             ::metric-labels-values-map
                                             ::metric-labels-values-map]))))))
(s/fdef make-histogram-metric
  :args (s/cat :metric-name                     ::metric-name
               :help                            ::help
               :threshold                       ::metric-value-value
               :metric-labels-values-map-sum    ::metric-labels-values-map
               :metric-labels-values-map-count  ::metric-labels-values-map
               :metric-labels-values-map-bucket ::metric-labels-values-map)
  :ret ::histogram-metric)
(defn make-histogram-metric
  [metric-name
   help
   threshold
   metric-labels-values-map-sum
   metric-labels-values-map-count
   metric-labels-values-map-bucket]
  (really-make-counter-metric metric-name
                              help
                              threshold
                              metric-labels-values-map-sum
                              metric-labels-values-map-count
                              metric-labels-values-map-bucket))

;; -----------------------------------------------------------------

(s/fdef set-metric-value
  :args (s/cat :metric-labels-values-map ::metric-labels-values-map
               :metric-labels            ::metric-labels
               :metric-value             ::metric-value)
  :ret ::metric-labels-values-map)
(defn set-metric-value
  "Set a metric-value within a metric-labels-values-map."
  [metric-labels-values-map metric-labels metric-value]
  (assoc metric-labels-values-map metric-labels metric-value))

(s/def ::update-function
  (s/fspec
   :args (s/cat :metric-value-value-1 ::metric-value-value
                :metric-value-value-2 ::metric-value-value)
   :ret ::metric-value-value))

(s/fdef update-metric-value'
  :args (s/cat
         :f              ::update-function
         :metric-value-1 (s/nilable ::metric-value)
         :metric-value-2 ::metric-value)
  :ret ::metric-value)
(defn update-metric-value'
  "Update a `MetricValue` by applying a function `f` to the `value`s of the old
  and the new `MetricValue` and setting the `timestamp` to the new timestamp. If
  the old-metric-value` is `nil` take the new-metric-value."
  [f metric-value-1 metric-value-2]
  (if metric-value-1
    (-> metric-value-1
        (lens/overhaul metric-value-value f (metric-value-value metric-value-2))
        (metric-value-last-update-time-ms (metric-value-last-update-time-ms metric-value-2)))
    metric-value-2))

(s/fdef update-metric-value
  :args (s/cat :metric-labels-values-map ::metric-labels-values-map
               :metric-labels            ::metric-labels
               :metric-value             ::metric-value
               :update-function          ::update-function)
  :ret ::metric-labels-values-map)
(defn update-metric-value
  "Update a metric-value within a metric-labels-values-map."
  [metric-labels-values-map metric-labels metric-value-2 f]
  (update metric-labels-values-map metric-labels
          (fn [metric-value-1]
            (update-metric-value' f metric-value-1 metric-value-2))))

(s/fdef sum-metric-value
  :args (s/cat
         :metric-value-1 (s/nilable ::metric-value)
         :metric-value-2 ::metric-value)
  :ret ::metric-value)
(def sum-metric-value (partial update-metric-value' +))

(s/fdef inc-metric-value
  :args (s/cat :metric-labels-values-map ::metric-labels-values-map
               :metric-labels            ::metric-labels
               :metric-value             ::metric-value)
  :ret ::metric-labels-values-map)
(defn inc-metric-value
  "Increment a metric-value within a metric-labels-values-map."
  [metric-labels-values-map metric-labels metric-value]
  (update-metric-value metric-labels-values-map metric-labels metric-value sum-metric-value))

;; update metrics

(s/fdef update-gauge-metric
  :args (s/cat :gauge-metric  ::gauge-metric
               :metric-labels ::metric-labels
               :metric-value  ::metric-value)
  :ret ::gauge-metric)
(defn update-gauge-metric
  "Updates a `GaugeMetric`."
  [gauge-metric metric-labels metric-value]
  (lens/overhaul gauge-metric
                 gauge-metric-labels-values-map
                 (fn [labels-values-map]
                   (set-metric-value labels-values-map
                                     metric-labels
                                     metric-value))))

(s/fdef update-counter-metric
  :args (s/cat :counter-metric ::counter-metric
               :metric-labels  ::metric-labels
               :metric-value   ::metric-value)
  :ret ::counter-metric)
(defn update-counter-metric
  "Updates a `CounterMetric`."
  [counter-metric metric-labels metric-value]
  (lens/overhaul counter-metric
                 counter-metric-labels-values-map
                 (fn [labels-values-map]
                   (inc-metric-value labels-values-map
                                     metric-labels
                                     metric-value))))

(s/fdef update-histogram-metric
  :args (s/cat :histogram-metric ::histogram-metric
               :metric-labels    ::metric-labels
               :metric-value     ::metric-value)
  :ret ::histogram-metric)
(defn update-histogram-metric
  "Updates a `HistogramMetric`."
  [histogram-metric metric-labels metric-value]
  (let [last-update         (metric-value-last-update-time-ms metric-value)
        value-value         (metric-value-value               metric-value)
        threshold           (histogram-metric-threshold histogram-metric)
        metric-value-0      (make-metric-value 0 last-update)
        metric-value-1      (make-metric-value 1 last-update)
        metric-value-bucket (if (<= value-value threshold)
                              metric-value-1
                              metric-value-0)]
    (-> histogram-metric
        (lens/overhaul histogram-metric-labels-values-map-sum
                       (fn [labels-values-map]
                         (inc-metric-value labels-values-map
                                           metric-labels
                                           metric-value)))
        (lens/overhaul histogram-metric-labels-values-map-count
                       (fn [labels-values-map]
                         (inc-metric-value labels-values-map
                                           metric-labels
                                           metric-value-1)))
         (lens/overhaul histogram-metric-labels-values-map-bucket
                        (fn [labels-values-map]
                          (inc-metric-value labels-values-map
                                            metric-labels
                                            metric-value-bucket))))))

(s/fdef update-metric
  :args (s/cat :metric        ::metric
               :metric-labels ::metric-labels
               :metric-value  ::metric-value)
  :ret ::metric)
(defn update-metric
  "Update a `Metric`."
  [metric metric-labels metric-value]
  (cond
    (gauge-metric? metric)
    (update-gauge-metric metric metric-labels metric-value)

    (counter-metric? metric)
    (update-counter-metric metric metric-labels metric-value)

    (histogram-metric? metric)
    (update-histogram-metric metric metric-labels metric-value)))
