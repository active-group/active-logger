(ns ^:no-doc active.clojure.logger.metric-accumulator
  "Metrics."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]
            [active.clojure.monad :as monad]))

;; DATA: raw metrics

(defn ^:no-doc fresh-raw-metric-store
  []
  (atom {}))

(define-record-type ^{:doc "Metric key with it's `name` and `labels`, where
`name` must be a string and `labels` must be a map."}
  MetricKey
  really-make-metric-key
  metric-key?
  [name metric-key-name
   labels metric-key-labels])

(defn make-metric-key
  [name labels]
  (assert (string? name) "name must be a string")
  (assert (map? labels) "labels must be a map")
  (really-make-metric-key name labels))

(define-record-type ^{:doc "Metric value with it's `value` and `timestamp`,
where `value` must be a number and ``timestamp` must be a number or nil."}
  MetricValue
  really-make-metric-value
  metric-value?
  [value metric-value-value
   timestamp metric-value-timestamp])

(defn make-metric-value
  [value timestamp]
  (assert (number? value) "value must be a number")
  (assert (or (number? timestamp) (nil? timestamp))
          "timestamp must be a number or nil")
  (really-make-metric-value value timestamp))

(define-record-type ^{:doc "Metric sample with the sum of the fields of
`MetricKey` and `MetricValue` and the same constraints."}
  MetricSample
  really-make-metric-sample
  metric-sample?
  [name metric-sample-name
   labels metric-sample-labels
   value metric-sample-value
   timestamp metric-sample-timestamp])

(defn make-metric-sample
  [name labels value timestamp]
  (assert (string? name) "name must be a string")
  (assert (map? labels) "labels must be a map")
  (assert (number? value) "value must be a number")
  (assert (or (number? timestamp) (nil? timestamp))
          "timestamp must be a number or nil")
  (really-make-metric-sample name labels value timestamp))

(defn set-raw-metric!
  "Sets a `metric-value` (`MetricValue`) for the given `metric-key`
  (`MetricKey`) in `a-raw-metric-store` (`Map`). If `metric-key` is not in
  `a-raw-metric-store` key and value are added, otherwise the value of
  `metric-key` will be overwritten."
  [a-raw-metric-store metric-key metric-value]
  (swap! a-raw-metric-store assoc metric-key metric-value))

;; Update a metric-value (`MetricValue`) by applying a function `f` to the
;; `value`s of `metric-value-1` (`MetricValue`) and `metric-value-2`
;; (`MetricValue`) and setting the `timestamp` to `metric-value-2`s timestamp.
;; If `metric-value-1` is `nil` take `metric-value-2`.
(defn update-metric-value
  [f metric-value-1 metric-value-2]
  (if metric-value-1
    (-> metric-value-1
        (lens/overhaul metric-value-value f (metric-value-value metric-value-2))
        (metric-value-timestamp (metric-value-timestamp metric-value-2)))
    metric-value-2))

(def sum-metric-value (partial update-metric-value +))

(defn inc-raw-metric!
  "Find a raw-metric with `metric-key` (`MetricKey`) in `a-raw-metric-store`
  (`Map`) and update this metric's value (`MetricValue`) by adding
  `metric-value` to the current metric's `value` and setting the `timestamp` of
  `metric-value`. If the metric is not in `a-raw-metric-store` it will be added
  as `metric-key` with `metric-value`."
  [a-raw-metric-store metric-key metric-value]
  (swap! a-raw-metric-store update metric-key sum-metric-value metric-value))

(defn get-raw-metric-sample!
  "Find a raw-metric with `metric-key` (`MetricKey`) in `a-raw-metric-store`
  (`Map`) and return it as a `MetricSample`."
  [a-raw-metric-store metric-key]
  (when-let [metric-value (get @a-raw-metric-store metric-key)]
    (make-metric-sample (metric-key-name metric-key)
                        (metric-key-labels metric-key)
                        (metric-value-value metric-value)
                        (metric-value-timestamp metric-value))))

(defn get-raw-metric-samples!
  "Return all raw-metrics in `a-raw-metric-store` as `MetricSample`s."
  [a-raw-metric-store]
  (reduce-kv (fn [r metric-key metric-value]
               (concat r
                       [(make-metric-sample (metric-key-name metric-key)
                                            (metric-key-labels metric-key)
                                            (metric-value-value metric-value)
                                            (metric-value-timestamp metric-value))]))
             []
             @a-raw-metric-store))

;; COMMANDS on raw metrics

(define-record-type ^{:doc "Monadic command for setting metrics."}
  SetRawMetric
  (make-set-raw-metric metric-key value timestamp)
  set-raw-metric?
  [metric-key set-raw-metric-metric-key
   value set-raw-metric-value
   timestamp set-raw-metric-timestamp])

(define-record-type ^{:doc "Monadic command for incrementing metrics."}
  IncrementRawMetric
  (make-inc-raw-metric metric-key value timestamp)
  inc-raw-metric?
  [metric-key inc-raw-metric-metric-key
   value inc-raw-metric-value
   timestamp inc-raw-metric-timestamp])

(define-record-type ^{:doc "Monadic command for getting metrics."}
  GetRawMetricSample
  (get-raw-metric-sample metric-key)
  get-raw-metric-sample?
  [metric-key get-raw-metric-sample-metric-key])

(defn with-maybe-timestamp
  [f metric-key metric-value & [timestamp]]
  (monad/monadic
    ;; https://prometheus.io/docs/instrumenting/writing_exporters/
    ;; "You should not set timestamps on the metrics you expose, let Prometheus
    ;; take care of that."
    #_[timestamp (if timestamp
                   (monad/return timestamp)
                   (timeout/get-milli-time))]
    (f metric-key metric-value timestamp)))

(def set-raw-metric (partial with-maybe-timestamp make-set-raw-metric))

(def inc-raw-metric (partial with-maybe-timestamp make-inc-raw-metric))

(defn run-metrics
  [_run-any env state m]
  (let [raw-metric-store (::raw-metric-store env)]
    (cond
      (set-raw-metric? m)
      [(set-raw-metric! raw-metric-store
                        (set-raw-metric-metric-key m)
                        (set-raw-metric-value m))
       state]

      (inc-raw-metric? m)
      [(inc-raw-metric! raw-metric-store
                        (inc-raw-metric-metric-key m)
                        (inc-raw-metric-value m))
       state]

      (get-raw-metric-sample? m)
      [(get-raw-metric-sample! raw-metric-store
                               (get-raw-metric-sample-metric-key m))
       state]

      :else
      monad/unknown-command)))

;; METRICS
;; prometheus-style:
;; - counter
;; - gauge
;; - histogram

(define-record-type ^{:doc "Counter metric."}
  CounterMetric
  really-make-counter-metric
  counter-metric?
  [help counter-metric-help
   mkey counter-metric-key])

(defn make-counter-metric
  [name & [help labels]]
  (let [metric-key (make-metric-key name labels)]
    (really-make-counter-metric help metric-key)))

(define-record-type ^{:doc "Gauge metric."}
  GaugeMetric
  really-make-gauge-metric
  gauge-metric?
  [help gauge-metric-help
   mkey gauge-metric-key])

(defn make-gauge-metric
  [name & [help labels]]
  (let [metric-key (make-metric-key name labels)]
    (really-make-gauge-metric help metric-key)))

(define-record-type ^{:doc "Histogram metric."}
  HistogramMetric
  really-make-histogram-metric
  histogram-metric?
  [help histogram-metric-help
   threshold histogram-metric-threshold
   total-sum histogram-metric-total-sum
   bucket-le-threshold histogram-metric-bucket-le-threshold
   total-count histogram-metric-total-count
   bucket-le-inf histogram-metric-bucket-le-inf])

(defn make-histogram-metric
  [basename threshold & [help labels]]
  (let [total-sum           (make-counter-metric (str basename "_sum") nil labels)
        bucket-le-threshold (make-counter-metric (str basename "_bucket") nil (assoc labels :le (str threshold)))
        total-count         (make-counter-metric (str basename "_count") nil labels)
        bucket-le-inf       (make-counter-metric (str basename "_bucket") nil (assoc labels :le "+Inf"))]
    (really-make-histogram-metric help threshold total-sum bucket-le-threshold total-count bucket-le-inf)))


(defn record-metric!
  [raw-metric-store metric & [value timestamp]]
  (let [metric-value (make-metric-value value timestamp)]
    (cond
      (counter-metric? metric)
      (inc-raw-metric! raw-metric-store (counter-metric-key metric) metric-value)

      (gauge-metric? metric)
      (set-raw-metric! raw-metric-store (gauge-metric-key metric) metric-value)

      (histogram-metric? metric)
      (do
        (record-metric! raw-metric-store (histogram-metric-total-sum metric) value timestamp)
        (record-metric! raw-metric-store (histogram-metric-bucket-le-inf metric) 1 timestamp)
        (record-metric! raw-metric-store (histogram-metric-total-count metric) 1 timestamp)
        (if (<= value (histogram-metric-threshold metric))
          (record-metric! raw-metric-store (histogram-metric-bucket-le-threshold metric) 1 timestamp)
          (record-metric! raw-metric-store (histogram-metric-bucket-le-threshold metric) 0 timestamp))))))

(defn record-metric
  [metric & [value timestamp]]
  (let [metric-value (make-metric-value value timestamp)]
    (cond
      (counter-metric? metric)
      (inc-raw-metric (counter-metric-key metric) metric-value)

      (gauge-metric? metric)
      (inc-raw-metric (gauge-metric-key metric) metric-value)

      (histogram-metric? metric)
      (monad/monadic
        (let [inc-by-1-metric-value (make-metric-value 1 timestamp)])
        (record-metric (histogram-metric-total-sum metric) metric-value)
        (record-metric (histogram-metric-bucket-le-inf metric) inc-by-1-metric-value)
        (record-metric (histogram-metric-total-count metric) inc-by-1-metric-value)
        (if (<= value (histogram-metric-threshold metric))
          (record-metric (histogram-metric-bucket-le-threshold metric) inc-by-1-metric-value)
          (monad/return nil))))))


(defn get-metrics!
  "Returns a collection of metric samples."
  [raw-metric-store metric]
  (cond
    (counter-metric? metric)
    [(get-raw-metric-sample! raw-metric-store (counter-metric-key metric))]

    (gauge-metric? metric)
    [(get-raw-metric-sample! raw-metric-store (gauge-metric-key metric))]

    (histogram-metric? metric)
    (mapcat (partial get-metrics! raw-metric-store)
            [(histogram-metric-total-sum metric)
             (histogram-metric-bucket-le-inf metric)
             (histogram-metric-total-count metric)
             (histogram-metric-bucket-le-threshold metric)])))

(defn get-metrics
  "Returns a collection of metric samples."
  [metric]
  (cond
    (counter-metric? metric)
    (monad/monadic
     [metric (get-raw-metric-sample (counter-metric-key metric))]
     (monad/return [metric]))

    (gauge-metric? metric)
    (monad/monadic
     [metric (get-raw-metric-sample (gauge-metric-key metric))]
     (monad/return [metric]))

    (histogram-metric? metric)
    (monad/monadic
     [metrics (monad/sequ
               (mapv get-metrics [(histogram-metric-total-sum metric)
                                  (histogram-metric-bucket-le-inf metric)
                                  (histogram-metric-total-count metric)
                                  (histogram-metric-bucket-le-threshold metric)]))]
     (monad/return (apply concat metrics)))))

(defn record-and-get!
  [metrics metric & [value timestamp]]
  (record-metric! metrics metric value timestamp)
  (get-metrics! metrics metric))

(defn record-and-get
  [metric & [value timestamp]]
  (monad/monadic
    (record-metric metric value timestamp)
    (get-metrics metric)))


(defn monad-command-config
  [& [metrics]]
  (monad/make-monad-command-config
    run-metrics
    {::raw-metric-store (or metrics (fresh-raw-metric-store))} {}))
