(ns ^:no-doc active.clojure.logger.metric-accumulator
  "Metrics."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]
            [active.clojure.monad :as monad]))

;; DATA

(defn ^:no-doc fresh-metrics
  []
  (atom {}))

;; FIXME: document assertions == types of fields
(define-record-type ^{:doc "Metric key. ..."}
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

(define-record-type ^{:doc "Metric value."}
  MetricValue
  make-metric-value
  metric-value?
  [value metric-value-value
   timestamp metric-value-timestamp])

(define-record-type ^{:doc "Metric sample."}
  MetricSample
  make-metric-sample
  metric-sample?
  [name metric-sample-name
   labels metric-sample-labels
   value metric-sample-value
   timestamp metric-sample-timestamp])

(defn set-metric!
  [a-metrics metric-key metric-value]
  (swap! a-metrics assoc metric-key metric-value))

(defn update-metric-value
  [f metric-value-1 metric-value-2]
  (if metric-value-1
    (-> metric-value-1
        (lens/overhaul metric-value-value f (metric-value-value metric-value-2))
        (metric-value-timestamp (metric-value-timestamp metric-value-2)))
    metric-value-2))

(def inc-metric-value (partial update-metric-value inc))

(defn inc-metric!
  [a-metrics metric-key metric-value]
  (swap! a-metrics update metric-key inc-metric-value metric-value))

(defn get-metric-sample!
  [a-metrics metric-key]
  (when-let [metric-value (get @a-metrics metric-key)]
    (make-metric-sample (metric-key-name metric-key)
                        (metric-key-labels metric-key)
                        (metric-value-value metric-value)
                        (metric-value-timestamp metric-value))))

(defn get-metric-samples!
  [a-metrics]
  (reduce-kv (fn [r metric-key metric-value]
               (concat r
                       [(make-metric-sample (metric-key-name metric-key)
                                            (metric-key-labels metric-key)
                                            (metric-value-value metric-value)
                                            (metric-value-timestamp metric-value))]))
              []
              @a-metrics))

;; COMMANDS

(define-record-type ^{:doc "Monadic command for setting metrics."}
  SetMetric
  (make-set-metric metric-key value timestamp)
  set-metric?
  [metric-key set-metric-metric-key
   value set-metric-value
   timestamp set-metric-timestamp])

(define-record-type ^{:doc "Monadic command for incrementing metrics."}
  IncrementMetric
  (make-inc-metric metric-key value timestamp)
  inc-metric?
  [metric-key inc-metric-metric-key
   value inc-metric-value
   timestamp inc-metric-timestamp])

(define-record-type ^{:doc "Monadic command for getting metrics."}
  GetMetricSample
  (make-get-metric-sample metric-key)
  get-metric-sample?
  [metric-key get-metric-sample-metric-key])

(defn with-maybe-timestamp
  [f metric value & [timestamp]]
  (monad/monadic
    ;; https://prometheus.io/docs/instrumenting/writing_exporters/
    ;; "You should not set timestamps on the metrics you expose, let Prometheus
    ;; take care of that."
    #_[timestamp (if timestamp
                   (monad/return timestamp)
                   (timeout/get-milli-time))]
    (f metric value timestamp)))

(def set-metric (partial with-maybe-timestamp make-set-metric))

(def inc-metric (partial with-maybe-timestamp make-inc-metric))

(defn run-metrics
  [_run-any env state m]
  (let [_metrics (::metrics env)]
    (cond
      (set-metric? m)
      [(set-metric! (set-metric-metric-key m)
                    (set-metric-value m)
                    (set-metric-timestamp m))
       state]

      (inc-metric? m)
      [(inc-metric! (inc-metric-metric-key m)
                    (inc-metric-value m)
                    (inc-metric-timestamp m))
       state]

      (get-metric-sample? m)
      [(get-metric-sample! (get-metric-sample-metric-key m))
       state]

      :else
      monad/unknown-command)))

;; METRICS

;; active-logger log-metric

(define-record-type ^{:doc "Metric."}
  Metric
  make-metric
  metric?
  [help metric-help
   ;; value timestamp -> m
   record-fn metric-record-fn
   get-fn metric-get-fn])

(defn record-metric
  [metric & [value timestamp]]
  ((metric-record-fn metric) value timestamp))

(defn get-metric
  [metric]
  (partial (metric-get-fn metric)))

(defn m-get-metrics
  [& ms]
  (monad/monadic
    [samples (monad/sequ (map make-get-metric-sample ms))]
    (monad/return (remove nil? samples))))

(defn make-gauge-metric
  [name & [help labels]]
  (let [mkey (make-metric-key name labels)]
    (make-metric help
                 (fn [value & [timestamp]]
                   (set-metric mkey value timestamp))
                 (constantly (m-get-metrics mkey)))))

(defn make-counter-metric
  [name & [help labels]]
  (let [mkey (make-metric-key name labels)]
    (make-metric help
                 (fn [& [value timestamp]]
                   (inc-metric mkey (or value 1) timestamp))
                 (constantly (m-get-metrics mkey)))))

(defn make-histogram-metric
  [basename threshold & [help labels]]
  (let [total-sum (make-counter-metric (str basename "_sum") nil labels)
        bucket-le-threshold (make-counter-metric (str basename "_bucket") nil (assoc labels :le (str threshold)))
        total-count (make-counter-metric (str basename "_count") nil labels)
        bucket-le-inf (make-counter-metric (str basename "_bucket") nil (assoc labels :le "+Inf"))] ;; counter
    (make-metric
      help
      (fn [value & [timestamp]]
        (monad/monadic
          (record-metric total-sum value timestamp)
          (record-metric bucket-le-inf 1 timestamp)
          (record-metric total-count 1 timestamp)
          (if (<= value threshold)
            (monad/monadic
              (record-metric bucket-le-threshold 1 timestamp))
            (monad/return nil))))
      (constantly (m-get-metrics total-sum total-count bucket-le-threshold bucket-le-inf)))))

(defn monad-command-config
  [& [metrics]]
  (fn [_label _inject-event-fn _get-milli-time-fn]
    (monad/make-monad-command-config
      run-metrics
      {::metrics (or metrics (fresh-metrics))} {})))
