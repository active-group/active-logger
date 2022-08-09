(ns ^:no-doc active.clojure.logger.metric-accumulator-refac
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
(s/def ::metric-labels-values-map (s/map-of ::metric-labels ::metric-value))

(define-record-type ^{:doc "Metric value is a combination of the `value` itself
  and the `last-update-time-ms` of the value."}
  MetricValue
  ^:private really-make-metric-value
  metric-value?
  [value               metric-value-value
   last-update-time-ms metric-value-last-update-time-ms])

;; TODO: maybe better counter-metric-value and gauge-metric-value?
(s/def ::metric-value-value number?)
;; https://prometheus.io/docs/instrumenting/writing_exporters/
;; "You should not set timestamps on the metrics you expose, let Prometheus
;; take care of that."
(s/def ::metric-value-last-update-time-ms  number?)

(declare make-metric-value)
(s/def ::metric-value
  (s/spec
   (partial instance? MetricValue)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-value-value metric-value-last-update-time-ms]}]
                       (make-metric-value metric-value-value metric-value-last-update-time-ms))
                     (s/gen (s/keys :req-un [::metric-value-value ::metric-value-last-update-time-ms]))))))

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


(s/def ::histogram-metric (s/spec (partial instance? HistogramMetric)))

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
  (really-make-histogram-metric metric-name
                                help
                                threshold
                                metric-labels-values-map-sum
                                metric-labels-values-map-count
                                metric-labels-values-map-bucket))

(define-record-type ^{:doc "Metric sample."}
  MetricSample
  ^:private really-make-metric-sample
  metric-sample?
  [name      metric-sample-name
   labels    metric-sample-labels
   value     metric-sample-value
   timestamp metric-sample-timestamp])

(s/def ::metric-sample (s/spec (partial instance? MetricSample)))

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

(s/fdef update-metric-value
  :args (s/cat
         :f              ::update-function
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

(s/fdef inc-metric-value
  :args (s/cat :metric-labels-values-map ::metric-labels-values-map
               :metric-labels            ::metric-labels
               :metric-value             ::metric-value)
  :ret ::metric-labels-values-map)
(defn inc-metric-value
  "Increment a metric-value within a metric-labels-values-map."
  [metric-labels-values-map metric-labels metric-value-2]
  (update metric-labels-values-map metric-labels
          (fn [metric-value-1]
            (update-metric-value + metric-value-1 metric-value-2))))

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

;; TODO: currently not used
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

;; TODO: How do we make sure that users do use `metric-name`s unique?
(s/fdef record-metric!
  :args (s/cat :metric-store ::metric-store
               :metric       ::metric
               :labels       ::metric-labels
               :value-value  ::metric-value-value
               :last-update  (s/nilable ::metric-value-last-update-time-ms))
  :ret ::metric-store)
(defn record-metric!
  "Record a metric."
  [a-raw-metric-store metric labels value-value & [last-update]]
  (let [last-update (or last-update (time/get-milli-time!))
        metric-value (make-metric-value value-value last-update)]
    (cond
      (gauge-metric? metric)
      (let [name (gauge-metric-name metric)]
        (swap! a-raw-metric-store
               (fn [metric-store]
                 (assoc metric-store
                        name
                        (update-gauge-metric metric labels metric-value)))))
      (counter-metric? metric)
      (let [name (counter-metric-name metric)]
        (swap! a-raw-metric-store
               (fn [metric-store]
                 (assoc metric-store
                        name
                        (update-counter-metric metric labels metric-value)))))
      (histogram-metric? metric)
      (let [name (histogram-metric-name metric)]
        (swap! a-raw-metric-store
               (fn [metric-store]
                 (assoc metric-store
                        name
                        (update-histogram-metric metric labels metric-value))))))))

;; TODO: [] or nil if labels are not in metric?
(s/fdef get-metric-sample
  :args (s/cat :metric ::metric
               :labels ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn get-metric-sample
  "Return all metric-samples with the given labels within this metric."
  [metric labels]
  (cond
    (gauge-metric? metric)
    (let [name              (gauge-metric-name              metric)
          labels-values-map (gauge-metric-labels-values-map metric)]
      (if (contains? labels-values-map labels)
        (let [metric-value      (get labels-values-map labels)]
          [(make-metric-sample name
                               labels
                               (metric-value-value               metric-value)
                               (metric-value-last-update-time-ms metric-value))])
        []))
    (counter-metric? metric)
    (let [name              (counter-metric-name              metric)
          labels-values-map (counter-metric-labels-values-map metric)]
      (if (contains? labels-values-map labels)
        (let [metric-value      (get labels-values-map labels)]
          [(make-metric-sample name
                               labels
                               (metric-value-value               metric-value)
                               (metric-value-last-update-time-ms metric-value))])
        []))
    (histogram-metric? metric)
    (let [basename   (histogram-metric-name                     metric)
          threshold  (histogram-metric-threshold                metric)
          map-sum    (histogram-metric-labels-values-map-sum    metric)
          map-count  (histogram-metric-labels-values-map-count  metric)
          map-bucket (histogram-metric-labels-values-map-bucket metric)]
      ;; TODO: do we trust that it is always in all three maps?
      (if (and (contains? map-sum    labels)
               (contains? map-count  labels)
               (contains? map-bucket labels))
        (let [metric-value-sum    (get map-sum labels)
          metric-value-count  (get map-count labels)
          metric-value-bucket (get map-bucket labels)]
      [(make-metric-sample (str basename "_sum")
                           labels
                           (metric-value-value               metric-value-sum)
                           (metric-value-last-update-time-ms metric-value-sum))
       (make-metric-sample (str basename "_count")
                           labels
                           (metric-value-value               metric-value-count)
                           (metric-value-last-update-time-ms metric-value-count))
       (make-metric-sample (str basename "_bucket")
                           (assoc labels :le "+Inf")
                           (metric-value-value               metric-value-count)
                           (metric-value-last-update-time-ms metric-value-count))
       (make-metric-sample (str basename "_bucket")
                           (assoc labels :le (str threshold))
                           (metric-value-value               metric-value-bucket)
                           (metric-value-last-update-time-ms metric-value-bucket))])
        []))))


(s/fdef get-raw-metric-sample!
  :args (s/cat :a-raw-metric-store ::metric-store
               :metric-name        ::metric-name
               :labels             ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn get-raw-metric-sample!
  "Return all metric-samples for a given metric-name within the given
  metric-store with the given labels."
  [a-raw-metric-store metric-name labels]
  (when-let [metric (get @a-raw-metric-store metric-name)]
    (get-metric-sample metric labels)))

(s/fdef get-all-labels-of-metric
  :args (s/cat :metric ::metric)
  :ret (s/nilable (s/coll-of ::metric-labels)))
(defn get-all-labels-of-metric
  [metric]
  (cond
    (gauge-metric? metric)
    (keys (gauge-metric-labels-values-map metric))
    (counter-metric? metric)
    (keys (counter-metric-labels-values-map metric))
    ;; TODO: is this enough? are we sure that sum/count/bucket have only the same labels?
    (histogram-metric? metric)
    (keys (histogram-metric-labels-values-map-sum metric))))

(s/fdef get-metric-samples!
  :args (s/cat :a-raw-metric-store ::metric-store)
  :ret (s/coll-of (s/coll-of ::metric-sample)))
(defn get-metric-samples!
  "Return all metric-samples within the given metric-store."
  [a-raw-metric-store]
  ;; for each metric within the metric-store
  (reduce-kv (fn [r1 _metric-name metric]
               (concat r1 (when-let [labelss (get-all-labels-of-metric metric)]
                            (reduce-kv (fn [r2 labels _v]
                                         (concat r2 (get-metric-sample metric labels)))
                                       []
                                       labelss))))
             []
             @a-raw-metric-store))
