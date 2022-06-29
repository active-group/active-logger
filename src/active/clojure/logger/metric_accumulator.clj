(ns ^:no-doc active.clojure.logger.metric-accumulator
  "Metrics."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]
            [active.clojure.monad :as monad]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.test.check.generators]))

(s/check-asserts true)

;; DATA: raw metrics

;; TODO: Can we improve the type of the metric-store?
(s/def ::metric-store (partial instance? clojure.lang.Atom))

(s/fdef fresh-raw-metric-store
  :ret ::metric-store)
(defn ^:no-doc fresh-raw-metric-store
  []
  (atom {}))

(define-record-type ^{:doc "Metric key with it's `name` and `labels`, where
`name` must be a string and `labels` must be a map."}
  MetricKey
  ^:private really-make-metric-key
  metric-key?
  [name   metric-key-name
   labels metric-key-labels])

(s/def ::metric-key-name   string?)
(s/def ::metric-key-labels (s/map-of keyword? any?))

(declare make-metric-key)  ; We want to refer to the specced
                           ; constructor in `::metric-key` but defined
                           ; it before `make-metric-key` for
                           ; readability.
(s/def ::metric-key
  (s/spec
   (partial instance? MetricKey)  ; We assume every instance of
                                  ; `MetricKey` is constructed via
                                  ; `make-metric-key` and therefore
                                  ; must be valid -- so we don't
                                  ; check the keys again.

          :gen (fn []  ; The generator for `::metric-key` just
                       ; generates a map of specced values (1), takes
                       ; the result of the generator and applies the
                       ; constructor (2) and returns it.
                 (sgen/fmap (fn [{:keys [metric-key-name metric-key-labels]}]
                              ;; (2)
                              (make-metric-key metric-key-name metric-key-labels))
                            ;; (1)
                            (s/gen (s/keys :req-un [::metric-key-name ::metric-key-labels]))))))

;; TODO: clean up
;; - this introduces a test-library in src - should this go into test instead?
(defn gen-metric-keys
  [num-elems]
  (s/spec (s/coll-of ::metric-key :into [])
          :gen (fn []
                 (clojure.test.check.generators/list-distinct (s/gen ::metric-key) {:num-elements num-elems}))))

(s/fdef make-metric-key
  :args (s/cat :name   ::metric-key-name
               :labels ::metric-key-labels)
  :ret  ::metric-key)
(defn make-metric-key
  [name labels]
  ;; maybe do some error checking here if you need validations in
  ;; production runtime.  During testing,
  ;; `clojure.spec.test.alpha/instrument` the specced functions and
  ;; you'll get spec feedback (calling with wrong args, etc.).
  (really-make-metric-key name labels))

(define-record-type ^{:doc "Metric value with it's `value` and `timestamp`,
where `value` must be a number, `timestamp` must be a number or nil and
`last-update-time-ms` must be a number."}
  MetricValue
  ^:private really-make-metric-value
  metric-value?
  [value               metric-value-value
   timestamp           metric-value-timestamp
   last-update-time-ms metric-value-last-update-time-ms])

;; By accepting only non negative numbers we make sure that counters can only be
;; incremented when using `update-metric-value`.
;; We accept 0 to initialize counters (e.g. histogram empty bucket).
(s/def ::metric-value-value (s/and number?
                                   (s/or :zero     zero?
                                         :positive pos?)))
;; https://prometheus.io/docs/instrumenting/writing_exporters/
;; "You should not set timestamps on the metrics you expose, let Prometheus
;; take care of that."
(s/def ::metric-value-timestamp (s/nilable number?))
(s/def ::metric-value-last-update-time-ms  number?)

(declare make-metric-value)
(s/def ::metric-value
  (s/spec
   (partial instance? MetricValue)
          :gen (fn []
                 (sgen/fmap (fn [{:keys [metric-value-value metric-value-timestamp metric-value-last-update-time-ms]}]
                              (make-metric-value metric-value-value metric-value-timestamp metric-value-last-update-time-ms))
                            (s/gen (s/keys :req-un [::metric-value-value ::metric-value-timestamp ::metric-value-last-update-time-ms]))))))

(s/fdef make-metric-value
  :args (s/cat :value       ::metric-value-value
               :timestamp   ::metric-value-timestamp
               :update-time ::metric-value-last-update-time-ms)
  :ret  ::metric-value)
(defn make-metric-value
  [value timestamp update-time]
  (really-make-metric-value value timestamp update-time))

(define-record-type ^{:doc "Metric sample with the sum of the fields of
`MetricKey` and `MetricValue` and the same constraints."}
  MetricSample
  ^:private really-make-metric-sample
  metric-sample?
  [name                metric-sample-name
   labels              metric-sample-labels
   value               metric-sample-value
   timestamp           metric-sample-timestamp
   last-update-time-ms metric-sample-last-update-time-ms])

(declare make-metric-sample)
(s/def ::metric-sample
  (s/spec
   (partial instance? MetricSample)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-key-name metric-key-labels metric-value-value metric-value-timestamp metric-value-last-update-time-ms]}]
                       (make-metric-sample metric-key-name metric-key-labels metric-value-value metric-value-timestamp metric-value-last-update-time-ms))
                     (s/gen (s/keys :req-un [::metric-key-name ::metric-key-labels ::metric-value-value ::metric-value-timestamp ::metric-value-last-update-time-ms]))))))

(s/fdef make-metric-sample
  :args (s/cat :name        ::metric-key-name
               :labels      ::metric-key-labels
               :value       ::metric-value-value
               :timestamp   ::metric-value-timestamp
               :update-time ::metric-value-last-update-time-ms)
  :ret ::metric-sample)
(defn make-metric-sample
  [name labels value timestamp update-time]
  (really-make-metric-sample name labels value timestamp update-time))

(s/fdef set-raw-metric!
  :args (s/cat :a-raw-metric-store ::metric-store
               :metric-key         ::metric-key
               :metric-value       ::metric-value)
  :ret ::metric-value)
(defn set-raw-metric!
  "Sets a `metric-value` (`MetricValue`) for the given `metric-key`
  (`MetricKey`) in `a-raw-metric-store` (`Map`). If `metric-key` is not in
  `a-raw-metric-store` key and value are added, otherwise the value of
  `metric-key` will be overwritten."
  [a-raw-metric-store metric-key metric-value]
  (swap! a-raw-metric-store assoc metric-key metric-value))

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
;; Update a metric-value (`MetricValue`) by applying a function `f` to the
;; `value`s of `metric-value-1` (`MetricValue`) and `metric-value-2`
;; (`MetricValue`) and setting the `timestamp` to `metric-value-2`s timestamp.
;; If `metric-value-1` is `nil` take `metric-value-2`.
(defn update-metric-value
  [f metric-value-1 metric-value-2]
  (if metric-value-1
    (-> metric-value-1
        (lens/overhaul metric-value-value f (metric-value-value metric-value-2))
        (metric-value-timestamp           (metric-value-timestamp           metric-value-2))
        (metric-value-last-update-time-ms (metric-value-last-update-time-ms metric-value-2)))
    metric-value-2))

(s/fdef sum-metric-value
  :args (s/cat
         :metric-value-1 (s/nilable ::metric-value)
         :metric-value-2 ::metric-value)
  :ret ::metric-value)
(def sum-metric-value (partial update-metric-value +))

(s/fdef inc-raw-metric!
  :args (s/cat :a-raw-metric-store ::metric-store
               :metric-key         ::metric-key
               :metric-value       ::metric-value)
  :ret ::metric-value)
(defn inc-raw-metric!
  "Find a raw-metric with `metric-key` (`MetricKey`) in `a-raw-metric-store`
  (`Map`) and update this metric's value (`MetricValue`) by adding
  `metric-value` to the current metric's `value` and setting the `timestamp` of
  `metric-value`. If the metric is not in `a-raw-metric-store` it will be added
  as `metric-key` with `metric-value`."
  [a-raw-metric-store metric-key metric-value]
  (swap! a-raw-metric-store update metric-key sum-metric-value metric-value))

;; TODO return? new-metric-store?
;; TODO are we sure that {} will stay? - related to fresh-metric-store
;; TODO < or <= ?
(s/fdef prune-stale-raw-metrics!
  :args (s/cat :a-raw-metric-store ::metric-store
               :time-ms            ::metric-value-last-update-time-ms))
(defn prune-stale-raw-metrics!
  "Prune all metrics in the `a-raw-metric-store` that are older than `time-ms`. That is,
  the last update time in ms of the metric value is smaller than `time-ms`."
  [a-raw-metric-store time-ms]
  (swap! a-raw-metric-store
         (fn [old-metric-store]
           (reduce-kv (fn [new-metric-store metric-key metric-value]
                        (if (< (metric-value-last-update-time-ms metric-value) time-ms)
                          new-metric-store
                          (assoc new-metric-store metric-key metric-value)))
                      {}
                      old-metric-store))))

(s/fdef get-raw-metric-sample!
  :args (s/cat :a-raw-metric-store ::metric-store
               :metric-key         ::metric-key)
  :ret  (s/nilable ::metric-sample))
(defn get-raw-metric-sample!
  "Find a raw-metric with `metric-key` (`MetricKey`) in `a-raw-metric-store`
  (`Map`) and return it as a `MetricSample`."
  [a-raw-metric-store metric-key]
  (when-let [metric-value (get @a-raw-metric-store metric-key)]
    (make-metric-sample (metric-key-name                  metric-key)
                        (metric-key-labels                metric-key)
                        (metric-value-value               metric-value)
                        (metric-value-timestamp           metric-value)
                        (metric-value-last-update-time-ms metric-value))))

(s/fdef get-raw-metric-samples!
  :args (s/cat :a-raw-metric-store ::metric-store)
  :ret  (s/coll-of ::metric-sample))
(defn get-raw-metric-samples!
  "Return all raw-metrics in `a-raw-metric-store` as `MetricSample`s."
  [a-raw-metric-store]
  (reduce-kv (fn [r metric-key metric-value]
               (concat r
                       [(make-metric-sample (metric-key-name                  metric-key)
                                            (metric-key-labels                metric-key)
                                            (metric-value-value               metric-value)
                                            (metric-value-timestamp           metric-value)
                                            (metric-value-last-update-time-ms metric-value))]))
             []
             @a-raw-metric-store))

;; COMMANDS on raw metrics

(define-record-type ^{:doc "Monadic command for setting metrics."}
  SetRawMetric
  ^:private really-set-raw-metric
  set-raw-metric?
  [metric-key   set-raw-metric-metric-key
   metric-value set-raw-metric-metric-value])

(s/def ::set-raw-metric
  (s/spec
   (partial instance? SetRawMetric)))

(s/fdef set-raw-metric
  :args (s/cat :metric-key   ::metric-key
               :metric-value ::metric-value)
  :ret ::set-raw-metric)
(defn set-raw-metric
  [metric-key metric-value]
  (really-set-raw-metric metric-key metric-value))

(define-record-type ^{:doc "Monadic command for incrementing metrics."}
  IncrementRawMetric
  ^:private really-inc-raw-metric
  inc-raw-metric?
  [metric-key   inc-raw-metric-metric-key
   metric-value inc-raw-metric-metric-value])

(s/def ::inc-raw-metric
  (s/spec
   (partial instance? IncrementRawMetric)))

(s/fdef inc-raw-metric
  :args (s/cat :metric-key   ::metric-key
               :metric-value ::metric-value)
  :ret ::inc-raw-metric)
(defn inc-raw-metric
  [metric-key metric-value]
  (really-inc-raw-metric metric-key metric-value))

(define-record-type ^{:doc "Monadic command for pruning stale metrics."}
  PruneStaleRawMetrics
  ^:private really-prune-stale-raw-metrics
  prune-stale-raw-metrics?
  [time-ms prune-stale-raw-metrics-time-ms])

(s/def ::prune-stale-raw-metrics
  (s/spec
   (partial instance? PruneStaleRawMetrics)))

(s/fdef prune-stale-raw-metrics
  :args (s/cat :time-ms ::metric-value-last-update-time-ms)
  :ret ::prune-stale-raw-metrics)
(defn prune-stale-raw-metrics
  [time-ms]
  (really-prune-stale-raw-metrics time-ms))

(define-record-type ^{:doc "Monadic command for getting a metric sample."}
  GetRawMetricSample
  ^:private really-get-raw-metric-sample
  get-raw-metric-sample?
  [metric-key get-raw-metric-sample-metric-key])

(s/def ::get-raw-metric-sample
  (s/spec
   (partial instance? GetRawMetricSample)))

(s/fdef get-raw-metric-sample
  :args (s/cat :metric-key ::metric-key)
  :ret ::get-raw-metric-sample)
(defn get-raw-metric-sample
  [metric-key]
  (really-get-raw-metric-sample metric-key))

(define-record-type ^{:doc "Monadic command for getting metric samples."}
  GetRawMetricSamples
  ^:private really-get-raw-metric-samples
  get-raw-metric-samples?
  [])

(s/def ::get-raw-metric-samples
  (s/spec
   (partial instance? GetRawMetricSamples)))

;; TODO: args empty - clean up?
(s/fdef get-raw-metric-samples
  :ret ::get-raw-metric-samples)
(defn get-raw-metric-samples
  []
  (really-get-raw-metric-samples))

(defn run-metrics
  [_run-any env state m]
  (let [a-raw-metric-store (::a-raw-metric-store env)]
    (cond
      (set-raw-metric? m)
      [(set-raw-metric! a-raw-metric-store
                        (set-raw-metric-metric-key   m)
                        (set-raw-metric-metric-value m))
       state]

      (inc-raw-metric? m)
      [(inc-raw-metric! a-raw-metric-store
                        (inc-raw-metric-metric-key   m)
                        (inc-raw-metric-metric-value m))
       state]

      (prune-stale-raw-metrics? m)
      [(prune-stale-raw-metrics! a-raw-metric-store
                                 (prune-stale-raw-metrics-time-ms m))
       state]

      (get-raw-metric-sample? m)
      [(get-raw-metric-sample! a-raw-metric-store
                               (get-raw-metric-sample-metric-key m))
       state]

      (get-raw-metric-samples? m)
      [(get-raw-metric-samples! a-raw-metric-store)
       state]

      :else
      monad/unknown-command)))

;; METRICS
;; prometheus-style:
;; - counter
;; - gauge
;; - histogram

(define-record-type ^{:doc "Counter metric with it's `help` and `metric-key`,
where `help` must be a string or nil and `metric-key` must be a `MetricKey`."}
  CounterMetric
  ^:private really-make-counter-metric
  counter-metric?
  [help counter-metric-help
   key  counter-metric-key])

(s/def ::help (s/nilable string?))

(declare make-counter-metric)

;; TODO: help and labels optional
(s/def ::counter-metric
  (s/spec
   (partial instance? CounterMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-key-name help metric-key-labels]}]
                       (make-counter-metric metric-key-name help metric-key-labels))
                       (s/gen (s/keys :req-un [::metric-key-name ::help ::metric-key-labels]))))))

;; TODO: clean up
;; - this introduces a test-library in src - should this go into test instead?
(defn gen-counter-metrics
  [num-elems]
  (s/spec (s/coll-of ::counter-metric :into [])
          :gen (fn []
                 (clojure.test.check.generators/fmap (fn [[metric-keys helps]]
                                                       (mapv (fn [metric-key help]
                                                               (make-counter-metric (metric-key-name metric-key)
                                                                                    help
                                                                                    (metric-key-labels metric-key)))
                                                             metric-keys helps))
                                                     (s/gen (s/tuple (gen-metric-keys num-elems) (s/coll-of ::help :count num-elems)))))))

(s/fdef make-counter-metric
  :args (s/cat :name ::metric-key-name
               :optional (s/? (s/cat :help   ::help
                                     :labels ::metric-key-labels)))
  :ret ::counter-metric)
(defn make-counter-metric
  [name & [help labels]]
  (let [metric-key (make-metric-key name (or labels {}))]
    (really-make-counter-metric help metric-key)))

(define-record-type ^{:doc "Gauge metric."}
  GaugeMetric
  ^:private really-make-gauge-metric
  gauge-metric?
  [help gauge-metric-help
   key  gauge-metric-key])

(declare make-gauge-metric)

;; TODO: help and labels optional
(s/def ::gauge-metric
  (s/spec
   (partial instance? GaugeMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [name gauge-metric-help metric-key-labels]}]
                       (make-gauge-metric name gauge-metric-help metric-key-labels))
                       (s/gen (s/keys :req-un [::metric-key-name ::help ::metric-key-labels]))))))

(s/fdef make-gauge-metric
  :args (s/cat :name ::metric-key-name
               :optional (s/? (s/cat :help   ::help
                                     :labels ::metric-key-labels)))
  :ret ::gauge-metric)
(defn make-gauge-metric
  [name & [help labels]]
  (let [metric-key (make-metric-key name (or labels {}))]
    (really-make-gauge-metric help metric-key)))

(define-record-type ^{:doc "Histogram metric."}
  HistogramMetric
  ^:private really-make-histogram-metric
  histogram-metric?
  [help histogram-metric-help
   threshold histogram-metric-threshold
   total-sum histogram-metric-total-sum
   bucket-le-threshold histogram-metric-bucket-le-threshold
   total-count histogram-metric-total-count
   bucket-le-inf histogram-metric-bucket-le-inf])

(declare make-histogram-metric)

;; TODO help and labels optional
(s/def ::histogram-metric
  (s/spec
   (partial instance? HistogramMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [basename histogram-metric-threshold histogram-metric-help metric-key-labels]}]
                       (make-histogram-metric basename histogram-metric-threshold histogram-metric-help metric-key-labels))
                       (s/gen (s/keys :req-un [::metric-key-name ::metric-value-value ::help ::labels]))))))

(s/fdef make-histogram-metric
  :args (s/cat :basename  ::metric-key-name
               :threshold ::metric-value-value
               :optional (s/? (s/cat :help   ::help
                                     :labels ::metric-key-labels)))
  :ret ::histogram-metric)
(defn make-histogram-metric
  [basename threshold & [help labels]]
  (let [total-sum           (make-counter-metric (str basename "_sum"   ) nil (or labels {}))
        bucket-le-threshold (make-counter-metric (str basename "_bucket") nil (assoc labels :le (str threshold)))
        total-count         (make-counter-metric (str basename "_count" ) nil (or labels {}))
        bucket-le-inf       (make-counter-metric (str basename "_bucket") nil (assoc labels :le "+Inf"))]
    (really-make-histogram-metric help threshold total-sum bucket-le-threshold total-count bucket-le-inf)))

(s/def ::metric (s/or :counter-metric   ::counter-metric
                      :gauge-metric     ::gauge-metric
                      :histogram-metric ::histogram-metric))

(s/fdef record-metric!
  :args (s/cat :a-raw-metric-store ::metric-store
               :metric             ::metric
               :metric-value       ::metric-value)
  :ret ::metric-value)
(defn record-metric!
  [a-raw-metric-store metric metric-value]
  (let [value          (metric-value-value               metric-value)
        timestamp      (metric-value-timestamp           metric-value)
        last-update    (metric-value-last-update-time-ms metric-value)
        metric-value-1 (make-metric-value 1 timestamp last-update)
        metric-value-0 (make-metric-value 0 timestamp last-update)]
    (cond
      (counter-metric? metric)
      (inc-raw-metric! a-raw-metric-store (counter-metric-key metric) metric-value)

      (gauge-metric? metric)
      (set-raw-metric! a-raw-metric-store (gauge-metric-key metric  ) metric-value)

      (histogram-metric? metric)
      (do
        (record-metric! a-raw-metric-store (histogram-metric-total-sum     metric) metric-value  )
        (record-metric! a-raw-metric-store (histogram-metric-bucket-le-inf metric) metric-value-1)
        (record-metric! a-raw-metric-store (histogram-metric-total-count   metric) metric-value-1)
        (if (<= value (histogram-metric-threshold metric))
          (record-metric! a-raw-metric-store (histogram-metric-bucket-le-threshold metric) metric-value-1)
          (record-metric! a-raw-metric-store (histogram-metric-bucket-le-threshold metric) metric-value-0))))))

;; TODO: Returns --- monad metric-value?
(s/fdef record-metric
  :args (s/cat :metric       ::metric
               :metric-value ::metric-value))
(defn record-metric
  [metric metric-value]
  (let [value          (metric-value-value               metric-value)
        timestamp      (metric-value-timestamp           metric-value)
        last-update    (metric-value-last-update-time-ms metric-value)
        metric-value-1 (make-metric-value 1 timestamp last-update)
        metric-value-0 (make-metric-value 0 timestamp last-update)]
    (cond
      (counter-metric? metric)
      (inc-raw-metric (counter-metric-key metric) metric-value)

      (gauge-metric? metric)
      (set-raw-metric (gauge-metric-key metric  ) metric-value)

      (histogram-metric? metric)
      (monad/monadic
        (record-metric (histogram-metric-total-sum     metric) metric-value  )
        (record-metric (histogram-metric-bucket-le-inf metric) metric-value-1)
        (record-metric (histogram-metric-total-count   metric) metric-value-1)
        (if (<= value (histogram-metric-threshold metric))
          (record-metric (histogram-metric-bucket-le-threshold metric) metric-value-1)
          (record-metric (histogram-metric-bucket-le-threshold metric) metric-value-0))))))

(s/fdef get-metrics!
  :args (s/cat :a-raw-metric-store ::metric-store
               :metric             ::metric)
  :ret (s/coll-of ::metric-sample))
(defn get-metrics!
  "Returns a collection of metric samples."
  [a-raw-metric-store metric]
  (cond
    (counter-metric? metric)
    [(get-raw-metric-sample! a-raw-metric-store (counter-metric-key metric))]

    (gauge-metric? metric)
    [(get-raw-metric-sample! a-raw-metric-store (gauge-metric-key metric))]

    (histogram-metric? metric)
    (mapcat (partial get-metrics! a-raw-metric-store)
            [(histogram-metric-total-sum metric)
             (histogram-metric-bucket-le-inf metric)
             (histogram-metric-total-count metric)
             (histogram-metric-bucket-le-threshold metric)])))

;; TODO: Return --- monad ::metric-sample.
(s/fdef get-metrics
  :args (s/cat :metric ::metric))
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
               (mapv get-metrics [(histogram-metric-total-sum           metric)
                                  (histogram-metric-bucket-le-inf       metric)
                                  (histogram-metric-total-count         metric)
                                  (histogram-metric-bucket-le-threshold metric)]))]
     (monad/return (apply concat metrics)))))

(s/fdef record-and-get!
  :args (s/cat :a-raw-metric-store ::metric-store
               :metric             ::metric
               :metric-value       ::metric-value)
  :ret  (s/coll-of ::metric-sample))
(defn record-and-get!
  [a-raw-metric-store metric metric-value]
  (record-metric! a-raw-metric-store metric metric-value)
  (get-metrics! a-raw-metric-store metric))

;; TODO: Return -- monad coll-of ::metric-sample
(s/fdef record-and-get
  :args (s/cat :metric       ::metric
               :metric-value ::metric-value))
(defn record-and-get
  [metric metric-value]
  (monad/monadic
    (record-metric metric metric-value)
    (get-metrics metric)))

(defn monad-command-config
  [& [a-raw-metric-store]]
  (monad/make-monad-command-config
    run-metrics
    {::a-raw-metric-store (or a-raw-metric-store (fresh-raw-metric-store))} {}))
