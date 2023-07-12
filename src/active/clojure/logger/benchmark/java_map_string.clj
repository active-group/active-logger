(ns active.clojure.logger.benchmark.java-map-string
  "Java-map-Implementation for metric-store-map."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]
            [active.clojure.monad :as monad]

            [active.clojure.logger.time :as time]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.string :as str])
  (:import (java.util.function BiFunction)))

(def who-am-i "java-map-string-implementation")

;; (s/check-asserts true)

;; ::metric-store-maps stores contains all known metrics and their stored values

;; #_(s/def ::metric-store-map (s/map-of ::metric ::stored-values))
#_(s/def ::metric-store-map any?)

;; ADJUST: metric-store-map
;; #_(s/fdef fresh-metric-store-map
;;   :ret ::metric-store-map)
(defn ^:no-doc fresh-metric-store-map
  []
  (java.util.HashMap. {}))

;; TODO: Can we improve the type of the metric-store?
#_(s/def ::metric-store (partial instance? clojure.lang.Atom))

#_(s/fdef fresh-metric-store
  :ret ::metric-store)
(defn ^:no-doc fresh-metric-store
  []
  (atom (fresh-metric-store-map)))

(defonce metric-store (fresh-metric-store))

#_(s/fdef set-global-metric-store!
  :args (s/cat :fresh-metric-store-map ::metric-store-map)
  :ret nil)
(defn set-global-metric-store!
  [fresh-metric-store-map]
  (reset! metric-store fresh-metric-store-map)
  nil)

#_(s/fdef reset-global-metric-store!
  :args (s/cat)
  :ret nil)
(defn reset-global-metric-store!
  []
  (reset! metric-store (fresh-metric-store-map))
  nil)

(declare make-metric-sample)

;; Maps from labels to value hold the current state of the metric
;; These maps are stored in ::stored-values

#_(s/def ::labels-value-map (s/map-of ::metric-labels ::metric-value))

;; ADJUST: labels-value-map
(def ^:const empty-values-map {})

#_(s/def ::metric-labels (s/map-of keyword? any?))

(define-record-type ^{:doc "Metric value is a combination of the `value` itself
  and the `last-update-time-ms` of the value."}
  MetricValue
  ^:private really-make-metric-value
  metric-value?
  [value               metric-value-value
   last-update-time-ms metric-value-last-update-time-ms])

;; TODO: maybe better counter-metric-value and gauge-metric-value?
#_(s/def ::metric-value-value number?)
#_(s/def ::metric-value-value-double double?)
;; https://prometheus.io/docs/instrumenting/writing_exporters/
;; "You should not set timestamps on the metrics you expose, let Prometheus
;; take care of that."
#_(s/def ::metric-value-last-update-time-ms nat-int?)

(declare make-metric-value)
#_(s/def ::metric-value
  (s/spec
   (partial instance? MetricValue)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-value-value-double metric-value-last-update-time-ms]}]
                       (make-metric-value metric-value-value-double metric-value-last-update-time-ms))
                     (s/gen (s/keys :req-un [::metric-value-value-double ::metric-value-last-update-time-ms]))))))

;; stores value as double
#_(s/fdef make-metric-value
  :args (s/cat :value       ::metric-value-value
               :update-time ::metric-value-last-update-time-ms)
  :ret  ::metric-value)
(defn make-metric-value
  [value update-time]
  (really-make-metric-value (double value) update-time))

;; Primitives on `labels-value-map`s

#_(s/fdef set-metric-value
  :args (s/cat :labels-value-map ::labels-value-map
               :metric-labels            ::metric-labels
               :metric-value             ::metric-value)
  :ret ::labels-value-map)
(defn set-metric-value
  "Set a metric-value within a labels-value-map."
  [labels-value-map metric-labels metric-value]
  ;; ADJUST: assoc-labels-value-map
  (assoc labels-value-map metric-labels metric-value))

#_(s/fdef update-metric-value
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

#_(s/fdef inc-metric-value
  :args (s/cat :labels-value-map ::labels-value-map
               :metric-labels            ::metric-labels
               :metric-value             ::metric-value)
  :ret ::labels-value-map)
(defn inc-metric-value
  "Increment a metric-value within a labels-value-map."
  [labels-value-map metric-labels metric-value-2]
  ;; ADJUST: update-labels-value-map
  (update labels-value-map metric-labels
          (fn [metric-value-1]
            (update-metric-value + metric-value-1 metric-value-2))))

#_(s/fdef prune-stale-metric-value
  :args (s/cat :labels-value-map ::labels-value-map
               :time-ms          ::metric-value-last-update-time-ms)
  :ret ::labels-value-map)
(defn prune-stale-metric-value
  [labels-value-map time-ms]
  (reduce-kv (fn [new-labels-value-map metric-labels metric-value]
               (if (< (metric-value-last-update-time-ms metric-value) time-ms)
                 new-labels-value-map
                 ;; ADJUST: assoc-labels-value-map
                 (assoc new-labels-value-map metric-labels metric-value)))
             {} ;; ADJUST: metric-value-map --- empty-metric-value?
             labels-value-map))

#_(s/def ::metric (s/or :gauge-metric     ::gauge-metric
                      :counter-metric   ::counter-metric
                      :histogram-metric ::histogram-metric))

#_(s/def ::stored-values (s/or :gauge     ::gauge-values
                             :counter   ::counter-values
                             :histogram ::histogram-values))

#_(s/def ::metric-name string?)
#_(s/def ::metric-help string?)


;; 1. Gauges

(define-record-type ^{:doc "Gauge metric"}
  GaugeMetric
  ^:private really-make-gauge-metric
  gauge-metric?
  [name gauge-metric-name
   help gauge-metric-help])

(declare make-gauge-metric)
#_(s/def ::gauge-metric
  (s/spec
   (partial instance? GaugeMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name metric-help]}]
                       (make-gauge-metric metric-name metric-help))
                     (s/gen (s/keys :req-un [::metric-name ::metric-help]))))))

#_(s/fdef make-gauge-metric
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

#_(s/def ::gauge-values (s/spec (partial instance? GaugeValues)))

#_(s/fdef make-gauge-values
  :args (s/cat)
  :ret ::gauge-values)
(defn make-gauge-values
  []
  (really-make-gauge-values empty-values-map))

#_(s/fdef update-gauge-values
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

#_(s/fdef gauge-values->metric-samples
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

#_(s/fdef prune-stale-gauge-values
  :args (s/cat :gauge-values ::gauge-values
               :time-ms      ::metric-value-last-update-time-ms)
  :ret ::gauge-values)
(defn prune-stale-gauge-values
  [gauge-values time-ms]
  (lens/overhaul gauge-values gauge-values-map prune-stale-metric-value time-ms))

#_(s/fdef empty-gauge-values?
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

(declare make-counter-metric)
#_(s/def ::counter-metric
  (s/spec
   (partial instance? CounterMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name metric-help]}]
                       (make-counter-metric metric-name metric-help))
                     (s/gen (s/keys :req-un [::metric-name ::metric-help]))))))

#_(s/fdef make-counter-metric
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

#_(s/def ::counter-values (s/spec (partial instance? CounterValues)))

#_(s/fdef make-counter-values
  :args (s/cat)
  :ret ::counter-values)
(defn make-counter-values
  []
  (really-make-counter-values empty-values-map))

#_(s/fdef update-counter-values
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

#_(s/fdef counter-values->metric-samples
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

#_(s/fdef prune-stale-counter-values
  :args (s/cat :counter-values ::counter-values
               :time-ms        ::metric-value-last-update-time-ms)
  :ret ::counter-values)
(defn prune-stale-counter-values
  [counter-values time-ms]
  (lens/overhaul counter-values counter-values-map prune-stale-metric-value time-ms))

#_(s/fdef empty-counter-values?
  :args (s/cat :counter-values ::counter-values)
  :ret boolean?)
(defn empty-counter-values?
  [counter-values]
  (empty? (counter-values-map counter-values)))

;; 3. Histograms

#_(s/def ::threshold (s/get-spec ::metric-value-value))
#_(s/def ::thresholds (s/coll-of ::threshold))

(define-record-type ^{:doc "Histogram metric"}
  HistogramMetric
  ^:private really-make-histogram-metric
  histogram-metric?
  [name       histogram-metric-name
   help       histogram-metric-help
   thresholds histogram-metric-thresholds])

(declare make-histogram-metric)
#_(s/def ::histogram-metric
  (s/spec
   (partial instance? HistogramMetric)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name metric-help thresholds]}]
                       (make-histogram-metric metric-name metric-help thresholds))
                     (s/gen (s/keys :req-un [::metric-name ::metric-help ::thresholds]))))))

#_(s/fdef make-histogram-metric
  :args (s/cat :metric-name ::metric-name
               :metric-help ::metric-help
               :thresholds  ::thresholds)
  :ret ::histogram-metric)
(defn make-histogram-metric
  [metric-name metric-help thresholds]
  (really-make-histogram-metric metric-name metric-help thresholds))

(define-record-type ^{:doc "Stored Histogram values, i.e. a threshold and three
  maps (sum, count, bucket) from labels to metric-values."}
  HistogramValues
  ^:private really-make-histogram-values
  histogram-values?
  [thresholds histogram-values-thresholds
   sum-map histogram-values-sum-map
   count-map histogram-values-count-map
   bucket-maps histogram-values-bucket-maps])

#_(s/def ::histogram-values (s/spec (partial instance? HistogramValues)))

#_(s/fdef make-histogram-values
  :args (s/cat :thresholds ::thresholds)
  :ret ::histogram-values)
(defn make-histogram-values
  [thresholds]
  (really-make-histogram-values thresholds empty-values-map empty-values-map (vec (repeat (count thresholds) empty-values-map))))

(make-histogram-values [1])

#_(s/fdef update-histogram-values
  :args (s/cat :histogram-values ::histogram-values
               :metric-labels    ::metric-labels
               :metric-value     ::metric-value)
  :ret ::histogram-values)
(defn update-histogram-values
  "Updates a `HistogramMetric`."
  [histogram-values metric-labels metric-value]
  (let [last-update         (metric-value-last-update-time-ms metric-value)
        value-value         (metric-value-value               metric-value)
        thresholds          (histogram-values-thresholds histogram-values)
        metric-value-0      (make-metric-value 0 last-update)
        metric-value-1      (make-metric-value 1 last-update)]
    (-> histogram-values
        (lens/overhaul histogram-values-sum-map
                       inc-metric-value metric-labels metric-value)
        (lens/overhaul histogram-values-count-map
                       inc-metric-value metric-labels metric-value-1)
        (lens/overhaul histogram-values-bucket-maps
                       #(mapv (fn [threshold bucket-map]
                                (inc-metric-value bucket-map metric-labels
                                                  (if (<= value-value threshold)
                                                    metric-value-1
                                                    metric-value-0)))
                              thresholds %)))))

#_(s/fdef histogram-values->metric-samples
  :args (s/cat :basename ::metric-name
               :histogram-values ::histogram-values
               :metric-labels ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn histogram-values->metric-samples
  "Return all metric-samples with the given labels within this histogram-metric."
  [basename histogram-values metric-labels]
  (let [thresholds  (histogram-values-thresholds histogram-values)
        sum-map     (histogram-values-sum-map histogram-values)
        count-map   (histogram-values-count-map histogram-values)
        bucket-maps (histogram-values-bucket-maps histogram-values)
        ;; TODO: do we trust that it is always in all three maps?
        metric-value-sum    (get sum-map metric-labels)
        metric-value-count  (get count-map metric-labels)]
    (concat
      (when (metric-value? metric-value-sum)
        [(make-metric-sample (str basename "_sum")
                             metric-labels
                             (metric-value-value               metric-value-sum)
                             (metric-value-last-update-time-ms metric-value-sum))])
      (when (metric-value? metric-value-count)
        [(make-metric-sample (str basename "_count")
                           metric-labels
                           (metric-value-value               metric-value-count)
                           (metric-value-last-update-time-ms metric-value-count))
         (make-metric-sample (str basename "_bucket")
                             ;; ADJUST: assoc-metric-labels-map
                             (assoc metric-labels :le "+Inf")
                             (metric-value-value               metric-value-count)
                             (metric-value-last-update-time-ms metric-value-count))])
      (mapcat (fn [threshold bucket-map]
                (let [metric-value-bucket (get bucket-map metric-labels)]
                  (when (metric-value? metric-value-bucket)
                    [(make-metric-sample (str basename "_bucket")
                                         ;; ADJUST: assoc-metric-labels-map
                                         (assoc metric-labels :le (str threshold))
                                         (metric-value-value               metric-value-bucket)
                                         (metric-value-last-update-time-ms metric-value-bucket))])))
              thresholds bucket-maps))))

#_(s/fdef prune-stale-histogram-values
  :args (s/cat :histogram-values ::histogram-values
               :time-ms          ::metric-value-last-update-time-ms)
  :ret ::histogram-values)
(defn prune-stale-histogram-values
  [histogram-values time-ms]
  (-> histogram-values
      (lens/overhaul histogram-values-sum-map prune-stale-metric-value time-ms)
      (lens/overhaul histogram-values-count-map prune-stale-metric-value time-ms)
      (lens/overhaul histogram-values-bucket-maps
                     #(mapv (fn [bucket-map] (prune-stale-metric-value bucket-map time-ms)) %))))

#_(s/fdef empty-histogram-values?
  :args (s/cat :histogram-values ::histogram-values)
  :ret boolean?)
(defn empty-histogram-values?
  [histogram-values]
  (empty? (histogram-values-sum-map histogram-values)))

;; Primitives on stored values

#_(s/fdef update-stored-values
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

#_(s/fdef make-stored-values
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

#_(s/fdef prune-stale-stored-values
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

#_(s/fdef empty-stored-values?
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

;; Metrics samples and sample sets

(define-record-type ^{:doc "Metric sample."}
  MetricSample
  ^:private really-make-metric-sample
  metric-sample?
  [name      metric-sample-name
   labels    metric-sample-labels
   value     metric-sample-value
   timestamp metric-sample-timestamp])

#_(s/def ::metric-sample
  (s/spec
   (partial instance? MetricSample)
   :gen (fn []
          (sgen/fmap (fn [{:keys [metric-name
                                  metric-labels
                                  metric-value-value
                                  metric-value-last-update-time-ms]}]
                       (make-metric-sample metric-name
                                           metric-labels
                                           metric-value-value
                                           metric-value-last-update-time-ms))
                     (s/gen (s/keys :req-un [::metric-name
                                             ::metric-labels
                                             ::metric-value-value
                                             ::metric-value-last-update-time-ms]))))))

#_(s/fdef make-metric-sample
  :args (s/cat :name      ::metric-name
               :labels    ::metric-labels
               :value     ::metric-value-value
               :timestamp ::metric-value-last-update-time-ms)
  :ret ::metric-sample)
(defn make-metric-sample
  [name labels value timestamp]
  (really-make-metric-sample name labels value timestamp))

;; -----------------------------------------------------------------

;; ADJUST: metric-store-map
(defn adjust-metric-representation
  [metric]
  (cond
    (gauge-metric?     metric) (str ":gauge - not tested")
    (counter-metric?   metric) (str ":counter:"
                                    (counter-metric-name metric) ":"
                                    (counter-metric-help metric) ":"
                                    (counter-metric-set-value? metric))
    (histogram-metric? metric) (str ":histogram - not tested")))

;; ADJUST: update-metric-store-map
(defn f-to-Bifunction
  [f]
  (reify BiFunction
    (apply [this arg1 arg2] (f arg1 arg2))))

(defn update-metric-store-map
  [metric-store-map metric-store-key metric-labels metric-value]
  (do (. metric-store-map compute (adjust-metric-representation metric-store-key)
         (f-to-Bifunction (fn [mkey maybe-stored-values]
                            (if (some? maybe-stored-values)
                              (update-stored-values maybe-stored-values metric-labels metric-value)
                              (make-stored-values metric-store-key metric-labels metric-value))))) metric-store-map))

#_(s/fdef record-metric-1
  :args (s/cat :metric-store ::metric-store-map
               :metric       ::metric
               :labels       ::metric-labels
               :value        ::metric-value)
  :ret ::metric-store-map)
(defn record-metric-1
  [metric-store metric metric-labels metric-value]
  (update-metric-store-map metric-store
                           metric
                           metric-labels
                           metric-value))

#_(s/fdef record-metric!
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

#_(s/fdef stored-value->metric-samples
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

#_(s/fdef stored-value->all-metric-samples
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
            (keys (histogram-values-sum-map stored-value)))))

;; ADJUST: metric-store-map?
(defn get-metric-store-map
  [metric-store-map metric]
  (get metric-store-map (adjust-metric-representation metric)))

#_(s/fdef get-metric-samples-1
  :args (s/cat :metric-store  ::metric-store-map
               :metric        ::metric
               :metric-labels ::metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn get-metric-samples-1
  [metric-store metric metric-labels]
  ;; ADJUST: metric-store-map
  (if-let [stored-value (get-metric-store-map metric-store metric)]
    (stored-value->metric-samples metric stored-value metric-labels)
    []))

#_(s/fdef get-metric-samples!
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

#_(s/def ::metric-sample-set (s/spec (partial instance? MetricSampleSet)))
#_(s/def ::metric-type #{:gauge :counter :histogram})

#_(s/fdef make-metric-sample-set
  :args (s/cat :name        ::metric-name
               :type ::metric-type
               :help        ::metric-help
               :samples     (s/coll-of ::metric-sample))
  :ret ::metric-sample-set)
(defn make-metric-sample-set
  [name type help samples]
  (really-make-metric-sample-set name type help samples))

#_(s/fdef metric-type
  :args (s/cat :metric ::metric)
  :ret ::metric-type)
(defn metric-type
  [metric]
  (cond
    (gauge-metric?     metric) :gauge
    (counter-metric?   metric) :counter
    (histogram-metric? metric) :histogram))

#_(s/fdef metric-name
  :args (s/cat :metric ::metric)
  :ret ::metric-name)
(defn metric-name
  [metric]
  (cond
    (gauge-metric?     metric) (gauge-metric-name     metric)
    (counter-metric?   metric) (counter-metric-name   metric)
    (histogram-metric? metric) (histogram-metric-name metric)))

#_(s/fdef metric-help
  :args (s/cat :metric ::metric)
  :ret ::metric-help)
(defn metric-help
  [metric]
  (cond
    (gauge-metric?     metric) (gauge-metric-help     metric)
    (counter-metric?   metric) (counter-metric-help   metric)
    (histogram-metric? metric) (histogram-metric-help metric)))

#_(s/fdef get-metric-sample-set-1
  :args (s/cat :metric-store ::metric-store-map
               :metric       ::metric)
  :ret ::metric-sample-set)
(defn get-metric-sample-set-1
  [metric-store metric]
  ;; ADJUST: metric-store-map
  (if-let [stored-value (get-metric-store-map metric-store metric)]
    (make-metric-sample-set (metric-name metric)
                            (metric-type metric)
                            (metric-help metric)
                            (stored-value->all-metric-samples metric stored-value))
    []))

#_(s/fdef get-all-metric-sample-sets-1
  :args (s/cat :metric-store ::metric-store-map)
  :ret (s/coll-of (s/coll-of ::metric-sample-set)))
(defn get-all-metric-sample-sets-1
  "Return all metric-samples-sets within the given metric-store."
  [metric-store]
  (mapv (fn [metric] (get-metric-sample-set-1 metric-store metric))
        (keys metric-store)))

#_(s/fdef get-all-metric-sample-sets!
  :args (s/cat :optional (s/? (s/cat :a-metric-store ::metric-store)))
  :ret (s/coll-of (s/coll-of ::metric-sample-set)))
(defn get-all-metric-sample-sets!
  "Return all metric-samples-sets within the given metric-store.
  If no metric-store is given, use the global metric store."
  ([]
   (get-all-metric-sample-sets! metric-store))
  ([a-metric-store]
   (get-all-metric-sample-sets-1 @a-metric-store)))

;; ADJUST: assoc-metric-store-map
(defn assoc-metric-store-map
  [metric-store-map metric-store-key value]
  (assoc metric-store-map metric-store-key value))

#_(s/fdef prune-stale-metrics!
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
                             (assoc-metric-store-map new-metric-store metric new-stored-values))))
                       ;; {} ;; ADJUST: metric-store-map --- fresh-metric-store-map?
                       (fresh-metric-store-map)
                       old-metric-store)))
   nil))

(defn start-prune-stale-metrics-thread!
  "Start a thread that prunes stale metrics older than `stale-seconds` seconds
  every `every-seconds` seconds.  If called without an argument, it prunes
  metrics older than 24h every 5m."
  [& [stale-seconds every-seconds]]
  (let [stale-milliseconds (* (or stale-seconds (* 5 60)) 1000)
        every-milliseconds (* (or every-seconds (* 24 60 60)) 1000)]
    (doto (Thread.
           (fn []
             (loop []
               (let [now (time/get-milli-time!)
                     earlier (- now stale-milliseconds)]
                 (prune-stale-metrics! earlier)
                 (Thread/sleep every-milliseconds)
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

#_(s/def ::record-metric
  (s/spec
   (partial instance? RecordMetric)))

#_(s/fdef record-metric
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

#_(s/def ::prune-stale-metrics
  (s/spec
   (partial instance? PruneStaleMetrics)))

#_(s/fdef prune-stale-metrics
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

#_(s/def ::get-metric-samples
  (s/spec
   (partial instance? GetMetricSamples)))

#_(s/fdef get-metric-samples
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

#_(s/def ::get-all-metric-sample-sets
  (s/spec
   (partial instance? GetAllMetricSampleSets)))

#_(s/fdef get-all-metric-sample-sets
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

#_(s/fdef record-and-get!
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
#_(s/fdef record-and-get
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
