(ns ^:no-doc active.clojure.logger.metric-types
  "Internal."
  (:require [active.clojure.record :refer [define-record-type]]
            
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(s/def ::metric (s/or :gauge-metric     ::gauge-metric
                      :counter-metric   ::counter-metric
                      :histogram-metric ::histogram-metric))

(s/def ::metric-name string?)
(s/def ::metric-help string?)
(s/def ::metric-type #{:gauge :counter :histogram})

(s/def ::metric-labels (s/map-of keyword? any?))
(s/def ::metric-value  (s/and number? #(not (Double/isNaN %))))
(s/def ::metric-last-update-time-ms nat-int?)

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
          (sgen/fmap (fn [[metric-name metric-help]]
                       (really-make-gauge-metric metric-name metric-help))
                     (s/gen (s/tuple ::metric-name ::metric-help))))))

(s/fdef make-gauge-metric
  :args (s/cat :metric-name ::metric-name
               :metric-help ::metric-help)
  :ret ::gauge-metric)
(defn make-gauge-metric
  [metric-name metric-help]
  (really-make-gauge-metric metric-name metric-help))

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
          (sgen/fmap (fn [[metric-name metric-help]]
                       (really-make-counter-metric metric-name metric-help false))
                     (s/gen (s/tuple ::metric-name ::metric-help))))))

(s/fdef make-counter-metric
  :args (s/cat :metric-name ::metric-name
               :metric-help ::metric-help
               :optional (s/? (s/cat :set-value? boolean?)))
  :ret ::counter-metric)
(defn make-counter-metric
  [metric-name metric-help & [set-value?]]
  ;; FIXME: set-value?=false and set-value?=nil would be different metrics; that's too surprising.
  (really-make-counter-metric metric-name metric-help set-value?))

;; 3. Histograms

(s/def ::threshold (s/and number? #(not (Double/isNaN %))))
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
          (sgen/fmap (fn [[metric-name metric-help thresholds]]
                       (really-make-histogram-metric metric-name metric-help thresholds))
                     (s/gen (s/tuple ::metric-name ::metric-help ::thresholds))))))

(s/fdef make-histogram-metric
  :args (s/cat :metric-name ::metric-name
               :metric-help ::metric-help
               :thresholds  ::thresholds)
  :ret ::histogram-metric)
(defn make-histogram-metric
  [metric-name metric-help thresholds]
  (really-make-histogram-metric metric-name metric-help thresholds))

;; -----------------------------------------------------------------

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

