(ns ^:no-doc active.clojure.logger.metric-histogram-value
  "Internal.

  Stores a histogram of metric values, together with a last udpate time."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]
            [active.clojure.logger.metric-types :as metric-types]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

(define-record-type ^{:doc "Stored Histogram metric values."}
  HistogramMetricValues
  ^:private really-make-histogram-metric-values
  histogram-metric-values?
  [last-update-time-ms histogram-metric-values-last-update-time-ms
   sum-value histogram-metric-values-sum-value
   count-value histogram-metric-values-count-value
   bucket-values histogram-metric-values-bucket-values])

(s/def ::metric-value-double (s/and double? #(not (Double/isNaN %))))
(s/def ::bucket-values (s/coll-of nat-int?))

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
                     (s/gen (s/tuple ::metric-types/metric-last-update-time-ms ::metric-types/metric-double-value nat-int?
                                     ::bucket-values))))))

(s/fdef make-histogram-metric-values
  :args (s/cat :last-update-time-ms ::metric-types/metric-last-update-time-ms
               :sum-value ::metric-types/metric-value
               :count-value nat-int?
               :bucket-values ::bucket-values)
  :ret ::histogram-metric-values)
(defn make-histogram-metric-values
  [last-update-time-ms sum-value count-value bucket-values]
  (really-make-histogram-metric-values last-update-time-ms (double sum-value) count-value bucket-values))

(s/fdef update-histogram-values
  :args (s/cat :histogram-metric-values ::histogram-metric-values
               :thresholds       ::metric-types/thresholds
               :metric-value     ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::histogram-metric-values)
(defn update-histogram-metric-values
  "Adds a value to a given histogram."
  [histogram-metric-values thresholds value increase last-update-time-ms]
  (make-histogram-metric-values last-update-time-ms
                                (+ (histogram-metric-values-sum-value histogram-metric-values)
                                   (* increase value))
                                (+ increase (histogram-metric-values-count-value histogram-metric-values))
                                (mapv (fn [threshold bucket-value]
                                        (if (<= value threshold)
                                          (+ increase bucket-value)
                                          bucket-value))
                                      thresholds
                                      (histogram-metric-values-bucket-values histogram-metric-values))))

(s/fdef fresh-histogram-metric-values
  :args (s/cat :thresholds       ::metric-types/thresholds
               :metric-value     (s/or :value ::metric-types/metric-value :value-increase ::metric-types/histogram-value-increase)
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::histogram-metric-values)
(defn fresh-histogram-metric-values
  "Create a fresh histogram from a recorded value."
  [thresholds value last-update-time-ms]
  (let [[value increase]
        (if (metric-types/histogram-value-increase? value)
          [(metric-types/histogram-value-increase-value value)
           (metric-types/histogram-value-increase-increase value)]
          [value 1])]
    (make-histogram-metric-values last-update-time-ms
                                  (* increase value)
                                  increase
                                  (mapv (fn [threshold]
                                          (if (<= value threshold)
                                            increase
                                            0))
                                        thresholds))))
