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
               :count-value ::metric-types/metric-value
               :bucket-values ::bucket-values)
  :ret ::histogram-metric-values)
(defn make-histogram-metric-values
  [last-update-time-ms sum-value count-value bucket-values]
  (really-make-histogram-metric-values last-update-time-ms sum-value count-value bucket-values))

(s/fdef make-empty-histogram-metric-values
  :args (s/cat :thresholds ::metric-types/thresholds)
  :ret ::histogram-metric-values)
(defn- make-empty-histogram-metric-values
  "Creates an empty histogram."
  [thresholds]
  ;; FIXME: I assume the sum should be 0.0 and the count 0 - no vica-versa.
  (make-histogram-metric-values 0 0.0 0.0 (vec (repeat (count thresholds) 0.0))))

(s/fdef update-histogram-values
  :args (s/cat :histogram-metric-values ::histogram-metric-values
               :thresholds ::metric-types/thresholds
               :metric-value     ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::histogram-metric-values)
(defn update-histogram-metric-values
  "Adds a value to a given histogram."
  [histogram-metric-values thresholds value last-update-time-ms]
  ;; OPT: construct a new record only once.
  (-> (or histogram-metric-values (make-empty-histogram-metric-values thresholds))
      (histogram-metric-values-last-update-time-ms last-update-time-ms)
      (lens/overhaul histogram-metric-values-sum-value + value)
      (lens/overhaul histogram-metric-values-count-value + 1.0) ;; counts are doubles?
      (lens/overhaul histogram-metric-values-bucket-values
                     #(mapv (fn [threshold bucket-value]
                              (if (<= value threshold)
                                (+ bucket-value 1.0)
                                bucket-value))
                            thresholds %))))
