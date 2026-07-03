(ns active.clojure.logger.metric-samples
  "Types of metrics samples and sample sets"
  (:require [active.clojure.record :refer [define-record-type]]

            [active.clojure.logger.metric-types :as metric-types]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))


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
          (sgen/fmap (fn [[metric-name
                           metric-labels
                           metric-value-value
                           metric-value-last-update-time-ms]]
                       (really-make-metric-sample metric-name
                                                  metric-labels
                                                  metric-value-value
                                                  metric-value-last-update-time-ms))
                     (s/gen (s/tuple ::metric-types/metric-name
                                     ::metric-types/metric-labels
                                     ::metric-types/metric-value
                                     ::metric-types/metric-last-update-time-ms))))))

(s/fdef make-metric-sample
  :args (s/cat :name      ::metric-types/metric-name
               :labels    ::metric-types/metric-labels
               :value     ::metric-types/metric-value
               :timestamp ::metric-types/metric-last-update-time-ms)
  :ret ::metric-sample)
(defn make-metric-sample
  [name labels value timestamp]
  (really-make-metric-sample name labels value timestamp))


(define-record-type ^{:doc "Metric sample set."}
  MetricSampleSet
  ^:private really-make-metric-sample-set
  metric-sample-set?
  [name metric-sample-set-name
   type metric-sample-set-type
   help metric-sample-set-help
   samples metric-sample-set-samples])

(s/def ::metric-sample-set (s/spec (partial instance? MetricSampleSet)))

(s/fdef make-metric-sample-set
  :args (s/cat :name        ::metric-types/metric-name
               :type        ::metric-types/metric-type
               :help        ::metric-types/metric-help
               :samples     (s/coll-of ::metric-sample))
  :ret ::metric-sample-set)
(defn make-metric-sample-set
  [name type help samples]
  (really-make-metric-sample-set name type help samples))
