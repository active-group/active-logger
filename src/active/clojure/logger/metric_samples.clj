(ns active.clojure.logger.metric-samples
  "Types of metrics samples and sample sets"
  (:require [active.clojure.record :refer [define-record-type]]

            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-values :as metric-values]
            [active.clojure.logger.metric-singular-value :as singular]
            [active.clojure.logger.metric-histogram-value :as histogram]
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

;; ------------------------------------------------------------

(s/fdef singular->metric-samples
  :args (s/cat :metric-value ::singular/metric-value
               :name         ::metric-types/metric-name
               :labels       ::metric-types/metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn- singular->metric-samples [metric-value name labels]
  [(make-metric-sample name
                       labels
                       (singular/metric-value-value               metric-value)
                       (singular/metric-value-last-update-time-ms metric-value))])

(s/fdef histogram->metric-samples
  :args (s/cat :histogram-metric-values ::histogram/histogram-metric-values
               :basename                ::metric-types/metric-name
               :thresholds              ::metric-types/thresholds
               :labels                  ::metric-types/metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn- histogram->metric-samples [histogram-metric-values basename thresholds labels]
  (let [last-update-time-ms (histogram/histogram-metric-values-last-update-time-ms histogram-metric-values)
        sum (histogram/histogram-metric-values-sum-value histogram-metric-values)
        count (histogram/histogram-metric-values-count-value histogram-metric-values)
        buckets (histogram/histogram-metric-values-bucket-values histogram-metric-values)]
    (concat
     [(make-metric-sample (str basename "_sum")
                          labels
                          sum
                          last-update-time-ms)
      (make-metric-sample (str basename "_count")
                          labels
                          (double count)
                          last-update-time-ms)
      (make-metric-sample (str basename "_bucket")
                          (assoc labels :le "+Inf")
                          (double count)
                          last-update-time-ms)]
     (mapcat (fn [threshold bucket]
               [(make-metric-sample (str basename "_bucket")
                                    (assoc labels :le (str threshold))
                                    (double bucket)
                                    last-update-time-ms)])
             thresholds buckets))))

;; -----------------------------------------------------------------

(s/fdef snapshot->metric-samples
  :args (s/cat :metric        ::metric-types/metric
               :snapshot      ::metric-values/snapshot
               :metric-labels ::metric-types/metric-labels)
  :ret (s/coll-of ::metric-sample))
(defn snapshot->metric-samples
  [metric snapshot metric-labels]
  (cond
    (singular/metric-value? snapshot)
    (singular->metric-samples snapshot (metric-types/metric-name metric) metric-labels)
    (histogram/histogram-metric-values? snapshot)
    (histogram->metric-samples snapshot (metric-types/metric-name metric) (metric-types/histogram-metric-thresholds metric) metric-labels)))

(s/fdef stored-value-snapshots->all-metric-samples
  :args (s/cat :metric        ::metric-types/metric
               :all-snapshots (s/map-of ::metric-types/metric-labels ::metric-values/snapshot))
  :ret (s/coll-of ::metric-sample))
(defn all-snapshots->all-metric-samples
  [metric all-snapshots]
  (mapcat (fn [[labels value]]
            (snapshot->metric-samples metric value labels))
          all-snapshots))

