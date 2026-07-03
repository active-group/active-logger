(ns active.clojure.logger.metric-monad
  "Monadic commands to access the metric machinery."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.monad :as monad]

            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.time :as time]

            [clojure.spec.alpha :as s]))

;; COMMANDS on raw metrics

(define-record-type ^{:doc "Monadic command for recording a metric."}
  RecordMetric
  ^:private really-record-metric
  record-metric?
  [metric      record-metric-metric
   labels      record-metric-labels
   value       record-metric-value
   last-update record-metric-last-update])

(s/def ::record-metric
  (s/spec
   (partial instance? RecordMetric)))

(s/fdef record-metric
  :args (s/cat :metric ::metric-types/metric
               :labels ::metric-types/metric-labels
               :value  ::metric-types/metric-value
               :optional (s/? (s/cat :last-update (s/nilable ::metric-types/metric-last-update-time-ms))))
  :ret ::record-metric)
(defn record-metric
  [metric labels value & [last-update]]
  (really-record-metric metric labels value last-update))

(define-record-type ^{:doc "Monadic command for pruning stale metrics."}
  PruneStaleMetrics
  ^:private really-prune-stale-metrics
  prune-stale-metrics?
  [time-ms prune-stale-metrics-time-ms])

(s/def ::prune-stale-metrics
  (s/spec
   (partial instance? PruneStaleMetrics)))

(s/fdef prune-stale-metrics
  :args (s/cat :time-ms ::metric-types/metric-last-update-time-ms)
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

(s/def ::get-metric-samples
  (s/spec
   (partial instance? GetMetricSamples)))

(s/fdef get-metric-samples
  :args (s/cat :metric ::metric-types/metric
               :labels ::metric-types/metric-labels)
  :ret ::get-metric-samples)
(defn get-metric-samples
  [metric labels]
  (really-get-metric-samples metric labels))

(define-record-type ^{:doc "Monadic command for getting metric samples."}
  GetAllMetricSampleSets
  ^:private really-get-all-metric-sample-sets
  get-all-metric-sample-sets?
  [])

(s/def ::get-all-metric-sample-sets
  (s/spec
   (partial instance? GetAllMetricSampleSets)))

(s/fdef get-all-metric-sample-sets
  :args (s/cat)
  :ret ::get-all-metric-sample-sets)
(defn get-all-metric-sample-sets
  []
  (really-get-all-metric-sample-sets))

(defn run-metrics
  [run-any env state m]
  (cond
    (record-metric? m)
    (let [[milli-time state'] (if-let [maybe-last-update (record-metric-last-update m)]
                                [maybe-last-update state]
                                (run-any env state time/get-milli-time))]
      [(metric-accumulator/record-metric! (record-metric-metric m)
                                          (record-metric-labels m)
                                          (record-metric-value m)
                                          milli-time)
       state'])

    (get-metric-samples? m)
    [(metric-accumulator/get-metric-samples! (get-metric-samples-metric m)
                                             (get-metric-samples-labels m))
     state]

    (get-all-metric-sample-sets? m)
    [(metric-accumulator/get-all-metric-sample-sets!)
     state]

    (prune-stale-metrics? m)
    [(metric-accumulator/prune-stale-metrics! (prune-stale-metrics-time-ms m))
     state]

    :else
    monad/unknown-command))

(s/fdef record-and-get
  :args (s/cat :metric ::metric-types/metric
               :labels ::metric-types/metric-labels
               :value  ::metric-types/metric-value
               :optional (s/? (s/cat :last-update (s/nilable ::metric-types/metric-last-update-time-ms))))
  ;; TODO: Return is wrong; actually monad coll-of ::metric-sample
  :ret  (s/coll-of ::metric-samples/metric-sample))
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
