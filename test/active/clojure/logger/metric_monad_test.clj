(ns active.clojure.logger.metric-monad-test
  (:require [active.clojure.logger.metric-monad :as m]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-accumulator-test :as util]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-samples :as metric-samples]

            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]
            
            [clojure.test :as t]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest])
  (:use [active.quickcheck]))

(t/use-fixtures :once
  (fn [f]
    (stest/instrument)
    (s/check-asserts true)
    (f)
    (s/check-asserts false)
    (stest/unstrument)))

(t/deftest t-record-metric
  (t/testing "Creating the `RecordMetric` record type works."
    (t/is (quickcheck
           (property [names       (spec (util/gen-distinct-metric-names  3))
                      helps       (spec (util/gen-metric-helps           3))
                      threshold   (spec ::metric-types/threshold)
                      labels      (spec ::metric-types/metric-labels)
                      value-value (spec ::metric-types/metric-value)
                      update-time (spec ::metric-types/metric-last-update-time-ms)]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])

                           example-gauge-recorded-metric-0 (m/record-metric example-gauge-metric labels value-value)
                           example-gauge-recorded-metric-1 (m/record-metric example-gauge-metric labels value-value update-time)

                           example-counter-recorded-metric-0 (m/record-metric example-counter-metric labels value-value)
                           example-counter-recorded-metric-1 (m/record-metric example-counter-metric labels value-value update-time)

                           example-histogram-recorded-metric-0 (m/record-metric example-histogram-metric labels value-value)
                           example-histogram-recorded-metric-1 (m/record-metric example-histogram-metric labels value-value update-time)]

                       (t/is (m/record-metric? example-gauge-recorded-metric-0))
                       (t/is (m/record-metric? example-gauge-recorded-metric-1))

                       (t/is (m/record-metric? example-counter-recorded-metric-0))
                       (t/is (m/record-metric? example-counter-recorded-metric-1))

                       (t/is (m/record-metric? example-histogram-recorded-metric-0))
                       (t/is (m/record-metric? example-histogram-recorded-metric-1))

                       (t/is (= example-gauge-metric (m/record-metric-metric example-gauge-recorded-metric-0)))
                       (t/is (= example-gauge-metric (m/record-metric-metric example-gauge-recorded-metric-1)))

                       (t/is (= example-counter-metric (m/record-metric-metric example-counter-recorded-metric-0)))
                       (t/is (= example-counter-metric (m/record-metric-metric example-counter-recorded-metric-1)))

                       (t/is (= example-histogram-metric (m/record-metric-metric example-histogram-recorded-metric-0)))
                       (t/is (= example-histogram-metric (m/record-metric-metric example-histogram-recorded-metric-1)))

                       (t/is (= labels (m/record-metric-labels example-gauge-recorded-metric-0)))
                       (t/is (= labels (m/record-metric-labels example-gauge-recorded-metric-1)))

                       (t/is (= labels (m/record-metric-labels example-counter-recorded-metric-0)))
                       (t/is (= labels (m/record-metric-labels example-counter-recorded-metric-1)))

                       (t/is (= labels (m/record-metric-labels example-histogram-recorded-metric-0)))
                       (t/is (= labels (m/record-metric-labels example-histogram-recorded-metric-1)))

                       (t/is (= value-value (m/record-metric-value example-gauge-recorded-metric-0)))
                       (t/is (= value-value (m/record-metric-value example-gauge-recorded-metric-1)))

                       (t/is (= value-value (m/record-metric-value example-counter-recorded-metric-0)))
                       (t/is (= value-value (m/record-metric-value example-counter-recorded-metric-1)))

                       (t/is (= value-value (m/record-metric-value example-histogram-recorded-metric-0)))
                       (t/is (= value-value (m/record-metric-value example-histogram-recorded-metric-1)))

                       (t/is (= nil         (m/record-metric-last-update example-gauge-recorded-metric-0)))
                       (t/is (= update-time (m/record-metric-last-update example-gauge-recorded-metric-1)))

                       (t/is (= nil         (m/record-metric-last-update example-counter-recorded-metric-0)))
                       (t/is (= update-time (m/record-metric-last-update example-counter-recorded-metric-1)))

                       (t/is (= nil         (m/record-metric-last-update example-histogram-recorded-metric-0)))
                       (t/is (= update-time (m/record-metric-last-update example-histogram-recorded-metric-1)))))))))

(t/deftest t-prune-stale-metrics
  (t/testing "Creating the `PruneStaleMetrics` record type works."
    (t/is (quickcheck
           (property [update-time (spec ::metric-types/metric-last-update-time-ms)]
                     (let [example-prune-stale-metrics  (m/prune-stale-metrics update-time)]

                       (t/is                (m/prune-stale-metrics?        example-prune-stale-metrics))
                       (t/is (= update-time (m/prune-stale-metrics-time-ms example-prune-stale-metrics)))))))))

(t/deftest t-get-metric-samples
  (t/testing "Creating the `GetMetricSamples` record type works."
    (t/is (quickcheck
           (property [names       (spec (util/gen-distinct-metric-names  3))
                      helps       (spec (util/gen-metric-helps           3))
                      threshold   (spec ::metric-types/threshold)
                      labels      (spec ::metric-types/metric-labels)]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])

                           example-gauge-get-metric-samples     (m/get-metric-samples example-gauge-metric labels)
                           example-counter-get-metric-samples   (m/get-metric-samples example-counter-metric labels)
                           example-histogram-get-metric-samples (m/get-metric-samples example-histogram-metric labels)]

                       (t/is (m/get-metric-samples? example-gauge-get-metric-samples))
                       (t/is (m/get-metric-samples? example-counter-get-metric-samples))
                       (t/is (m/get-metric-samples? example-histogram-get-metric-samples))

                       (t/is (= example-gauge-metric     (m/get-metric-samples-metric example-gauge-get-metric-samples)))
                       (t/is (= example-counter-metric   (m/get-metric-samples-metric example-counter-get-metric-samples)))
                       (t/is (= example-histogram-metric (m/get-metric-samples-metric example-histogram-get-metric-samples)))

                       (t/is (= labels (m/get-metric-samples-labels example-gauge-get-metric-samples)))
                       (t/is (= labels (m/get-metric-samples-labels example-counter-get-metric-samples)))
                       (t/is (= labels (m/get-metric-samples-labels example-histogram-get-metric-samples)))))))))

(t/deftest t-get-all-metric-sample-sets
  (t/testing "Creating the `GetAllMetricSampleSets` record type works."
    (t/is (m/get-all-metric-sample-sets? (m/get-all-metric-sample-sets)))))

(t/deftest t-record-and-get
  (t/testing "Monadically recording and getting the metric samples for the
  recorded metric with the given labels works."
    (t/is (quickcheck
           (property [names     (spec (util/gen-distinct-metric-names  3))
                      helps     (spec (util/gen-metric-helps           3))
                      threshold (spec ::metric-types/threshold)
                      labels    (spec ::metric-types/metric-labels)
                      values    (spec (util/gen-metric-values          2))]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])]

                       (metric-accumulator/reset-global-metric-store!)

                       ;; GAUGES
                       (let [[example-record-and-get-1 example-record-and-get-2]
                             (mock-monad/mock-run-monad
                              m/monad-command-config
                              []
                              (monad/monadic
                               [example-record-and-get-1
                                (m/record-and-get example-gauge-metric
                                                  labels
                                                  (util/metric-value-value               (nth values 0))
                                                  (util/metric-value-last-update-time-ms (nth values 0)))]
                               [example-record-and-get-2
                                (m/record-and-get example-gauge-metric
                                                  labels
                                                  (util/metric-value-value               (nth values 1))
                                                  (util/metric-value-last-update-time-ms (nth values 1)))]
                               (monad/return [example-record-and-get-1 example-record-and-get-2])))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                      labels
                                                                      (util/metric-value-value               (nth values 0))
                                                                      (util/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get-1))
                         (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                      labels
                                                                      (util/metric-value-value               (nth values 1))
                                                                      (util/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get-2)))

                       (metric-accumulator/reset-global-metric-store!)

                       ;; COUNTERS
                       (let [[example-record-and-get-1 example-record-and-get-2]
                             (mock-monad/mock-run-monad
                              m/monad-command-config
                              []
                              (monad/monadic
                               [example-record-and-get-1
                                (m/record-and-get example-counter-metric
                                                  labels
                                                  (util/metric-value-value               (nth values 0))
                                                  (util/metric-value-last-update-time-ms (nth values 0)))]
                               [example-record-and-get-2
                                (m/record-and-get example-counter-metric
                                                  labels
                                                  (util/metric-value-value               (nth values 1))
                                                  (util/metric-value-last-update-time-ms (nth values 1)))]

                               (monad/return [example-record-and-get-1 example-record-and-get-2])))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                      labels
                                                                      (util/metric-value-value               (nth values 0))
                                                                      (util/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get-1))
                         (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                      labels
                                                                      (+ (util/metric-value-value (nth values 0))
                                                                         (util/metric-value-value (nth values 1)))
                                                                      (util/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get-2)))

                       (metric-accumulator/reset-global-metric-store!)

                       ;; HISTOGRAMS
                       (let [[example-record-and-get-1 example-record-and-get-2]
                             (mock-monad/mock-run-monad
                              m/monad-command-config
                              []
                              (monad/monadic
                               [example-record-and-get-1

                                (m/record-and-get example-histogram-metric
                                                  labels
                                                  (util/metric-value-value               (nth values 0))
                                                  (util/metric-value-last-update-time-ms (nth values 0)))]
                               [example-record-and-get-2
                                (m/record-and-get example-histogram-metric
                                                  labels
                                                  (util/metric-value-value               (nth values 1))
                                                  (util/metric-value-last-update-time-ms (nth values 1)))]
                               (monad/return [example-record-and-get-1 example-record-and-get-2])))]
                         (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                      labels
                                                                      (util/metric-value-value               (nth values 0))
                                                                      (util/metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                      labels
                                                                      1.0
                                                                      (util/metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le "+Inf")
                                                                      1.0
                                                                      (util/metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le (str threshold))
                                                                      (if (<= (util/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                      (util/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get-1))

                         (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                      labels
                                                                      (+ (util/metric-value-value (nth values 0))
                                                                         (util/metric-value-value (nth values 1)))
                                                                      (util/metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                      labels
                                                                      2.0
                                                                      (util/metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le "+Inf")
                                                                      2.0
                                                                      (util/metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le (str threshold))
                                                                      (+ (if (<= (util/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                         (if (<= (util/metric-value-value (nth values 1)) threshold) 1.0 0.0))
                                                                      (util/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get-2)))))))))
