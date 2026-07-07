(ns active.clojure.logger.metric-samples-test
  (:require [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-histogram-value :as histogram]
            [active.clojure.logger.metric-singular-value :as singular]
            
            [clojure.test :as t]

            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest])
  (:use [active.quickcheck]))

(t/use-fixtures :once
  (fn [f]
    (stest/instrument)
    (s/check-asserts true)
    (f)
    (s/check-asserts false)
    (stest/unstrument)))

(defn gen-metric-samples
  [num-elems]
  (s/spec (s/coll-of ::metric-samples/metric-sample :distinct false :into [] :count num-elems)))

(t/deftest t-make-metric-sample
  (t/testing "All fields of a metric-sample are set correct."
    (t/is (quickcheck
           (property [metric-name (spec ::metric-types/metric-name)
                      labels      (spec ::metric-types/metric-labels)
                      value       (spec ::metric-types/metric-value)
                      timestamp   (spec ::metric-types/metric-last-update-time-ms)]
                     (let [example-metric-sample (metric-samples/make-metric-sample metric-name
                                                                                    labels
                                                                                    value
                                                                                    timestamp)]
                       (t/is                (metric-samples/metric-sample?          example-metric-sample))
                       (t/is (= metric-name (metric-samples/metric-sample-name      example-metric-sample)))
                       (t/is (= labels      (metric-samples/metric-sample-labels    example-metric-sample)))
                       (t/is (= value       (metric-samples/metric-sample-value     example-metric-sample)))
                       (t/is (= timestamp   (metric-samples/metric-sample-timestamp example-metric-sample)))))))))

(t/deftest t-make-metric-sample-set
  (t/testing "Creating a metric-sample-set works."
    (t/is (quickcheck
           (property [name        (spec ::metric-types/metric-name)
                      type (spec ::metric-types/metric-type)
                      help        (spec ::metric-types/metric-help)
                      samples     (spec (gen-metric-samples 4))]
                     (let [example-metric-sample-set (metric-samples/make-metric-sample-set name
                                                                                            type
                                                                                            help
                                                                                            samples)]
                       (t/is                (metric-samples/metric-sample-set?            example-metric-sample-set))
                       (t/is (= name        (metric-samples/metric-sample-set-name        example-metric-sample-set)))
                       (t/is (= type        (metric-samples/metric-sample-set-type example-metric-sample-set)))
                       (t/is (= help        (metric-samples/metric-sample-set-help        example-metric-sample-set)))
                       (t/is (= samples     (metric-samples/metric-sample-set-samples     example-metric-sample-set)))))))))


(t/deftest t-all-snapshots->all-metric-samples
  (t/testing "Getting all metric-samples from stored values works for all kind of metrics."
    (let [metric-name "foo"
          gauge-metric (metric-types/make-gauge-metric metric-name "bar")
          counter-metric (metric-types/make-counter-metric metric-name "bar")
          threshold1 0.0
          threshold2 100.0
          histogram-metric (metric-types/make-histogram-metric metric-name "bar" [threshold1 threshold2])
          labels {:a "baz"}
          value1 42.0
          value2 20.0
          time-ms 11
          count1 5
          count2 3]
      
      (t/is (= (metric-samples/all-snapshots->all-metric-samples gauge-metric {labels (singular/make-metric-value value1 time-ms)})
               [(metric-samples/make-metric-sample metric-name labels value1 time-ms)]))
      (t/is (= (metric-samples/all-snapshots->all-metric-samples counter-metric {labels (singular/make-metric-value value2 time-ms)})
               [(metric-samples/make-metric-sample metric-name labels value2 time-ms)]))
      (t/is (= (metric-samples/all-snapshots->all-metric-samples histogram-metric
                                                                 {labels (histogram/make-histogram-metric-values time-ms value1 (+ count1 count2) [count1 count2])})
               [(metric-samples/make-metric-sample (str metric-name "_sum")
                                                   labels
                                                   value1
                                                   time-ms)
                (metric-samples/make-metric-sample (str metric-name "_count")
                                                   labels
                                                   (double (+ count1 count2))
                                                   time-ms)
                (metric-samples/make-metric-sample (str metric-name "_bucket")
                                                   (assoc labels :le "+Inf")
                                                   (double (+ count1 count2))
                                                   time-ms)
                (metric-samples/make-metric-sample (str metric-name "_bucket")
                                                   (assoc labels :le (str threshold1))
                                                   (double count1)
                                                   time-ms)
                (metric-samples/make-metric-sample (str metric-name "_bucket")
                                                   (assoc labels :le (str threshold2))
                                                   (double count2)
                                                   time-ms)])))))
