(ns active.clojure.logger.metric-samples-test
  (:require [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-types :as metric-types]
            
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

