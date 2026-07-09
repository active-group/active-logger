(ns active.clojure.logger.metric-singular-value-test
  (:require [active.clojure.logger.metric-singular-value :as m]
            [active.clojure.logger.metric-types :as metric-types]

            [clojure.test :as t]

            [clojure.spec.alpha :as s]

            [clojure.spec.test.alpha :as stest])
  (:use [active.quickcheck]))

(t/deftest t-make-metric-value
  (t/testing "All fields of a metric-value are set correct."
    (t/is (quickcheck
           (property [value       (spec ::m/metric-value-value-double)
                      update-time (spec ::metric-types/metric-last-update-time-ms)]
                     (let [example-metric-value (m/make-metric-value value update-time)]
                       (and (m/metric-value?                    example-metric-value)
                            (= (double value) (m/metric-value-value               example-metric-value))
                            (= update-time    (m/metric-value-last-update-time-ms example-metric-value)))))))))

(t/deftest t-inc-metric-value
  (t/testing "Increment of metric-values works."
    (t/is (quickcheck
           (property [metric-value (spec ::m/metric-value)
                      value (spec ::m/metric-value-value-double)
                      update-time (spec ::metric-types/metric-last-update-time-ms)]
                     (= (m/make-metric-value (+ value (if metric-value (m/metric-value-value metric-value) 0)) update-time)
                        (m/inc-metric-value metric-value value update-time)))))))
