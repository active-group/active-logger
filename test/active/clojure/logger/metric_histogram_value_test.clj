(ns active.clojure.logger.metric-histogram-value-test
  (:require [active.clojure.logger.metric-histogram-value :as m]
            [active.clojure.logger.metric-types :as metric-types]

            [clojure.test :as t]

            [clojure.spec.alpha :as s]

            [clojure.spec.test.alpha :as stest])
  (:use [active.quickcheck]))

(t/deftest t-make-histogram-metric-values
  (t/testing "All fields of a histogram-metric-values are set correct."
    (let [example-metric-value (m/make-histogram-metric-values 11 5 42 [11 12])]
      (t/is (m/histogram-metric-values? example-metric-value))
      (t/is (= 11          (m/histogram-metric-values-last-update-time-ms example-metric-value)))
      (t/is (= 5.0         (m/histogram-metric-values-sum-value example-metric-value)))
      (t/is (= 42          (m/histogram-metric-values-count-value example-metric-value)))
      (t/is (= [11 12]     (m/histogram-metric-values-bucket-values example-metric-value))))))


(t/deftest t-update-histogram-metric-values
  (t/is (= (m/make-histogram-metric-values 100 20 43 [11 13])
           (m/update-histogram-metric-values (m/make-histogram-metric-values 1 5 42 [11 12])
                                             [10 20]
                                             15
                                             100)))

  (t/testing "every new value has an effect, and not only the timestamp"
    (t/is (quickcheck
           (property [value (spec ::metric-types/metric-value)]
                     (let [base (m/make-histogram-metric-values 11 5 42 [11 12.0])]
                       (not= base
                             (m/update-histogram-metric-values base [0 100] value 11)))))))
  )
