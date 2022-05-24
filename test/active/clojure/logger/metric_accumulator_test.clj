(ns active.clojure.logger.metric-accumulator-test
  (:require [active.clojure.logger.metric-accumulator :as m]
            [clojure.test :as t]))

;; DATA

(t/deftest t-fresh-metrics
  (t/testing "fresh-metrics does not return nil"
    (t/is (some? (m/fresh-metrics)))
    (t/is (empty? (m/get-metric-samples! (m/fresh-metrics))))))

(t/deftest t-set-metric!
  (t/testing "basic setting and getting of one metric works"
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (m/set-metric! metrics example-metric-key (m/make-metric-value 23 1))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
             (m/get-metric-sample! metrics example-metric-key)))))
  (t/testing "basic setting and resetting and getting of one metric works"
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (m/set-metric! metrics example-metric-key (m/make-metric-value 23 1))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
             (m/get-metric-sample! metrics example-metric-key)))
      (m/set-metric! metrics example-metric-key (m/make-metric-value 42 2))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 42 2)
             (m/get-metric-sample! metrics example-metric-key))))))
