(ns active.clojure.logger.metric-accumulator-test
  (:require [active.clojure.logger.metric-accumulator :as m]
            [clojure.test :as t]
            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]))

;; DATA

(t/deftest t-fresh-metrics
  (t/testing "fresh-metrics is created empty"
    (t/is (some? (m/fresh-metrics)))
    (t/is (empty? (m/get-metric-samples! (m/fresh-metrics))))))

(t/deftest t-make-metric-key
  (t/testing "No metric-key field must be nil"
    (t/is (thrown? AssertionError (m/make-metric-key nil {:label-1 :value-1})))
    (t/is (thrown? AssertionError (m/make-metric-key "test-metric" nil)))))

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
               (m/get-metric-sample! metrics example-metric-key)))))

  (t/testing "Metrics are different: Same metric name, but different labels."
    (let [metrics (m/fresh-metrics)
          example-metric-key-1 (m/make-metric-key "same-name" {:label-1 :value-1})
          example-metric-key-2 (m/make-metric-key "same-name" {:label-1 :value-2})]
      (m/set-metric! metrics example-metric-key-1 (m/make-metric-value 23 1))
      (m/set-metric! metrics example-metric-key-2 (m/make-metric-value 28 1))
      (t/is (not= (m/get-metric-sample! metrics example-metric-key-1)
                  (m/get-metric-sample! metrics example-metric-key-2)))
      (t/is (= (m/make-metric-sample "same-name" {:label-1 :value-1} 23 1)
               (m/get-metric-sample! metrics example-metric-key-1)))
      (t/is (= (m/make-metric-sample "same-name" {:label-1 :value-2} 28 1)
               (m/get-metric-sample! metrics example-metric-key-2)))))

  (t/testing "Metrics are different: Different metric names, but same labels."
    (let [metrics (m/fresh-metrics)
          example-metric-key-1 (m/make-metric-key "test-metric-1" {:label-1 :value-1})
          example-metric-key-2 (m/make-metric-key "test-metric-2" {:label-1 :value-1})]
      (m/set-metric! metrics example-metric-key-1 (m/make-metric-value 23 1))
      (m/set-metric! metrics example-metric-key-2 (m/make-metric-value 28 1))
      (t/is (not= (m/get-metric-sample! metrics example-metric-key-1)
                  (m/get-metric-sample! metrics example-metric-key-2)))
      (t/is (= (m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1)
               (m/get-metric-sample! metrics example-metric-key-1)))
      (t/is (= (m/make-metric-sample "test-metric-2" {:label-1 :value-1} 28 1)
               (m/get-metric-sample! metrics example-metric-key-2))))))

(t/deftest t-update-metric-value
  (t/testing "Basic update metric works"
    (let [example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 1 2)]
      (t/is (= example-metric-value-2
               (m/update-metric-value + nil example-metric-value-2))
               "If there is no base value, just take the second.")
      (t/is (thrown? NullPointerException
                     (m/update-metric-value + example-metric-value-1 nil))
              "metric-value-2 must not be nil")
      (t/is (= (m/make-metric-value 24 2)
               (m/update-metric-value + example-metric-value-1 example-metric-value-2))
            "Add value-1 and value-2. Take timestamp from 2.")))
  (t/testing "Values and timestamp can be any type and even nil."
    (let [example-metric-value-1 (m/make-metric-value nil nil)
          example-metric-value-2 (m/make-metric-value nil nil)]
      (t/is (= (m/make-metric-value "" nil)
               (m/update-metric-value str example-metric-value-1 example-metric-value-2))))
    (let [example-metric-value-1 (m/make-metric-value nil nil)
          example-metric-value-2 (m/make-metric-value "b" nil)]
      (t/is (= (m/make-metric-value "b" nil)
               (m/update-metric-value str example-metric-value-1 example-metric-value-2))))
    (let [example-metric-value-1 (m/make-metric-value "a" nil)
          example-metric-value-2 (m/make-metric-value "b" nil)]
      (t/is (= (m/make-metric-value "ab" nil)
               (m/update-metric-value str example-metric-value-1 example-metric-value-2))))
    (let [example-metric-value-1 (m/make-metric-value "a" nil)
          example-metric-value-2 (m/make-metric-value "b" "my-time")]
      (t/is (= (m/make-metric-value "ab" "my-time")
               (m/update-metric-value str example-metric-value-1 example-metric-value-2))))))

(t/deftest t-inc-metric!
  (t/testing "Basic increasing metric works"
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value (m/make-metric-value 23 1)]
      (m/inc-metric! metrics example-metric-key example-metric-value)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               (m/get-metric-sample! metrics example-metric-key))
            "If the metric is not in metrics, just add it."))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 23 2)]
      (m/set-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 46 2)
               (m/get-metric-sample! metrics example-metric-key))
            "Update metric in metrics, if available.")))
  (t/testing "Increasing metric with nil included nils"
    (let [metrics nil
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value (m/make-metric-value 23 1)]
      (t/is (thrown? NullPointerException
                     (m/inc-metric! metrics example-metric-key example-metric-value))))
    (let [metrics (m/fresh-metrics)
          example-metric-key nil
          example-metric-value (m/make-metric-value 23 1)]
      (m/inc-metric! metrics example-metric-key example-metric-value)
      (t/is (empty? (m/get-metric-samples! (m/fresh-metrics)))))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value nil]
      (m/inc-metric! metrics example-metric-key example-metric-value)
      (t/is (empty? (m/get-metric-samples! (m/fresh-metrics)))))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {nil nil})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {nil nil} 33 2)
               (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 nil})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 nil} 33 2)
               (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {nil :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {nil :value-1} 33 2)
               (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value nil 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (t/is (thrown? NullPointerException
                     (m/inc-metric! metrics example-metric-key example-metric-value-2))))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value nil 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (t/is (thrown? NullPointerException
                     (m/inc-metric! metrics example-metric-key example-metric-value-2))))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 nil)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 2)
               (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 nil)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 nil)
               (m/get-metric-sample! metrics example-metric-key))))))

(t/deftest t-monadic-set-get
  (t/testing "monadic setting and getting works"
    (let [result
          (mock-monad/mock-run-monad
            (m/monad-command-config)
            []
            (let [example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
              (monad/monadic
                (m/set-metric example-metric-key (m/make-metric-value 23 1))
                (m/get-metric-sample example-metric-key))))]
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               result)))))
