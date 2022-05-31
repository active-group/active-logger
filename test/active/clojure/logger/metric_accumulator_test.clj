(ns active.clojure.logger.metric-accumulator-test
  (:require [active.clojure.logger.metric-accumulator :as m]
            [clojure.test :as t]
            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]))

;; Note: For the moment we split the tests into
;; - constructive use: we use the functions as expected
;;   Function names: t-my-function
;; - destructive use: we try to break the functions
;;   Function names: t-d-my-function

;; CONSTRUCTIVE

;; -- DATA

(t/deftest t-fresh-metrics
  (t/testing "fresh-metrics is created empty"
    (t/is (some? (m/fresh-metrics)))
    (t/is (empty? (m/get-metric-samples! (m/fresh-metrics))))))

(t/deftest t-make-metric-key
  (t/testing "All fields of a metric-key are set correct."
    (let [example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (t/is (m/metric-key? example-metric-key))
      (t/is (= "test-metric" (m/metric-key-name example-metric-key)))
      (t/is (= {:label-1 :value-1} (m/metric-key-labels example-metric-key))))))

(t/deftest t-make-metric-value)
(t/deftest t-make-metric-sample)

(t/deftest t-set-metric!
  (t/testing "basic setting and getting of one metric works"
    (let [metrics            (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (m/set-metric! metrics example-metric-key (m/make-metric-value 23 1))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               (m/get-metric-sample! metrics example-metric-key)))))

  (t/testing "basic setting and resetting and getting of one metric works"
    (let [metrics            (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (m/set-metric! metrics example-metric-key (m/make-metric-value 23 1))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               (m/get-metric-sample! metrics example-metric-key)))
      (m/set-metric! metrics example-metric-key (m/make-metric-value 42 2))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 42 2)
               (m/get-metric-sample! metrics example-metric-key)))))

  (t/testing "Metrics are different: Same metric name, but different labels."
    (let [metrics              (m/fresh-metrics)
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
    (let [metrics              (m/fresh-metrics)
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
      (t/is (= (m/make-metric-value 24 2)
               (m/update-metric-value + example-metric-value-1 example-metric-value-2))
            "Add value-1 and value-2. Take timestamp from 2."))))

(t/deftest t-sum-metric-value
  (t/testing "Adds metric-values and nothing else."
    (let [example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 1 2)]
      (t/is (= example-metric-value-2
               (m/sum-metric-value nil example-metric-value-2))
            "If there is no base value, just take the second.")
      (t/is (= (m/make-metric-value 24 2)
               (m/sum-metric-value example-metric-value-1 example-metric-value-2))
            "Add value-1 and value-2. Take timestamp from 2."))))

(t/deftest t-inc-metric!
  (t/testing "Basic increasing metric works"
    (let [metrics              (m/fresh-metrics)
          example-metric-key   (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value (m/make-metric-value 23 1)]
      (m/inc-metric! metrics example-metric-key example-metric-value)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               (m/get-metric-sample! metrics example-metric-key))
            "If the metric is not in metrics, just add it."))
    (let [metrics                (m/fresh-metrics)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 23 2)]
      (m/set-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 46 2)
               (m/get-metric-sample! metrics example-metric-key))
            "Update metric in metrics, if available."))))

(t/deftest t-get-metric-sample!
  (t/testing "Getting metrics after simple set and inc operations works."
    (let [metrics            (m/fresh-metrics)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (t/is (nil? (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics                (m/fresh-metrics)
          example-metric-key-1   (m/make-metric-key "test-metric-1" {:label-1 :value-1})
          example-metric-key-2   (m/make-metric-key "test-metric-2" {:label-1 :value-2})
          example-metric-key-3   (m/make-metric-key "test-metric-3" {:label-3 :value-1})
          example-metric-key-4   (m/make-metric-key "test-metric-4" {:label-4 :value-4})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 33 2)
          example-metric-value-3 (m/make-metric-value 13 3)]
      (m/set-metric! metrics example-metric-key-1 example-metric-value-1)
      (m/set-metric! metrics example-metric-key-2 example-metric-value-1)
      (m/set-metric! metrics example-metric-key-3 example-metric-value-2)
      (m/inc-metric! metrics example-metric-key-4 example-metric-value-1)
      (t/is (= (m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1)
               (m/get-metric-sample! metrics example-metric-key-1)))
      (t/is (= (m/make-metric-sample "test-metric-2" {:label-1 :value-2} 23 1)
               (m/get-metric-sample! metrics example-metric-key-2)))
      (t/is (= (m/make-metric-sample "test-metric-3" {:label-3 :value-1} 33 2)
               (m/get-metric-sample! metrics example-metric-key-3)))
      (t/is (= (m/make-metric-sample "test-metric-4" {:label-4 :value-4} 23 1)
               (m/get-metric-sample! metrics example-metric-key-4)))
      (m/inc-metric! metrics example-metric-key-4 example-metric-value-2)
      (m/inc-metric! metrics example-metric-key-4 example-metric-value-3)
      (t/is (= (m/make-metric-sample "test-metric-4" {:label-4 :value-4} 69 3)
               (m/get-metric-sample! metrics example-metric-key-4))))))

(t/deftest t-get-metric-samples!
  (t/testing "Getting all metrics after simple set and inc operations works."
    (let [metrics (m/fresh-metrics)]
      (t/is (= [] (m/get-metric-samples! metrics))))
    (let [metrics                (m/fresh-metrics)
          example-metric-key-1   (m/make-metric-key "test-metric-1" {:label-1 :value-1})
          example-metric-key-2   (m/make-metric-key "test-metric-2" {:label-1 :value-2})
          example-metric-key-3   (m/make-metric-key "test-metric-3" {:label-3 :value-1})
          example-metric-key-4   (m/make-metric-key "test-metric-4" {:label-4 :value-4})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 33 2)
          example-metric-value-3 (m/make-metric-value 13 3)]
      (m/set-metric! metrics example-metric-key-1 example-metric-value-1)
      (t/is (= [(m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1)]
               (m/get-metric-samples! metrics)))
      (m/set-metric! metrics example-metric-key-2 example-metric-value-1)
      (m/set-metric! metrics example-metric-key-3 example-metric-value-2)
      (m/inc-metric! metrics example-metric-key-4 example-metric-value-1)
      (t/is (= [(m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1),
                (m/make-metric-sample "test-metric-2" {:label-1 :value-2} 23 1),
                (m/make-metric-sample "test-metric-3" {:label-3 :value-1} 33 2),
                (m/make-metric-sample "test-metric-4" {:label-4 :value-4} 23 1)]
               (m/get-metric-samples! metrics)))
      (m/inc-metric! metrics example-metric-key-4 example-metric-value-2)
      (m/inc-metric! metrics example-metric-key-4 example-metric-value-3)
      (t/is (= [(m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1),
                (m/make-metric-sample "test-metric-2" {:label-1 :value-2} 23 1),
                (m/make-metric-sample "test-metric-3" {:label-3 :value-1} 33 2),
                (m/make-metric-sample "test-metric-4" {:label-4 :value-4} 69 3)]
               (m/get-metric-samples! metrics))))))

;; -- COMMANDS

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


;; DESTRUCTIVE

;; -- DATA

(t/deftest t-d-make-metric-key
  (t/testing "No metric-key field must be nil"
    (t/is (thrown? AssertionError (m/make-metric-key nil {:label-1 :value-1})))
    (t/is (thrown? AssertionError (m/make-metric-key "test-metric" nil)))))

(t/deftest t-d-make-metric-value)
(t/deftest t-d-make-metric-sample)

(t/deftest t-d-update-metric-value
  (t/testing "Nil as arguments."
    (let [example-metric-value-1 (m/make-metric-value 23 1)]
      (t/is (thrown? NullPointerException
                     (m/update-metric-value + example-metric-value-1 nil))
            "metric-value-2 must not be nil"))
    (t/testing "The function may not be nil."
      (let [example-metric-value-1 (m/make-metric-value 23 1)
            example-metric-value-2 (m/make-metric-value 1 2)]
        (t/is (thrown? NullPointerException
                       (m/update-metric-value nil example-metric-value-1 example-metric-value-2)))))))

(t/deftest t-d-sum-metric-value
  (t/testing "Adds metric-values and nothing else."
    (let [example-metric-value-1 (m/make-metric-value 23 1)]
      (t/is (thrown? NullPointerException
                     (m/sum-metric-value example-metric-value-1 nil))
            "metric-value-2 must not be nil"))))

;; active-quickcheck?
(t/deftest t-d-inc-metric!
  (t/testing "Increasing metric with included nils."
    (let [metrics              nil
          example-metric-key   (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value (m/make-metric-value 23 1)]
      (t/is (thrown? NullPointerException
                     (m/inc-metric! metrics example-metric-key example-metric-value))))
    (let [metrics              (m/fresh-metrics)
          example-metric-key   nil
          example-metric-value (m/make-metric-value 23 1)]
      (m/inc-metric! metrics example-metric-key example-metric-value)
      (t/is (empty? (m/get-metric-samples! (m/fresh-metrics)))))
    (let [metrics              (m/fresh-metrics)
          example-metric-key   (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value nil]
      (m/inc-metric! metrics example-metric-key example-metric-value)
      (t/is (empty? (m/get-metric-samples! (m/fresh-metrics)))))
    (let [metrics                (m/fresh-metrics)
          example-metric-key     (m/make-metric-key "test-metric" {nil nil})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {nil nil} 33 2)
               (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics                (m/fresh-metrics)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 nil})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 nil} 33 2)
               (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics                (m/fresh-metrics)
          example-metric-key     (m/make-metric-key "test-metric" {nil :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {nil :value-1} 33 2)
               (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics                (m/fresh-metrics)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 nil)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 2)
               (m/get-metric-sample! metrics example-metric-key))))
    (let [metrics                (m/fresh-metrics)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 nil)]
      (m/inc-metric! metrics example-metric-key example-metric-value-1)
      (m/inc-metric! metrics example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 nil)
               (m/get-metric-sample! metrics example-metric-key))))))

(t/deftest t-d-get-metric-sample!
  (t/testing "Nil as arguments."
    (let [example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (t/is (thrown? NullPointerException
                     (m/get-metric-sample! nil example-metric-key))))
    (let [metrics (m/fresh-metrics)]
      (t/is (nil? (m/get-metric-sample! metrics nil))))))

(t/deftest t-d-get-metric-samples!
  (t/testing "Nil as arguments."
    (t/is (thrown? NullPointerException (m/get-metric-samples! nil)))))
