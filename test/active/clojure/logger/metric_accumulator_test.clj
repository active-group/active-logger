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

;; -- DATA: raw metrics

(t/deftest t-fresh-raw-metric-store
  (t/testing "fresh-raw-metric-store is created empty"
    (t/is (some? (m/fresh-raw-metric-store)))
    (t/is (empty? (m/get-raw-metric-samples! (m/fresh-raw-metric-store))))))

(t/deftest t-make-metric-key
  (t/testing "All fields of a metric-key are set correct."
    (let [example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (t/is (m/metric-key? example-metric-key))
      (t/is (= "test-metric" (m/metric-key-name example-metric-key)))
      (t/is (= {:label-1 :value-1} (m/metric-key-labels example-metric-key))))))

(t/deftest t-make-metric-value
  (t/testing "All fields of a metric-value are set correct."
    (let [example-metric-value (m/make-metric-value 23 1)]
      (t/is (m/metric-value? example-metric-value))
      (t/is (= 23 (m/metric-value-value example-metric-value)))
      (t/is (= 1 (m/metric-value-timestamp example-metric-value))))))

(t/deftest t-make-metric-sample
  (t/testing "All fields of a metric-sample are set correct."
    (let [example-metric-sample (m/make-metric-sample "test-sample" {:label-1 :value-1} 23 1)]
      (t/is (m/metric-sample? example-metric-sample))
      (t/is "test-sample" (m/metric-sample-name example-metric-sample))
      (t/is {:label-1 :value-1} (m/metric-sample-labels example-metric-sample))
      (t/is 23 (m/metric-sample-value example-metric-sample))
      (t/is 1 (m/metric-sample-timestamp example-metric-sample)))))

(t/deftest t-set-raw-metric!
  (t/testing "basic setting and getting of one raw metric works"
    (let [raw-metric-store   (m/fresh-raw-metric-store)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (m/set-raw-metric! raw-metric-store example-metric-key (m/make-metric-value 23 1))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key)))))

  (t/testing "basic setting and resetting and getting of one raw metric works"
    (let [raw-metric-store   (m/fresh-raw-metric-store)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (m/set-raw-metric! raw-metric-store example-metric-key (m/make-metric-value 23 1))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key)))
      (m/set-raw-metric! raw-metric-store example-metric-key (m/make-metric-value 42 2))
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 42 2)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key)))))

  (t/testing "Raw metrics are different: Same metric name, but different labels."
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key-1 (m/make-metric-key "same-name" {:label-1 :value-1})
          example-metric-key-2 (m/make-metric-key "same-name" {:label-1 :value-2})]
      (m/set-raw-metric! raw-metric-store example-metric-key-1 (m/make-metric-value 23 1))
      (m/set-raw-metric! raw-metric-store example-metric-key-2 (m/make-metric-value 28 1))
      (t/is (not= (m/get-raw-metric-sample! raw-metric-store example-metric-key-1)
                  (m/get-raw-metric-sample! raw-metric-store example-metric-key-2)))
      (t/is (= (m/make-metric-sample "same-name" {:label-1 :value-1} 23 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-1)))
      (t/is (= (m/make-metric-sample "same-name" {:label-1 :value-2} 28 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-2)))))

  (t/testing "Metrics are different: Different metric names, but same labels."
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key-1 (m/make-metric-key "test-metric-1" {:label-1 :value-1})
          example-metric-key-2 (m/make-metric-key "test-metric-2" {:label-1 :value-1})]
      (m/set-raw-metric! raw-metric-store example-metric-key-1 (m/make-metric-value 23 1))
      (m/set-raw-metric! raw-metric-store example-metric-key-2 (m/make-metric-value 28 1))
      (t/is (not= (m/get-raw-metric-sample! raw-metric-store example-metric-key-1)
                  (m/get-raw-metric-sample! raw-metric-store example-metric-key-2)))
      (t/is (= (m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-1)))
      (t/is (= (m/make-metric-sample "test-metric-2" {:label-1 :value-1} 28 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-2))))))

(t/deftest t-update-metric-value
  (t/testing "Basic update raw metric works"
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

(t/deftest t-inc-raw-metric!
  (t/testing "Basic increasing raw metric works"
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key   (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value (m/make-metric-value 23 1)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))
            "If the raw metric is not in raw-metric-store, just add it."))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 23 2)]
      (m/set-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 46 2)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))
            "Update raw metric in raw-metric-store, if available."))))

(t/deftest t-get-raw-metric-sample!
  (t/testing "Getting metrics after simple set and inc operations works."
    (let [raw-metric-store   (m/fresh-raw-metric-store)
          example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (t/is (nil? (m/get-raw-metric-sample! raw-metric-store example-metric-key))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key-1   (m/make-metric-key "test-metric-1" {:label-1 :value-1})
          example-metric-key-2   (m/make-metric-key "test-metric-2" {:label-1 :value-2})
          example-metric-key-3   (m/make-metric-key "test-metric-3" {:label-3 :value-1})
          example-metric-key-4   (m/make-metric-key "test-metric-4" {:label-4 :value-4})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 33 2)
          example-metric-value-3 (m/make-metric-value 13 3)]
      (m/set-raw-metric! raw-metric-store example-metric-key-1 example-metric-value-1)
      (m/set-raw-metric! raw-metric-store example-metric-key-2 example-metric-value-1)
      (m/set-raw-metric! raw-metric-store example-metric-key-3 example-metric-value-2)
      (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-1)
      (t/is (= (m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-1)))
      (t/is (= (m/make-metric-sample "test-metric-2" {:label-1 :value-2} 23 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-2)))
      (t/is (= (m/make-metric-sample "test-metric-3" {:label-3 :value-1} 33 2)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-3)))
      (t/is (= (m/make-metric-sample "test-metric-4" {:label-4 :value-4} 23 1)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-4)))
      (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-2)
      (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-3)
      (t/is (= (m/make-metric-sample "test-metric-4" {:label-4 :value-4} 69 3)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key-4))))))

(t/deftest t-get-raw-metric-samples!
  (t/testing "Getting all raw-metric-store after simple set and inc operations works."
    (let [raw-metric-store (m/fresh-raw-metric-store)]
      (t/is (= [] (m/get-raw-metric-samples! raw-metric-store))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key-1   (m/make-metric-key "test-metric-1" {:label-1 :value-1})
          example-metric-key-2   (m/make-metric-key "test-metric-2" {:label-1 :value-2})
          example-metric-key-3   (m/make-metric-key "test-metric-3" {:label-3 :value-1})
          example-metric-key-4   (m/make-metric-key "test-metric-4" {:label-4 :value-4})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 33 2)
          example-metric-value-3 (m/make-metric-value 13 3)]
      (m/set-raw-metric! raw-metric-store example-metric-key-1 example-metric-value-1)
      (t/is (= [(m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1)]
               (m/get-raw-metric-samples! raw-metric-store)))
      (m/set-raw-metric! raw-metric-store example-metric-key-2 example-metric-value-1)
      (m/set-raw-metric! raw-metric-store example-metric-key-3 example-metric-value-2)
      (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-1)
      (t/is (= [(m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1),
                (m/make-metric-sample "test-metric-2" {:label-1 :value-2} 23 1),
                (m/make-metric-sample "test-metric-3" {:label-3 :value-1} 33 2),
                (m/make-metric-sample "test-metric-4" {:label-4 :value-4} 23 1)]
               (m/get-raw-metric-samples! raw-metric-store)))
      (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-2)
      (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-3)
      (t/is (= [(m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1),
                (m/make-metric-sample "test-metric-2" {:label-1 :value-2} 23 1),
                (m/make-metric-sample "test-metric-3" {:label-3 :value-1} 33 2),
                (m/make-metric-sample "test-metric-4" {:label-4 :value-4} 69 3)]
               (m/get-raw-metric-samples! raw-metric-store))))))

;; -- COMMANDS on raw metrics

(t/deftest t-monadic-set-get
  (t/testing "monadic setting and getting works"
    (let [result
          (mock-monad/mock-run-monad
            (m/monad-command-config)
            []
            (let [example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
              (monad/monadic
                (m/set-raw-metric example-metric-key (m/make-metric-value 23 1))
                (m/get-raw-metric-sample example-metric-key))))]
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 23 1)
               result)))))

(t/deftest t-monadic-set-inc-get
  (t/testing "monadic setting, incrementing and getting works"
    (let [result
          (mock-monad/mock-run-monad
            (m/monad-command-config)
            []
            (let [example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
              (monad/monadic
                (m/set-raw-metric example-metric-key (m/make-metric-value 23 1))
                (m/inc-raw-metric example-metric-key (m/make-metric-value 10 2))
                (m/get-raw-metric-sample example-metric-key))))]
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 2)
               result)))))

(t/deftest t-monadic-run-metrics)

;; -- METRICS

(t/deftest t-record-get-metric!
  (t/testing "Basic recording and getting counters works."
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-counter-metric (m/make-counter-metric "test-counter" nil {:label-1 :value-1})]
      (m/record-metric! raw-metric-store example-counter-metric 23 1)
      (t/is (= [(m/make-metric-sample "test-counter" {:label-1 :value-1} 23 1)]
               (m/get-metrics! raw-metric-store example-counter-metric)))
      (m/record-metric! raw-metric-store example-counter-metric 10 2)
      (t/is (= [(m/make-metric-sample "test-counter" {:label-1 :value-1} 33 2)]
               (m/get-metrics! raw-metric-store example-counter-metric)))))
  (t/testing "Basic recording and getting gauges works."
    (let [raw-metric-store      (m/fresh-raw-metric-store)
          example-gauge-metric (m/make-gauge-metric "test-gauge" nil {:label-1 :value-1})]
      (m/record-metric! raw-metric-store example-gauge-metric 23 1)
      (t/is (= [(m/make-metric-sample "test-gauge" {:label-1 :value-1} 23 1)]
               (m/get-metrics! raw-metric-store example-gauge-metric)))
      (m/record-metric! raw-metric-store example-gauge-metric 10 2)
      (t/is (= [(m/make-metric-sample "test-gauge" {:label-1 :value-1} 10 2)]
               (m/get-metrics! raw-metric-store example-gauge-metric)))))

  (t/testing "Basic recording and getting histograms works."
    (let [raw-metric-store      (m/fresh-raw-metric-store)
          example-histogram-metric (m/make-histogram-metric "test-histogram" 25 nil {:label-1 :value-1})]
      (m/record-metric! raw-metric-store example-histogram-metric 23 1)
      (t/is (= [(m/make-metric-sample "test-histogram_sum" {:label-1 :value-1} 23 1)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"} 1 1)
                (m/make-metric-sample "test-histogram_count" {:label-1 :value-1} 1 1)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"} 1 1)]
               ;; returns vector of vector?
               (m/get-metrics! raw-metric-store example-histogram-metric)))
      (m/record-metric! raw-metric-store example-histogram-metric 10 2)
      (t/is (= [(m/make-metric-sample "test-histogram_sum" {:label-1 :value-1} 33 2)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"} 2 2)
                (m/make-metric-sample "test-histogram_count" {:label-1 :value-1} 2 2)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"} 2 2)]
               (m/get-metrics! raw-metric-store example-histogram-metric)))
      (m/record-metric! raw-metric-store example-histogram-metric 25 3)
      (t/is (= [(m/make-metric-sample "test-histogram_sum" {:label-1 :value-1} 58 3)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"} 3 3)
                (m/make-metric-sample "test-histogram_count" {:label-1 :value-1} 3 3)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"} 3 3)]
               (m/get-metrics! raw-metric-store example-histogram-metric)))
      (m/record-metric! raw-metric-store example-histogram-metric 30 5)
      (t/is (= [(m/make-metric-sample "test-histogram_sum" {:label-1 :value-1} 88 5)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"} 4 5)
                (m/make-metric-sample "test-histogram_count" {:label-1 :value-1} 4 5)
                ;; Timestamp gets updated, counter remains the same
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"} 3 5)]
               (m/get-metrics! raw-metric-store example-histogram-metric))))
    (let [raw-metric-store      (m/fresh-raw-metric-store)
          example-histogram-metric (m/make-histogram-metric "test-histogram" 20 nil {:label-1 :value-1})]
      (m/record-metric! raw-metric-store example-histogram-metric 23 1)
      (t/is (= [(m/make-metric-sample "test-histogram_sum" {:label-1 :value-1} 23 1)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"} 1 1)
                (m/make-metric-sample "test-histogram_count" {:label-1 :value-1} 1 1)
                ;; Bucket available but count is 0
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "20"} 0 1)]
                (m/get-metrics! raw-metric-store example-histogram-metric))))))

(t/deftest t-monadic-record-get
  (t/testing "monadic recording and getting counters works"
    (let [result
          (mock-monad/mock-run-monad
           (m/monad-command-config)
           []
           (let [example-counter-metric (m/make-counter-metric "test-counter" nil {:label-1 :value-1})]
             (monad/monadic
              (m/record-metric example-counter-metric 23 1)
              (m/get-metrics example-counter-metric))))]
      (t/is (= [(m/make-metric-sample "test-counter" {:label-1 :value-1} 23 1)]
               result)))))


;; DESTRUCTIVE

;; -- DATA: raw metrics

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
(t/deftest t-d-inc-raw-metric!
  (t/testing "Metric value may be negative."
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value -33 2)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} -10 2)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key)))))
  (t/testing "Increasing raw metric with included nils."
    (let [raw-metric-store     nil
          example-metric-key   (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value (m/make-metric-value 23 1)]
      (t/is (thrown? NullPointerException
                     (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value))))
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key   nil
          example-metric-value (m/make-metric-value 23 1)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value)
      (t/is (empty? (m/get-raw-metric-samples! (m/fresh-raw-metric-store)))))
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key   (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value nil]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value)
      (t/is (empty? (m/get-raw-metric-samples! (m/fresh-raw-metric-store)))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {nil nil})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {nil nil} 33 2)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 nil})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 nil} 33 2)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {nil :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {nil :value-1} 33 2)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 nil)
          example-metric-value-2 (m/make-metric-value 10 2)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 2)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1)
          example-metric-value-2 (m/make-metric-value 10 nil)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 nil)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))))))

(t/deftest t-d-get-raw-metric-sample!
  (t/testing "Nil as arguments."
    (let [example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (t/is (thrown? NullPointerException
                     (m/get-raw-metric-sample! nil example-metric-key))))
    (let [raw-metric-store (m/fresh-raw-metric-store)]
      (t/is (nil? (m/get-raw-metric-sample! raw-metric-store nil))))))

(t/deftest t-d-get-raw-metric-samples!
  (t/testing "Nil as arguments."
    (t/is (thrown? NullPointerException (m/get-raw-metric-samples! nil)))))
