(ns active.clojure.logger.metric-accumulator-test
  (:require [active.clojure.logger.metric-accumulator :as m]
            [clojure.test :as t]
            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]

            [clojure.spec.test.alpha :as stest])
  (:use [active.quickcheck]))

(t/use-fixtures :each (fn [f] (m/reset-global-raw-metric-store!) (f)))

(defmacro mock-run-monad
  [& ?args]
  `(do
     (m/reset-global-raw-metric-store!)
     (mock-monad/mock-run-monad ~@?args)))

(stest/instrument)

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

;; TODO: is the `(t/is ...)` neccessary in front of `(quickcheck ...)`?
(t/deftest t-make-metric-key
  (t/testing "All fields of a metric-key are set correct."
    (t/is (quickcheck
           (property [name   (spec ::m/metric-key-name)
                      labels (spec ::m/metric-key-labels)]
                     (let [example-metric-key (m/make-metric-key name labels)]
                       (t/is           (m/metric-key?       example-metric-key))
                       (t/is (= name   (m/metric-key-name   example-metric-key)))
                       (t/is (= labels (m/metric-key-labels example-metric-key)))))))))

(t/deftest t-make-metric-value
  (t/testing "All fields of a metric-value are set correct."
    (t/is (quickcheck
           (property [value       (spec ::m/metric-value-value)
                      timestamp   (spec ::m/metric-value-timestamp)
                      update-time (spec ::m/metric-value-last-update-time-ms)]
                     (let [example-metric-value (m/make-metric-value value timestamp update-time)]
                       (t/is                (m/metric-value?                    example-metric-value))
                       (t/is (= value       (m/metric-value-value               example-metric-value)))
                       (t/is (= timestamp   (m/metric-value-timestamp           example-metric-value)))
                       (t/is (= update-time (m/metric-value-last-update-time-ms example-metric-value)))))))))

(t/deftest t-make-metric-sample
  (t/testing "All fields of a metric-sample are set correct."
    (t/is (quickcheck
           (property [name        (spec ::m/metric-key-name)
                      labels      (spec ::m/metric-key-labels)
                      value       (spec ::m/metric-value-value)
                      timestamp   (spec ::m/metric-value-timestamp)
                      update-time (spec ::m/metric-value-last-update-time-ms)]
                     (let [example-metric-sample (m/make-metric-sample name labels value timestamp update-time)]
                       (t/is                (m/metric-sample?                    example-metric-sample))
                       (t/is (= name        (m/metric-sample-name                example-metric-sample)))
                       (t/is (= labels      (m/metric-sample-labels              example-metric-sample)))
                       (t/is (= value       (m/metric-sample-value               example-metric-sample)))
                       (t/is (= timestamp   (m/metric-sample-timestamp           example-metric-sample)))
                       (t/is (= update-time (m/metric-sample-last-update-time-ms example-metric-sample)))))))))

(t/deftest t-set-raw-metric!
  (t/testing "basic setting and getting of one raw metric works"
    (t/is (quickcheck
           (property [example-metric-key   (spec ::m/metric-key)
                      example-metric-value (spec ::m/metric-value)]
                     (let [raw-metric-store (m/fresh-raw-metric-store)]
                       (m/set-raw-metric! raw-metric-store example-metric-key example-metric-value)
                       (t/is (= (m/make-metric-sample
                                 (m/metric-key-name                  example-metric-key)
                                 (m/metric-key-labels                example-metric-key)
                                 (m/metric-value-value               example-metric-value)
                                 (m/metric-value-timestamp           example-metric-value)
                                 (m/metric-value-last-update-time-ms example-metric-value))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key))))))))

  (t/testing "basic setting and getting of one raw metric works"
    (t/is (quickcheck
           (property [example-metric-key     (spec ::m/metric-key)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [raw-metric-store (m/fresh-raw-metric-store)]
                       (m/set-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
                       (t/is (= (m/make-metric-sample
                                 (m/metric-key-name                  example-metric-key)
                                 (m/metric-key-labels                example-metric-key)
                                 (m/metric-value-value               example-metric-value-1)
                                 (m/metric-value-timestamp           example-metric-value-1)
                                 (m/metric-value-last-update-time-ms example-metric-value-1))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key)))
                       (m/set-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
                       (t/is (= (m/make-metric-sample
                                 (m/metric-key-name                  example-metric-key)
                                 (m/metric-key-labels                example-metric-key)
                                 (m/metric-value-value               example-metric-value-2)
                                 (m/metric-value-timestamp           example-metric-value-2)
                                 (m/metric-value-last-update-time-ms example-metric-value-2))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key))))))))

  ;; TODO: keep? comment? delete?
  (t/testing "MetricKeys are different: Same metric name, but different labels."
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key-1 (m/make-metric-key "same-name" {:label-1 :value-1})
          example-metric-key-2 (m/make-metric-key "same-name" {:label-1 :value-2})]
      (m/set-raw-metric! raw-metric-store example-metric-key-1 (m/make-metric-value 23 1 500))
      (m/set-raw-metric! raw-metric-store example-metric-key-2 (m/make-metric-value 28 1 600))
      (t/is (not= (m/get-raw-metric-sample! raw-metric-store example-metric-key-1)
                  (m/get-raw-metric-sample! raw-metric-store example-metric-key-2)))
      (t/is (m/make-metric-sample "same-name" {:label-1 :value-1} 23 1 500)
            (m/get-raw-metric-sample! raw-metric-store example-metric-key-1))
      (t/is (m/make-metric-sample "same-name" {:label-1 :value-2} 28 1 600)
            (m/get-raw-metric-sample! raw-metric-store example-metric-key-2))))

  (t/testing "MetricKeys are different: Different metric names, but same labels."
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key-1 (m/make-metric-key "test-metric-1" {:label-1 :value-1})
          example-metric-key-2 (m/make-metric-key "test-metric-2" {:label-1 :value-1})]
      (m/set-raw-metric! raw-metric-store example-metric-key-1 (m/make-metric-value 23 1 500))
      (m/set-raw-metric! raw-metric-store example-metric-key-2 (m/make-metric-value 28 1 600))
      (t/is (not= (m/get-raw-metric-sample! raw-metric-store example-metric-key-1)
                  (m/get-raw-metric-sample! raw-metric-store example-metric-key-2)))
      (t/is (m/make-metric-sample "test-metric-1" {:label-1 :value-1} 23 1 500)
            (m/get-raw-metric-sample! raw-metric-store example-metric-key-1))
      (t/is (m/make-metric-sample "test-metric-2" {:label-1 :value-1} 28 1 600)
            (m/get-raw-metric-sample! raw-metric-store example-metric-key-2)))))

;; TODO: we do not enforce that timestamp-2 and update-time-2 is larger than the one from value-1
(t/deftest t-update-metric-value
  ;; TODO: More functions? Not only '+'?
  (t/testing "Basic update raw metric works"
    (t/is (quickcheck
           (property [example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     ;; TODO: Can we add the nil to the specs? (s/or nil? (spec ::m/metric-value))
                     ;; If there is no base value, just take the second.
                     (t/is (= example-metric-value-2
                              (m/update-metric-value + nil example-metric-value-2)))
                     ;; Add value-1 and value-2. Take timestamp and update-time from 2.
                     (let [value-1       (m/metric-value-value               example-metric-value-1)
                           value-2       (m/metric-value-value               example-metric-value-2)
                           timestamp-2   (m/metric-value-timestamp           example-metric-value-2)
                           update-time-2 (m/metric-value-last-update-time-ms example-metric-value-2)]
                     (t/is (= (m/make-metric-value (+ value-1 value-2) timestamp-2 update-time-2)
                              (m/update-metric-value + example-metric-value-1 example-metric-value-2)))))))))

;; TODO: we do not enforce that timestamp-2 and update-time-2 is larger than the one from value-1
(t/deftest t-sum-metric-value
  (t/testing "Adds metric-values and nothing else."
    (t/is (quickcheck
           (property [example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     ;; TODO: Can we add the nil to the specs? (s/or nil? (spec ::m/metric-value))
                     ;; If there is no base value, just take the second.
                     (t/is (= example-metric-value-2
                              (m/sum-metric-value nil example-metric-value-2)))
                     ;; Add value-1 and value-2. Take timestamp and update-time from 2.
                     (t/is (= (m/make-metric-value
                               (+ (m/metric-value-value            example-metric-value-1)
                                  (m/metric-value-value            example-metric-value-2))
                               (m/metric-value-timestamp           example-metric-value-2)
                               (m/metric-value-last-update-time-ms example-metric-value-2))
                              (m/sum-metric-value example-metric-value-1 example-metric-value-2))))))))

(t/deftest t-inc-raw-metric!
  (t/testing "Basic increasing raw metric works"
    (t/is (quickcheck
           (property [example-metric-key   (spec ::m/metric-key)
                      example-metric-value (spec ::m/metric-value)]
                     (let [raw-metric-store (m/fresh-raw-metric-store)]
                       ;; If the metric is not in raw-metric-store, just add it.
                       (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value)
                       (t/is (= (m/make-metric-sample
                                 (m/metric-key-name                  example-metric-key)
                                 (m/metric-key-labels                example-metric-key)
                                 (m/metric-value-value               example-metric-value)
                                 (m/metric-value-timestamp           example-metric-value)
                                 (m/metric-value-last-update-time-ms example-metric-value))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key)))))))
    (t/is (quickcheck
           (property [example-metric-key     (spec ::m/metric-key)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [raw-metric-store (m/fresh-raw-metric-store)]
                       ;; Update metric in raw-metric-store, if available.
                       (m/set-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
                       (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
                       (t/is (= (m/make-metric-sample
                                 (m/metric-key-name                  example-metric-key)
                                 (m/metric-key-labels                example-metric-key)
                                 (+ (m/metric-value-value            example-metric-value-1)
                                    (m/metric-value-value               example-metric-value-2))
                                 (m/metric-value-timestamp           example-metric-value-2)
                                 (m/metric-value-last-update-time-ms example-metric-value-2))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key)))))))))

;; TODO more Tests!
(t/deftest prune-stale-raw-metrics!
  (t/testing "Simple pruning of metrics works."
  (let [raw-metric-store       (m/fresh-raw-metric-store)
        example-metric-key-1   (m/make-metric-key "t1" {:l1 :v1})
        example-metric-value-1 (m/make-metric-value 1 2 100)
        example-metric-key-2   (m/make-metric-key "t2" {:l2 :v2})
        example-metric-value-2 (m/make-metric-value 2 3 200)
        example-metric-key-3   (m/make-metric-key "t3" {:l3 :v3})
        example-metric-value-3 (m/make-metric-value 3 4 300)]
    ;; empty nothing to prune
    (m/prune-stale-raw-metrics! raw-metric-store 200)
    (t/is (= [] (m/get-raw-metric-samples! raw-metric-store)))

    (m/set-raw-metric! raw-metric-store example-metric-key-1 example-metric-value-1)
    (m/set-raw-metric! raw-metric-store example-metric-key-2 example-metric-value-2)
    (m/set-raw-metric! raw-metric-store example-metric-key-3 example-metric-value-3)

    ;; there is no value smaller than 50
    (m/prune-stale-raw-metrics! raw-metric-store 50)
    (t/is (= [(m/make-metric-sample "t1" {:l1 :v1} 1 2 100)
              (m/make-metric-sample "t2" {:l2 :v2} 2 3 200)
              (m/make-metric-sample "t3" {:l3 :v3} 3 4 300)]
             (m/get-raw-metric-samples! raw-metric-store)))

    ;; there is one value smaller than 200
    (m/prune-stale-raw-metrics! raw-metric-store 200)
    (t/is (= [(m/make-metric-sample "t2" {:l2 :v2} 2 3 200)
              (m/make-metric-sample "t3" {:l3 :v3} 3 4 300)]
             (m/get-raw-metric-samples! raw-metric-store))))))

(t/deftest t-get-raw-metric-sample!
  (t/testing "Getting metrics after simple set and inc operations works."
    (t/is (quickcheck
           (property [example-metric-key (spec ::m/metric-key)]
                     (let [raw-metric-store (m/fresh-raw-metric-store)]
                       (t/is (nil? (m/get-raw-metric-sample! raw-metric-store example-metric-key)))))))
    (t/is (quickcheck
           (property [[example-metric-key-1,
                       example-metric-key-2,
                       example-metric-key-3,
                       example-metric-key-4] (spec (m/gen-distinct-metric-keys 4))

                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)
                      example-metric-value-3 (spec ::m/metric-value)
                      example-metric-value-4 (spec ::m/metric-value)
                      example-metric-value-5 (spec ::m/metric-value)
                      example-metric-value-6 (spec ::m/metric-value)]
                     (let [raw-metric-store (m/fresh-raw-metric-store)]
                       (m/set-raw-metric! raw-metric-store example-metric-key-1 example-metric-value-1)
                       (m/set-raw-metric! raw-metric-store example-metric-key-2 example-metric-value-2)
                       (m/set-raw-metric! raw-metric-store example-metric-key-3 example-metric-value-3)
                       (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-4)

                       (t/is (= (m/make-metric-sample (m/metric-key-name                  example-metric-key-1)
                                                      (m/metric-key-labels                example-metric-key-1)
                                                      (m/metric-value-value               example-metric-value-1)
                                                      (m/metric-value-timestamp           example-metric-value-1)
                                                      (m/metric-value-last-update-time-ms example-metric-value-1))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key-1)))

                       (t/is (= (m/make-metric-sample (m/metric-key-name                  example-metric-key-2)
                                                      (m/metric-key-labels                example-metric-key-2)
                                                      (m/metric-value-value               example-metric-value-2)
                                                      (m/metric-value-timestamp           example-metric-value-2)
                                                      (m/metric-value-last-update-time-ms example-metric-value-2))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key-2)))

                       (t/is (= (m/make-metric-sample (m/metric-key-name                  example-metric-key-3)
                                                      (m/metric-key-labels                example-metric-key-3)
                                                      (m/metric-value-value               example-metric-value-3)
                                                      (m/metric-value-timestamp           example-metric-value-3)
                                                      (m/metric-value-last-update-time-ms example-metric-value-3))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key-3)))

                       (t/is (= (m/make-metric-sample (m/metric-key-name                  example-metric-key-4)
                                                      (m/metric-key-labels                example-metric-key-4)
                                                      (m/metric-value-value               example-metric-value-4)
                                                      (m/metric-value-timestamp           example-metric-value-4)
                                                      (m/metric-value-last-update-time-ms example-metric-value-4))
                                (m/get-raw-metric-sample! raw-metric-store example-metric-key-4)))
                       (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-5)
                       (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-6)
                       (let [example-metric-value-inced (+ (m/metric-value-value example-metric-value-4)
                                                           (m/metric-value-value example-metric-value-5)
                                                           (m/metric-value-value example-metric-value-6))]
                         (t/is (= (m/make-metric-sample (m/metric-key-name                  example-metric-key-4)
                                                        (m/metric-key-labels                example-metric-key-4)
                                                        example-metric-value-inced
                                                        (m/metric-value-timestamp           example-metric-value-6)
                                                        (m/metric-value-last-update-time-ms example-metric-value-6))
                                  (m/get-raw-metric-sample! raw-metric-store example-metric-key-4))))))))))

(t/deftest t-get-raw-metric-samples!
  (t/testing "Getting all raw-metric-store after simple set and inc operations works."
    (let [raw-metric-store (m/fresh-raw-metric-store)]
      (t/is (= [] (m/get-raw-metric-samples! raw-metric-store))))
    (t/is (quickcheck
           (property [[example-metric-key-1,
                       example-metric-key-2,
                       example-metric-key-3,
                       example-metric-key-4] (spec (m/gen-distinct-metric-keys 4))

                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)
                      example-metric-value-3 (spec ::m/metric-value)
                      example-metric-value-4 (spec ::m/metric-value)
                      example-metric-value-5 (spec ::m/metric-value)
                      example-metric-value-6 (spec ::m/metric-value)]
                     (let [raw-metric-store (m/fresh-raw-metric-store)]
                       (m/set-raw-metric! raw-metric-store example-metric-key-1 example-metric-value-1)

                       (t/is (= [(m/make-metric-sample
                                 (m/metric-key-name                  example-metric-key-1)
                                 (m/metric-key-labels                example-metric-key-1)
                                 (m/metric-value-value               example-metric-value-1)
                                 (m/metric-value-timestamp           example-metric-value-1)
                                 (m/metric-value-last-update-time-ms example-metric-value-1))]
                                (m/get-raw-metric-samples! raw-metric-store)))

                       (m/set-raw-metric! raw-metric-store example-metric-key-2 example-metric-value-2)
                       (m/set-raw-metric! raw-metric-store example-metric-key-3 example-metric-value-3)
                       (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-4)

                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key-1)
                                  (m/metric-key-labels                example-metric-key-1)
                                  (m/metric-value-value               example-metric-value-1)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1)),

                                 (m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key-2)
                                  (m/metric-key-labels                example-metric-key-2)
                                  (m/metric-value-value               example-metric-value-2)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2)),

                                 (m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key-3)
                                  (m/metric-key-labels                example-metric-key-3)
                                  (m/metric-value-value               example-metric-value-3)
                                  (m/metric-value-timestamp           example-metric-value-3)
                                  (m/metric-value-last-update-time-ms example-metric-value-3)),

                                 (m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key-4)
                                  (m/metric-key-labels                example-metric-key-4)
                                  (m/metric-value-value               example-metric-value-4)
                                  (m/metric-value-timestamp           example-metric-value-4)
                                  (m/metric-value-last-update-time-ms example-metric-value-4))]
                                (m/get-raw-metric-samples! raw-metric-store)))

                       (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-5)
                       (m/inc-raw-metric! raw-metric-store example-metric-key-4 example-metric-value-6)
                       (let [example-metric-value-inced (+ (m/metric-value-value example-metric-value-4)
                                                           (m/metric-value-value example-metric-value-5)
                                                           (m/metric-value-value example-metric-value-6))]

                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key-1)
                                  (m/metric-key-labels                example-metric-key-1)
                                  (m/metric-value-value               example-metric-value-1)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1)),

                                 (m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key-2)
                                  (m/metric-key-labels                example-metric-key-2)
                                  (m/metric-value-value               example-metric-value-2)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2)),

                                 (m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key-3)
                                  (m/metric-key-labels                example-metric-key-3)
                                  (m/metric-value-value               example-metric-value-3)
                                  (m/metric-value-timestamp           example-metric-value-3)
                                  (m/metric-value-last-update-time-ms example-metric-value-3)),

                                 (m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key-4)
                                  (m/metric-key-labels                example-metric-key-4)
                                  example-metric-value-inced
                                  (m/metric-value-timestamp           example-metric-value-6)
                                  (m/metric-value-last-update-time-ms example-metric-value-6))]
                                (m/get-raw-metric-samples! raw-metric-store))))))))))


;; -- COMMANDS on raw metrics

;; TODO missing empty get?
(t/deftest t-monadic-set-get
  (t/testing "monadic setting and getting works"
    (t/is (quickcheck
           (property [example-metric-key   (spec ::m/metric-key)
                      example-metric-value (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/set-raw-metric example-metric-key example-metric-value)
                             (m/get-raw-metric-sample example-metric-key)))]

                       (t/is (= (m/make-metric-sample
                                 (m/metric-key-name                  example-metric-key)
                                 (m/metric-key-labels                example-metric-key)
                                 (m/metric-value-value               example-metric-value)
                                 (m/metric-value-timestamp           example-metric-value)
                                 (m/metric-value-last-update-time-ms example-metric-value))
                                result))))))))

(t/deftest t-monadic-set-inc-get
  (t/testing "monadic setting, incrementing and getting works"
    (t/is (quickcheck
           (property [example-metric-key     (spec ::m/metric-key)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/set-raw-metric example-metric-key example-metric-value-1)
                             (m/inc-raw-metric example-metric-key example-metric-value-2)
                             (m/get-raw-metric-sample example-metric-key)))

                           example-metric-value-inced (+ (m/metric-value-value example-metric-value-1)
                                                         (m/metric-value-value example-metric-value-2))]

                         (t/is (= (m/make-metric-sample
                                   (m/metric-key-name                  example-metric-key)
                                   (m/metric-key-labels                example-metric-key)
                                   example-metric-value-inced
                                   (m/metric-value-timestamp           example-metric-value-2)
                                   (m/metric-value-last-update-time-ms example-metric-value-2))
                                  result))))))))

;; TODO more Tests!
;; TODO quickcheck?
(t/deftest t-monadic-prune-stale-metrics
  (t/testing "Simple pruning of metrics works."
    (let [result
          (mock-run-monad
           m/monad-command-config
           []
           (monad/monadic
            ;; empty nothing to prune
            (m/prune-stale-raw-metrics 200)
            (m/get-raw-metric-samples)
            ))]
      (t/is (= [] result)))

    (let [example-metric-key-1   (m/make-metric-key "t1" {:l1 :v1})
          example-metric-key-2   (m/make-metric-key "t2" {:l2 :v2})
          example-metric-key-3   (m/make-metric-key "t3" {:l3 :v3})
          example-metric-value-1 (m/make-metric-value 1 2 100)
          example-metric-value-2 (m/make-metric-value 2 3 200)
          example-metric-value-3 (m/make-metric-value 3 4 300)

          result
          (mock-run-monad
           m/monad-command-config
           []
           (monad/monadic

            (m/set-raw-metric example-metric-key-1 example-metric-value-1)
            (m/set-raw-metric example-metric-key-2 example-metric-value-2)
            (m/set-raw-metric example-metric-key-3 example-metric-value-3)

            ;; there is no value smaller than 50
            (m/prune-stale-raw-metrics 50)
            (m/get-raw-metric-samples)
            ))]
      (t/is (= [(m/make-metric-sample
                 (m/metric-key-name                  example-metric-key-1)
                 (m/metric-key-labels                example-metric-key-1)
                 (m/metric-value-value               example-metric-value-1)
                 (m/metric-value-timestamp           example-metric-value-1)
                 (m/metric-value-last-update-time-ms example-metric-value-1)),

                (m/make-metric-sample
                 (m/metric-key-name                  example-metric-key-2)
                 (m/metric-key-labels                example-metric-key-2)
                 (m/metric-value-value               example-metric-value-2)
                 (m/metric-value-timestamp           example-metric-value-2)
                 (m/metric-value-last-update-time-ms example-metric-value-2)),

                (m/make-metric-sample
                 (m/metric-key-name                  example-metric-key-3)
                 (m/metric-key-labels                example-metric-key-3)
                 (m/metric-value-value               example-metric-value-3)
                 (m/metric-value-timestamp           example-metric-value-3)
                 (m/metric-value-last-update-time-ms example-metric-value-3))]
               result)))

    (let [example-metric-key-1   (m/make-metric-key "t1" {:l1 :v1})
          example-metric-key-2   (m/make-metric-key "t2" {:l2 :v2})
          example-metric-key-3   (m/make-metric-key "t3" {:l3 :v3})
          example-metric-value-1 (m/make-metric-value 1 2 100)
          example-metric-value-2 (m/make-metric-value 2 3 200)
          example-metric-value-3 (m/make-metric-value 3 4 300)

          result
          (mock-run-monad
           m/monad-command-config
           []
           (monad/monadic

            (m/set-raw-metric example-metric-key-1 example-metric-value-1)
            (m/set-raw-metric example-metric-key-2 example-metric-value-2)
            (m/set-raw-metric example-metric-key-3 example-metric-value-3)

            ;; there is one value smaller than 200
            (m/prune-stale-raw-metrics 200)
            (m/get-raw-metric-samples)
            ))]
      (t/is (= [(m/make-metric-sample
                 (m/metric-key-name                  example-metric-key-2)
                 (m/metric-key-labels                example-metric-key-2)
                 (m/metric-value-value               example-metric-value-2)
                 (m/metric-value-timestamp           example-metric-value-2)
                 (m/metric-value-last-update-time-ms example-metric-value-2)),

                (m/make-metric-sample
                 (m/metric-key-name                  example-metric-key-3)
                 (m/metric-key-labels                example-metric-key-3)
                 (m/metric-value-value               example-metric-value-3)
                 (m/metric-value-timestamp           example-metric-value-3)
                 (m/metric-value-last-update-time-ms example-metric-value-3))]
               result)))))

;; TODO: indirectly tested?
(t/deftest t-monadic-run-metrics)

;; -- METRICS

;; TODO: test store with more entries (gen-counter-metric)
(t/deftest t-record-get-metric!
  (t/testing "Basic recording and getting counters works."
    (t/is (quickcheck
           (property [example-counter-metric  (spec ::m/counter-metric)
                      example-metric-value-1  (spec ::m/metric-value)
                      example-metric-value-2  (spec ::m/metric-value)]
                     (let [raw-metric-store   (m/fresh-raw-metric-store)
                           example-metric-key (m/counter-metric-key example-counter-metric)
                           example-metric-value-inced (+ (m/metric-value-value example-metric-value-1)
                                                         (m/metric-value-value example-metric-value-2))]
                       ;; TODO: do we really want to have here nil?
                       ;; empty metric store
                       (t/is (= [nil]
                                (m/get-metrics! raw-metric-store example-counter-metric)))
                       ;; record value-1
                       (m/record-metric! raw-metric-store example-counter-metric example-metric-value-1)
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-1)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                (m/get-metrics! raw-metric-store example-counter-metric)))
                       ;; record value-2 (=> value-1 + value-2)
                       (m/record-metric! raw-metric-store example-counter-metric example-metric-value-2)
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  example-metric-value-inced
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                (m/get-metrics! raw-metric-store example-counter-metric))))))))
  (t/testing "Basic recording and getting gauges works."
    (t/is (quickcheck
           (property [example-gauge-metric    (spec ::m/gauge-metric)
                      example-metric-value-1  (spec ::m/metric-value)
                      example-metric-value-2  (spec ::m/metric-value)]
                     (let [raw-metric-store   (m/fresh-raw-metric-store)
                           example-metric-key (m/gauge-metric-key example-gauge-metric)]
                       ;; TODO: do we really want to have here nil?
                       ;; empty metric store
                       (t/is (= [nil]
                                (m/get-metrics! raw-metric-store example-gauge-metric)))
                       ;; record value-1
                       (m/record-metric! raw-metric-store example-gauge-metric example-metric-value-1)
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-1)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                (m/get-metrics! raw-metric-store example-gauge-metric)))
                       ;; record value-2 (=> set value-2)
                       (m/record-metric! raw-metric-store example-gauge-metric example-metric-value-2)
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-2)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                (m/get-metrics! raw-metric-store example-gauge-metric))))))))
  (t/testing "Basic recording and getting histograms works."
    (t/is (quickcheck
           (property [example-histogram-metric (spec ::m/histogram-metric)
                      example-metric-value-1   (spec ::m/metric-value)
                      example-metric-value-2   (spec ::m/metric-value)]

                     (let [raw-metric-store                  (m/fresh-raw-metric-store)
                           example-total-sum-metric-key      (m/counter-metric-key (m/histogram-metric-total-sum example-histogram-metric))
                           example-total-sum-metric-key-name (m/metric-key-name example-total-sum-metric-key)
                           ;; To get the "basename" we need to remove the "_sum"
                           example-metric-key-basename       (subs example-total-sum-metric-key-name
                                                                   0
                                                                   (- (count example-total-sum-metric-key-name) 4))
                           ;; The labels of the total-sum counter has no added labels
                           example-metric-key-labels         (m/metric-key-labels example-total-sum-metric-key)
                           example-metric-value-1-value      (m/metric-value-value example-metric-value-1)
                           example-metric-value-2-value      (m/metric-value-value example-metric-value-2)
                           example-histogram-threshold       (m/histogram-metric-threshold example-histogram-metric)]
                       ;; TODO: do we really want to have here [nil nil nil nil]?
                       ;; empty metric store
                       (t/is (= [nil nil nil nil]
                                (m/get-metrics! raw-metric-store example-histogram-metric)))
                       ;; record value-1
                       (m/record-metric! raw-metric-store example-histogram-metric example-metric-value-1)
                       (t/is (= [(m/make-metric-sample
                                  (str example-metric-key-basename "_sum")
                                  example-metric-key-labels
                                  example-metric-value-1-value
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le "+Inf")
                                  1
                                  (m/metric-value-timestamp example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_count")
                                  example-metric-key-labels
                                  1
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le (str example-histogram-threshold))
                                  (if (<= example-metric-value-1-value example-histogram-threshold) 1 0)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                (m/get-metrics! raw-metric-store example-histogram-metric)))
                       (m/record-metric! raw-metric-store example-histogram-metric example-metric-value-2)
                       (t/is (= [(m/make-metric-sample
                                  (str example-metric-key-basename "_sum")
                                  example-metric-key-labels
                                  (+ example-metric-value-1-value example-metric-value-2-value)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le "+Inf")
                                  2
                                  (m/metric-value-timestamp example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_count")
                                  example-metric-key-labels
                                  2
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le (str example-histogram-threshold))
                                  (cond
                                    (and (<= example-metric-value-1-value example-histogram-threshold)
                                         (<= example-metric-value-2-value example-histogram-threshold)) 2
                                    (or  (<= example-metric-value-1-value example-histogram-threshold)
                                         (<= example-metric-value-2-value example-histogram-threshold)) 1
                                    :else                                                               0)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                (m/get-metrics! raw-metric-store example-histogram-metric))))))))
  ;; TODO: remove, keep, document? Testing some specific test cases explicitly.
  (t/testing "Some more specific recording and getting histograms works."
    (let [raw-metric-store         (m/fresh-raw-metric-store)
          example-histogram-metric (m/make-histogram-metric "test-histogram" 25 nil {:label-1 :value-1})]
      (m/record-metric! raw-metric-store example-histogram-metric (m/make-metric-value 23 1 500))
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 23 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  1 1 500)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  1 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  1 1 500)]
               (m/get-metrics! raw-metric-store example-histogram-metric)))
      (m/record-metric! raw-metric-store example-histogram-metric (m/make-metric-value 10 2 600))
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 33 2 600)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  2 2 600)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  2 2 600)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  2 2 600)]
               (m/get-metrics! raw-metric-store example-histogram-metric)))
      (m/record-metric! raw-metric-store example-histogram-metric (m/make-metric-value 25 3 700))
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 58 3 700)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  3 3 700)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  3 3 700)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  3 3 700)]
               (m/get-metrics! raw-metric-store example-histogram-metric)))
      (m/record-metric! raw-metric-store example-histogram-metric (m/make-metric-value 30 5 900))
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 88 5 900)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  4 5 900)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  4 5 900)
                ;; Timestamp gets updated, counter remains the same
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  3 5 900)]
               (m/get-metrics! raw-metric-store example-histogram-metric))))
    (let [raw-metric-store         (m/fresh-raw-metric-store)
          example-histogram-metric (m/make-histogram-metric "test-histogram" 20 nil {:label-1 :value-1})]
      (m/record-metric! raw-metric-store example-histogram-metric (m/make-metric-value 23 1 500))
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 23 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  1 1 500)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  1 1 500)
                ;; Bucket available but count is 0
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "20"  }  0 1 500)]
                (m/get-metrics! raw-metric-store example-histogram-metric))))))

(t/deftest t-monadic-record-get
  (t/testing "monadic recording and getting counters works"
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-counter-metric (spec ::m/counter-metric)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/get-metrics example-counter-metric)))]
                       (t/is (= [nil]
                                result))))))
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-counter-metric (spec ::m/counter-metric)
                      example-metric-value-1 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-metric example-counter-metric example-metric-value-1)
                             (m/get-metrics example-counter-metric)))
                           example-metric-key (m/counter-metric-key example-counter-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-1)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                result))))))
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-counter-metric (spec ::m/counter-metric)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-metric example-counter-metric example-metric-value-1)
                             (m/record-metric example-counter-metric example-metric-value-2)
                             (m/get-metrics example-counter-metric)))
                           example-metric-key (m/counter-metric-key example-counter-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (+ (m/metric-value-value example-metric-value-1)
                                     (m/metric-value-value example-metric-value-2))
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                result)))))))
  (t/testing "monadic recording and getting gauges works"
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-gauge-metric (spec ::m/gauge-metric)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/get-metrics example-gauge-metric)))]
                       (t/is (= [nil]
                                result))))))
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-gauge-metric (spec ::m/gauge-metric)
                      example-metric-value-1 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-metric example-gauge-metric example-metric-value-1)
                             (m/get-metrics example-gauge-metric)))
                           example-metric-key (m/gauge-metric-key example-gauge-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-1)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                result))))))
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-gauge-metric (spec ::m/gauge-metric)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-metric example-gauge-metric example-metric-value-1)
                             (m/record-metric example-gauge-metric example-metric-value-2)
                             (m/get-metrics example-gauge-metric)))
                           example-metric-key (m/gauge-metric-key example-gauge-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-2)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                result)))))))
  (t/testing "monadic recording and getting histograms works"
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-histogram-metric (spec ::m/histogram-metric)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/get-metrics example-histogram-metric)))]
                       (t/is (= [nil nil nil nil]
                                result))))))
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-histogram-metric (spec ::m/histogram-metric)
                      example-metric-value-1   (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-metric example-histogram-metric example-metric-value-1)
                             (m/get-metrics example-histogram-metric)))

                           example-total-sum-metric-key      (m/counter-metric-key (m/histogram-metric-total-sum example-histogram-metric))
                           example-total-sum-metric-key-name (m/metric-key-name example-total-sum-metric-key)
                           ;; To get the "basename" we need to remove the "_sum"
                           example-metric-key-basename       (subs example-total-sum-metric-key-name
                                                                   0
                                                                   (- (count example-total-sum-metric-key-name) 4))
                           ;; The labels of the total-sum counter has no added labels
                           example-metric-key-labels         (m/metric-key-labels example-total-sum-metric-key)
                           example-metric-value-1-value      (m/metric-value-value example-metric-value-1)
                           example-histogram-threshold       (m/histogram-metric-threshold example-histogram-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (str example-metric-key-basename "_sum")
                                  example-metric-key-labels
                                  example-metric-value-1-value
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le "+Inf")
                                  1
                                  (m/metric-value-timestamp example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_count")
                                  example-metric-key-labels
                                  1
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le (str example-histogram-threshold))
                                  (if (<= example-metric-value-1-value example-histogram-threshold) 1 0)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                result))))))
    (m/reset-global-raw-metric-store!)
    (t/is (quickcheck
           (property [example-histogram-metric (spec ::m/histogram-metric)
                      example-metric-value-1   (spec ::m/metric-value)
                      example-metric-value-2   (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-metric example-histogram-metric example-metric-value-1)
                             (m/record-metric example-histogram-metric example-metric-value-2)
                             (m/get-metrics example-histogram-metric)))

                           example-total-sum-metric-key      (m/counter-metric-key (m/histogram-metric-total-sum example-histogram-metric))
                           example-total-sum-metric-key-name (m/metric-key-name example-total-sum-metric-key)
                           ;; To get the "basename" we need to remove the "_sum"
                           example-metric-key-basename       (subs example-total-sum-metric-key-name
                                                                   0
                                                                   (- (count example-total-sum-metric-key-name) 4))
                           ;; The labels of the total-sum counter has no added labels
                           example-metric-key-labels         (m/metric-key-labels example-total-sum-metric-key)
                           example-metric-value-1-value      (m/metric-value-value example-metric-value-1)
                           example-metric-value-2-value      (m/metric-value-value example-metric-value-2)
                           example-histogram-threshold       (m/histogram-metric-threshold example-histogram-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (str example-metric-key-basename "_sum")
                                  example-metric-key-labels
                                  (+ example-metric-value-1-value example-metric-value-2-value)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le "+Inf")
                                  2
                                  (m/metric-value-timestamp example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_count")
                                  example-metric-key-labels
                                  2
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le (str example-histogram-threshold))
                                  (cond
                                    (and (<= example-metric-value-1-value example-histogram-threshold)
                                         (<= example-metric-value-2-value example-histogram-threshold)) 2
                                    (or  (<= example-metric-value-1-value example-histogram-threshold)
                                         (<= example-metric-value-2-value example-histogram-threshold)) 1
                                    :else                                                               0)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                result)))))))
  ;; TODO: remove, keep, document? Testing some specific test cases explicitly.
  (t/testing "Some more specific recording and getting histograms works."
    (m/reset-global-raw-metric-store!)
    (let [result
          (mock-run-monad
           m/monad-command-config
           []
           (let [example-histogram-metric (m/make-histogram-metric "test-histogram" 25 nil {:label-1 :value-1})]
             (monad/monadic
              (m/record-metric example-histogram-metric (m/make-metric-value 23 1 500))
              (m/record-metric example-histogram-metric (m/make-metric-value 10 2 600))
              (m/record-metric example-histogram-metric (m/make-metric-value 25 3 700))
              (m/get-metrics example-histogram-metric))))]
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 58 3 700)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  3 3 700)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  3 3 700)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  3 3 700)]
               result)))
    (m/reset-global-raw-metric-store!)
    (let [result
          (mock-run-monad
           m/monad-command-config
           []
           (let [example-histogram-metric (m/make-histogram-metric "test-histogram" 25 nil {:label-1 :value-1})]
             (monad/monadic
              (m/record-metric example-histogram-metric (m/make-metric-value 23 1 500))
              (m/record-metric example-histogram-metric (m/make-metric-value 10 2 600))
              (m/record-metric example-histogram-metric (m/make-metric-value 25 3 700))
              (m/record-metric example-histogram-metric (m/make-metric-value 30 5 900))
              (m/get-metrics example-histogram-metric))))]
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 88 5 900)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  4 5 900)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  4 5 900)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  3 5 900)]
               result)))
    (m/reset-global-raw-metric-store!)
    (let [result
          (mock-run-monad
           m/monad-command-config
           []
           (let [example-histogram-metric (m/make-histogram-metric "test-histogram" 20 nil {:label-1 :value-1})]
             (monad/monadic
              (m/record-metric example-histogram-metric (m/make-metric-value 23 1 500))
              (m/get-metrics example-histogram-metric))))]
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 23 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  1 1 500)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  1 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "20"  }  0 1 500)]
               result)))))

(t/deftest t-record-and-get!
  (t/testing "Basic recording and getting counters works."
    (t/is (quickcheck
           (property [example-counter-metric (spec ::m/counter-metric)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [raw-metric-store (m/reset-global-raw-metric-store!)
                           example-metric-key (m/counter-metric-key example-counter-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-1)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                (m/record-and-get! example-counter-metric example-metric-value-1)))
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (+ (m/metric-value-value               example-metric-value-1)
                                     (m/metric-value-value               example-metric-value-2))
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                (m/record-and-get! example-counter-metric example-metric-value-2))))))))
  (t/testing "Basic recording and getting gauges works."
    (t/is (quickcheck
           (property [example-gauge-metric   (spec ::m/gauge-metric)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [raw-metric-store (m/reset-global-raw-metric-store!)
                           example-metric-key (m/gauge-metric-key example-gauge-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-1)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                (m/record-and-get! example-gauge-metric example-metric-value-1)))
                       (t/is (= [(m/make-metric-sample
                                  (m/metric-key-name                  example-metric-key)
                                  (m/metric-key-labels                example-metric-key)
                                  (m/metric-value-value               example-metric-value-2)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                (m/record-and-get! example-gauge-metric example-metric-value-2))))))))

  (t/testing "Basic recording and getting histograms works."
    (t/is (quickcheck
           (property [example-histogram-metric (spec ::m/histogram-metric)
                      example-metric-value-1   (spec ::m/metric-value)
                      example-metric-value-2   (spec ::m/metric-value)]
                     (let [raw-metric-store (m/reset-global-raw-metric-store!)
                           example-total-sum-metric-key      (m/counter-metric-key (m/histogram-metric-total-sum example-histogram-metric))
                           example-total-sum-metric-key-name (m/metric-key-name example-total-sum-metric-key)
                           ;; To get the "basename" we need to remove the "_sum"
                           example-metric-key-basename       (subs example-total-sum-metric-key-name
                                                                   0
                                                                   (- (count example-total-sum-metric-key-name) 4))
                           ;; The labels of the total-sum counter has no added labels
                           example-metric-key-labels         (m/metric-key-labels example-total-sum-metric-key)
                           example-metric-value-1-value      (m/metric-value-value example-metric-value-1)
                           example-metric-value-2-value      (m/metric-value-value example-metric-value-2)
                           example-histogram-threshold       (m/histogram-metric-threshold example-histogram-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (str example-metric-key-basename "_sum")
                                  example-metric-key-labels
                                  example-metric-value-1-value
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le "+Inf")
                                  1
                                  (m/metric-value-timestamp example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_count")
                                  example-metric-key-labels
                                  1
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le (str example-histogram-threshold))
                                  (if (<= example-metric-value-1-value example-histogram-threshold) 1 0)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                (m/record-and-get! example-histogram-metric example-metric-value-1)))
                       (t/is (= [(m/make-metric-sample
                                  (str example-metric-key-basename "_sum")
                                  example-metric-key-labels
                                  (+ example-metric-value-1-value example-metric-value-2-value)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le "+Inf")
                                  2
                                  (m/metric-value-timestamp example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_count")
                                  example-metric-key-labels
                                  2
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le (str example-histogram-threshold))
                                  (cond
                                    (and (<= example-metric-value-1-value example-histogram-threshold)
                                         (<= example-metric-value-2-value example-histogram-threshold)) 2
                                    (or  (<= example-metric-value-1-value example-histogram-threshold)
                                         (<= example-metric-value-2-value example-histogram-threshold)) 1
                                    :else                                                               0)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                (m/record-and-get! example-histogram-metric example-metric-value-2))))))))
  ;; TODO: remove, keep, document? Testing some specific test cases explicitly.
  (t/testing "Some more specific recording and getting histograms works."
    (let [raw-metric-store         (m/reset-global-raw-metric-store!)
          example-histogram-metric (m/make-histogram-metric "test-histogram" 25 nil {:label-1 :value-1})]
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 23 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  1 1 500)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  1 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  1 1 500)]
               (m/record-and-get! example-histogram-metric (m/make-metric-value 23 1 500))))
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 33 2 600)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  2 2 600)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  2 2 600)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  2 2 600)]
               (m/record-and-get! example-histogram-metric (m/make-metric-value 10 2 600))))
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 58 3 700)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  3 3 700)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  3 3 700)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  3 3 700)]
               (m/record-and-get! example-histogram-metric (m/make-metric-value 25 3 700))))
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 88 5 900)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  4 5 900)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  4 5 900)
                ;; Timestamp gets updated, counter remains the same
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  3 5 900)]
               (m/record-and-get! example-histogram-metric (m/make-metric-value 30 5 900)))))
    (let [raw-metric-store         (m/reset-global-raw-metric-store!)
          example-histogram-metric (m/make-histogram-metric "test-histogram" 20 nil {:label-1 :value-1})]
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 23 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  1 1 500)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  1 1 500)
                ;; Bucket available but count is 0
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "20"  }  0 1 500)]
               (m/record-and-get! example-histogram-metric (m/make-metric-value 23 1 500)))))))

(t/deftest t-monadic-record-and-get
  (t/testing "monadic recording and getting counters works"
    (t/is (quickcheck
           (property [example-counter-metric (spec ::m/counter-metric)
                      example-metric-value-1 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-and-get example-counter-metric example-metric-value-1)))
                           example-metric-key (m/counter-metric-key example-counter-metric)]
                        (t/is (= [(m/make-metric-sample
                                   (m/metric-key-name                  example-metric-key)
                                   (m/metric-key-labels                example-metric-key)
                                   (m/metric-value-value               example-metric-value-1)
                                   (m/metric-value-timestamp           example-metric-value-1)
                                   (m/metric-value-last-update-time-ms example-metric-value-1))]
                                 result))))))
    (t/is (quickcheck
           (property [example-counter-metric (spec ::m/counter-metric)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-and-get example-counter-metric example-metric-value-1)
                             (m/record-and-get example-counter-metric example-metric-value-2)))
                           example-metric-key (m/counter-metric-key example-counter-metric)]
                        (t/is (= [(m/make-metric-sample
                                   (m/metric-key-name                  example-metric-key)
                                   (m/metric-key-labels                example-metric-key)
                                   (+ (m/metric-value-value               example-metric-value-1)
                                      (m/metric-value-value               example-metric-value-2))
                                   (m/metric-value-timestamp           example-metric-value-2)
                                   (m/metric-value-last-update-time-ms example-metric-value-2))]
                                 result)))))))
  (t/testing "monadic recording and getting gauges works"
    (t/is (quickcheck
           (property [example-gauge-metric   (spec ::m/gauge-metric)
                      example-metric-value-1 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-and-get example-gauge-metric example-metric-value-1)))
                           example-metric-key (m/gauge-metric-key example-gauge-metric)]
                        (t/is (= [(m/make-metric-sample
                                   (m/metric-key-name                  example-metric-key)
                                   (m/metric-key-labels                example-metric-key)
                                   (m/metric-value-value               example-metric-value-1)
                                   (m/metric-value-timestamp           example-metric-value-1)
                                   (m/metric-value-last-update-time-ms example-metric-value-1))]
                                 result))))))
    (t/is (quickcheck
           (property [example-gauge-metric   (spec ::m/gauge-metric)
                      example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-and-get example-gauge-metric example-metric-value-1)
                             (m/record-and-get example-gauge-metric example-metric-value-2)))
                           example-metric-key (m/gauge-metric-key example-gauge-metric)]
                        (t/is (= [(m/make-metric-sample
                                   (m/metric-key-name                  example-metric-key)
                                   (m/metric-key-labels                example-metric-key)
                                   (m/metric-value-value               example-metric-value-2)
                                   (m/metric-value-timestamp           example-metric-value-2)
                                   (m/metric-value-last-update-time-ms example-metric-value-2))]
                                 result)))))))

  (t/testing "monadic recording and getting histograms works"
    (t/is (quickcheck
           (property [example-histogram-metric (spec ::m/histogram-metric)
                      example-metric-value-1   (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-and-get example-histogram-metric example-metric-value-1)))

                           example-total-sum-metric-key      (m/counter-metric-key (m/histogram-metric-total-sum example-histogram-metric))
                           example-total-sum-metric-key-name (m/metric-key-name example-total-sum-metric-key)
                           ;; To get the "basename" we need to remove the "_sum"
                           example-metric-key-basename       (subs example-total-sum-metric-key-name
                                                                   0
                                                                   (- (count example-total-sum-metric-key-name) 4))
                           ;; The labels of the total-sum counter has no added labels
                           example-metric-key-labels         (m/metric-key-labels example-total-sum-metric-key)
                           example-metric-value-1-value      (m/metric-value-value example-metric-value-1)
                           example-histogram-threshold       (m/histogram-metric-threshold example-histogram-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (str example-metric-key-basename "_sum")
                                  example-metric-key-labels
                                  example-metric-value-1-value
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le "+Inf")
                                  1
                                  (m/metric-value-timestamp example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_count")
                                  example-metric-key-labels
                                  1
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le (str example-histogram-threshold))
                                  (if (<= example-metric-value-1-value example-histogram-threshold) 1 0)
                                  (m/metric-value-timestamp           example-metric-value-1)
                                  (m/metric-value-last-update-time-ms example-metric-value-1))]
                                 result))))))
    (t/is (quickcheck
           (property [example-histogram-metric (spec ::m/histogram-metric)
                      example-metric-value-1   (spec ::m/metric-value)
                      example-metric-value-2   (spec ::m/metric-value)]
                     (let [result
                           (mock-run-monad
                            m/monad-command-config
                            []
                            (monad/monadic
                             (m/record-and-get example-histogram-metric example-metric-value-1)
                             (m/record-and-get example-histogram-metric example-metric-value-2)))

                           example-total-sum-metric-key      (m/counter-metric-key (m/histogram-metric-total-sum example-histogram-metric))
                           example-total-sum-metric-key-name (m/metric-key-name example-total-sum-metric-key)
                           ;; To get the "basename" we need to remove the "_sum"
                           example-metric-key-basename       (subs example-total-sum-metric-key-name
                                                                   0
                                                                   (- (count example-total-sum-metric-key-name) 4))
                           ;; The labels of the total-sum counter has no added labels
                           example-metric-key-labels         (m/metric-key-labels example-total-sum-metric-key)
                           example-metric-value-1-value      (m/metric-value-value example-metric-value-1)
                           example-metric-value-2-value      (m/metric-value-value example-metric-value-2)
                           example-histogram-threshold       (m/histogram-metric-threshold example-histogram-metric)]
                       (t/is (= [(m/make-metric-sample
                                  (str example-metric-key-basename "_sum")
                                  example-metric-key-labels
                                  (+ example-metric-value-1-value example-metric-value-2-value)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le "+Inf")
                                  2
                                  (m/metric-value-timestamp example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_count")
                                  example-metric-key-labels
                                  2
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))
                                 (m/make-metric-sample
                                  (str example-metric-key-basename "_bucket")
                                  (assoc example-metric-key-labels :le (str example-histogram-threshold))
                                  (cond
                                    (and (<= example-metric-value-1-value example-histogram-threshold)
                                         (<= example-metric-value-2-value example-histogram-threshold)) 2
                                    (or  (<= example-metric-value-1-value example-histogram-threshold)
                                         (<= example-metric-value-2-value example-histogram-threshold)) 1
                                    :else                                                               0)
                                  (m/metric-value-timestamp           example-metric-value-2)
                                  (m/metric-value-last-update-time-ms example-metric-value-2))]
                                 result)))))))
  ;; TODO: remove, keep, document? Testing some specific test cases explicitly.
  (t/testing "Some more specific recording and getting histograms works."
    (let [result
          (mock-run-monad
           m/monad-command-config
           []
           (let [example-histogram-metric (m/make-histogram-metric "test-histogram" 25 nil {:label-1 :value-1})]
             (monad/monadic
              (m/record-and-get example-histogram-metric (m/make-metric-value 23 1 500))
              (m/record-and-get example-histogram-metric (m/make-metric-value 10 2 600))
              (m/record-and-get example-histogram-metric (m/make-metric-value 25 3 700)))))]
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 58 3 700)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  3 3 700)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  3 3 700)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  3 3 700)]
               result)))
    (let [result
          (mock-run-monad
           m/monad-command-config
           []
           (let [example-histogram-metric (m/make-histogram-metric "test-histogram" 25 nil {:label-1 :value-1})]
             (monad/monadic
              (m/record-and-get example-histogram-metric (m/make-metric-value 23 1 500))
              (m/record-and-get example-histogram-metric (m/make-metric-value 10 2 600))
              (m/record-and-get example-histogram-metric (m/make-metric-value 25 3 700))
              (m/record-and-get example-histogram-metric (m/make-metric-value 30 5 900)))))]
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 88 5 900)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  4 5 900)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  4 5 900)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "25"  }  3 5 900)]
               result)))
    (let [result
          (mock-run-monad
           m/monad-command-config
           []
           (let [example-histogram-metric (m/make-histogram-metric "test-histogram" 20 nil {:label-1 :value-1})]
             (monad/monadic
              (m/record-and-get example-histogram-metric (m/make-metric-value 23 1 500)))))]
      (t/is (= [(m/make-metric-sample "test-histogram_sum"    {:label-1 :value-1           } 23 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "+Inf"}  1 1 500)
                (m/make-metric-sample "test-histogram_count"  {:label-1 :value-1           }  1 1 500)
                (m/make-metric-sample "test-histogram_bucket" {:label-1 :value-1 :le "20"  }  0 1 500)]
               result)))))

;; DESTRUCTIVE

;; -- DATA: raw metrics

(t/deftest t-d-make-metric-key
  (t/testing "No metric-key field must be nil"
    (t/is (thrown? Exception (m/make-metric-key nil {:label-1 :value-1})))
    (t/is (thrown? Exception (m/make-metric-key "test-metric" nil)))))

(t/deftest t-d-make-metric-value)
(t/deftest t-d-make-metric-sample)

(t/deftest t-d-update-metric-value
  (t/testing "Nil as arguments."
    (let [example-metric-value-1 (m/make-metric-value 23 1 500)]
      (t/is (thrown? Exception (m/update-metric-value + example-metric-value-1 nil))
            "metric-value-2 must not be nil by spec."))
    (t/testing "The function may not be nil by spec."
      (let [example-metric-value-1 (m/make-metric-value 23 1 500)
            example-metric-value-2 (m/make-metric-value  1 2 600)]
        (t/is (thrown? Exception (m/update-metric-value nil example-metric-value-1 example-metric-value-2)))))))

(t/deftest t-d-sum-metric-value
  (t/testing "Adds metric-values and nothing else."
    (let [example-metric-value-1 (m/make-metric-value 23 1 500)]
      (t/is (thrown? Exception (m/sum-metric-value example-metric-value-1 nil))
            "metric-value-2 must not be nil by spec."))))

;; active-quickcheck?
(t/deftest t-d-inc-raw-metric!
  (t/testing "Increasing raw metric with included nils."
    (let [raw-metric-store     nil
          example-metric-key   (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value (m/make-metric-value 23 1 500)]
      ;; By specs
      (t/is (thrown? Exception
                     (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value))))
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key   nil
          example-metric-value (m/make-metric-value 23 1 500)]
      ;; TODO: Is there a reason why empty metric-keys should be accepted?
      ;; TODO: The behaviour that we are testing is not the behaviour that will be seen?
      (t/is (thrown? Exception
                     (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value)))
      #_(t/is (empty? (m/get-raw-metric-samples! (m/fresh-raw-metric-store)))))
    (let [raw-metric-store     (m/fresh-raw-metric-store)
          example-metric-key   (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value nil]
      ;; TODO: What is the expected behaviour here?
      (t/is (thrown? Exception (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value)))
      (t/is (empty? (m/get-raw-metric-samples! (m/fresh-raw-metric-store)))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 nil})
          example-metric-value-1 (m/make-metric-value 23 1 500)
          example-metric-value-2 (m/make-metric-value 10 2 600)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 nil} 33 2 600)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 nil 500)
          example-metric-value-2 (m/make-metric-value 10   2 600)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 2 600)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))))
    (let [raw-metric-store       (m/fresh-raw-metric-store)
          example-metric-key     (m/make-metric-key "test-metric" {:label-1 :value-1})
          example-metric-value-1 (m/make-metric-value 23 1 500)
          example-metric-value-2 (m/make-metric-value 10 nil 600)]
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-1)
      (m/inc-raw-metric! raw-metric-store example-metric-key example-metric-value-2)
      (t/is (= (m/make-metric-sample "test-metric" {:label-1 :value-1} 33 nil 600)
               (m/get-raw-metric-sample! raw-metric-store example-metric-key))))))

(t/deftest t-d-get-raw-metric-sample!
  (t/testing "Nil as arguments is against the specs."
    (let [example-metric-key (m/make-metric-key "test-metric" {:label-1 :value-1})]
      (t/is (thrown? Exception (m/get-raw-metric-sample! nil example-metric-key))))
    (let [raw-metric-store (m/fresh-raw-metric-store)]
      (t/is (thrown? Exception (m/get-raw-metric-sample! raw-metric-store nil))))))

(t/deftest t-d-get-raw-metric-samples!
  (t/testing "Nil as argument is against the specs."
    (t/is (thrown? Exception (m/get-raw-metric-samples! nil)))))
