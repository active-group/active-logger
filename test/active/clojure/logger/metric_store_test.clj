(ns active.clojure.logger.metric-store-test
  (:require [active.clojure.logger.metric-store :as m]
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

(t/deftest t-record-metric
  (t/testing "record-metric records something"
    
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labels (spec ::metric-types/metric-labels)
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [store (m/fresh-metric-store)]
                       (not= (m/get-all-snapshots store)
                             (-> store
                                 (m/record-metric metric labels value time-ms)
                                 (m/get-all-snapshots)))))))))

(t/deftest t-store-pruning
  (t/testing "find-stale-metrics finds stale metrics"
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labelss (spec (s/coll-of ::metric-types/metric-labels :distinct true :count 2))
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     false
                     (let [[stale-labels not-stale-labels] labelss
                           store (dosync (-> (m/fresh-metric-store)
                                             (m/record-metric metric stale-labels value time-ms)
                                             (m/record-metric metric not-stale-labels value (+ 2 time-ms))))]
                       (= [[metric stale-labels time-ms]]
                          (m/find-stale-metrics store (+ 1 time-ms))))))))
  (t/testing "removing a metric storage"
    (let [m1 (metric-types/make-counter-metric "a" "")
          labels {:label 1}
          store (dosync (-> (m/fresh-metric-store)
                            (m/record-metric m1 labels 0.0 0)))]
      (t/is (empty? (m/get-all-snapshots (dosync (m/maybe-remove-metric store m1 labels 0))))
            "removed if timestamps match")
      (t/is (nil? (dosync (m/maybe-remove-metric store m1 labels 1)))
            "not removed if not match")
      (t/is (nil? (dosync (m/maybe-remove-metric store m1 {:foo "bar"} 0)))
            "nothing removed with other labels")
      (t/is (nil? (dosync (m/maybe-remove-metric store (metric-types/make-counter-metric "other" "") labels 0)))
            "nothing removed with other metric"))))

(t/deftest t-get-metrics-and-labels-count
  (t/is (= [0 0] (m/get-metrics-and-labels-count (m/fresh-metric-store))))

  (let [m1 (metric-types/make-counter-metric "a" "")
        m2 (metric-types/make-gauge-metric "b" "")]
    (= [2 3]
       (m/get-metrics-and-labels-count
        (dosync (-> (m/fresh-metric-store)
                    (m/record-metric m1 {:label 1} 0.0 0)
                    (m/record-metric m1 {:label 2} 0.0 0)
                    (m/record-metric m2 {:label 1} 0.0 0)))))))
