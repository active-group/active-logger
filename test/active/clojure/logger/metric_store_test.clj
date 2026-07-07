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

(t/deftest t-prune-stale-metrics
  (t/testing "prune-stale-metrics prunes stale entries"
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labelss (spec (s/coll-of ::metric-types/metric-labels :distinct true :count 2))
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [store (m/fresh-metric-store)
                           [labels1 labels2] labelss]
                       (= (-> store
                              (m/record-metric metric labels2 value (inc time-ms))
                              (m/get-all-snapshots))
                          (-> store
                              (m/record-metric metric labels1 value time-ms)
                              (m/record-metric metric labels2 value (inc time-ms))
                              (m/prune-stale-metrics (inc time-ms))
                              (m/get-all-snapshots)))))))))
