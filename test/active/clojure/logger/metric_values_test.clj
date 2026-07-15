(ns active.clojure.logger.metric-values-test
  (:require [active.clojure.logger.metric-values :as m]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-histogram-value :as histogram]
            [active.clojure.logger.metric-singular-value :as singular]
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

(t/deftest t-update-or-make-stored-values
  ;; Note: might be unstable for histogram metrics - maybe because of certain combinartions of thresholds and values.
  (t/testing "Making a fresh stored-values stores something for the given labels, but not others."
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labels (spec ::metric-types/metric-labels)
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [stored-values (dosync (m/update-or-make-stored-values nil metric labels value time-ms))]
                       (and (not (empty? (m/get-stored-values-snapshot stored-values labels)))
                            (contains? (m/get-all-stored-values-snapshot stored-values) labels))))))
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labelss (spec (s/coll-of ::metric-types/metric-labels :distinct true :count 2))
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [[labels other-labels] labelss
                           stored-values (dosync (m/update-or-make-stored-values nil metric labels value time-ms))]
                       (empty? (m/get-stored-values-snapshot stored-values other-labels)))))))
  
  (t/testing "update-or-make-stored-values stores correct counter values"
    (let [metric (metric-types/make-counter-metric "foo" "bar")
          labels {:a "baz"}
          value1  42.0
          value2  11.0
          time-ms 1]
      (t/is (= (singular/make-metric-value (+ value1 value2) (inc time-ms))
               (m/get-stored-values-snapshot (dosync (-> nil
                                                         (m/update-or-make-stored-values metric labels value1 time-ms)
                                                         (m/update-or-make-stored-values metric labels value2 (inc time-ms))))
                                             labels)))))
  (t/testing "update-or-make-stored-values stores correct gauge values"
    (let [metric (metric-types/make-gauge-metric "foo" "bar")
          labels {:a "baz"}
          value  42.0
          time-ms 1]
      (t/is (= (singular/make-metric-value value (inc time-ms))
               (m/get-stored-values-snapshot (dosync (-> nil
                                                         (m/update-or-make-stored-values metric labels -1.0 time-ms)
                                                         (m/update-or-make-stored-values metric labels value (inc time-ms))))
                                             labels)))))
  (t/testing "update-or-make-stored-values stores correct histogram values"
    ;; Note: more thorough tests in metric-histogram-values-test
    (let [metric (metric-types/make-histogram-metric "foo" "bar" [0 100])
          labels {:a "baz"}
          value1  42.0
          value2 11.0
          time-ms 1]
      (t/is (= (histogram/make-histogram-metric-values (inc time-ms) (+ value1 value2) 2 [0 2])
               (m/get-stored-values-snapshot (dosync (-> nil
                                                         (m/update-or-make-stored-values metric labels value1 time-ms)
                                                         (m/update-or-make-stored-values metric labels value2 (inc time-ms))))
                                             labels))))))

(t/deftest t-pruning-values
  (t/testing "find-stale-labels lists stale entries"
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [labels1 {:foo "bar"}
                           labels2 {:bar "baz"}
                         
                           value 42.0
                           stored-values (dosync (-> nil
                                                     (m/update-or-make-stored-values metric labels1 value time-ms)
                                                     (m/update-or-make-stored-values metric labels2 value (inc time-ms))))
                           pruned (-> nil
                                      (m/update-or-make-stored-values metric labels2 value (inc time-ms)))]
                       (= [[labels1 time-ms]]
                          (m/find-stale-labels stored-values (inc time-ms))))))))

  (t/testing "maybe-remove-values removes matching entries"
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labels (spec ::metric-types/metric-labels)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [stored-values (dosync (m/update-or-make-stored-values nil metric labels 42.0 time-ms))]
                       (m/empty-stored-values? (dosync (m/maybe-remove-values stored-values labels time-ms))))))))

  (t/testing "maybe-remove-values does not remove other entries"
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labels (spec ::metric-types/metric-labels)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [stored-values (dosync (m/update-or-make-stored-values nil metric labels 42.0 time-ms))]
                       (nil? (dosync (m/maybe-remove-values stored-values labels (inc time-ms))))))))))
