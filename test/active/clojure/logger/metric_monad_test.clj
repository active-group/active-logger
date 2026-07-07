(ns active.clojure.logger.metric-monad-test
  (:require [active.clojure.logger.metric-monad :as m]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]

            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]
            
            [clojure.test :as t]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest])
  (:use [active.quickcheck]))

(t/use-fixtures :once
  (fn [f]
    (stest/instrument)
    (s/check-asserts true)
    (f)
    (s/check-asserts false)
    (stest/unstrument)))

(defn run [m]
  (first (m/run-metrics nil nil nil m)))

(t/deftest t-record-metric
  (t/testing "record-metric calls record-metric!"
    (let [call (atom nil)]
      (with-redefs [metric-accumulator/record-metric! (fn [& args]
                                                        (reset! call args))]
        (run (m/record-metric (metric-types/make-counter-metric "" "") {} 42.0 1))
        (t/is (= @call [(metric-types/make-counter-metric "" "") {} 42.0 1]))))))

(t/deftest t-get-metric-samples
  (t/testing "get-metric-samples calls get-metric-samples!"
    (let [call (atom nil)]
      (with-redefs [metric-accumulator/get-metric-samples! (fn [& args]
                                                        (reset! call args))]
        (run (m/get-metric-samples (metric-types/make-counter-metric "" "") {}))
        (t/is (= @call [(metric-types/make-counter-metric "" "") {}]))))))

(t/deftest t-get-all-metric-sample-sets
  (t/testing "get-all-metric-sample-sets calls get-all-metric-sample-sets!"
    (let [call (atom :foo)]
      (with-redefs [metric-accumulator/get-all-metric-sample-sets! (fn [& args]
                                                                     (reset! call args))]
        (run (m/get-all-metric-sample-sets))
        (t/is (= @call nil))))))

(t/deftest t-prune-stale-metrics
  (t/testing "prune-stale-metrics calls prune-stale-metrics!"
    (let [call (atom nil)]
      (with-redefs [metric-accumulator/prune-stale-metrics! (fn [& args]
                                                              (reset! call args))]
        (run (m/prune-stale-metrics 42))
        (t/is (= @call [42]))))))
