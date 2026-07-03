(ns active.clojure.logger.metric-values-test
  (:require [active.clojure.logger.metric-values :as metric-values]
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

;; 1. Gauges

(t/deftest t-make-gauge-metric
  (t/testing "All fields of a gauge-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::metric-types/metric-name)
                      help              (spec ::metric-types/metric-help)]
                     (let [example-gauge-metric (metric-types/make-gauge-metric metric-name help)]
                       (t/is                      (metric-types/gauge-metric?                  example-gauge-metric))
                       (t/is (= metric-name       (metric-types/gauge-metric-name              example-gauge-metric)))
                       (t/is (= help              (metric-types/gauge-metric-help              example-gauge-metric)))))))))


;; 2. Counters

(t/deftest t-make-counter-metric
  (t/testing "All fields of a counter-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::metric-types/metric-name)
                      help              (spec ::metric-types/metric-help)]
                     (let [example-counter-metric (metric-types/make-counter-metric metric-name help)]
                       (t/is                      (metric-types/counter-metric?                  example-counter-metric))
                       (t/is (= metric-name       (metric-types/counter-metric-name              example-counter-metric)))
                       (t/is (= help              (metric-types/counter-metric-help              example-counter-metric)))))))))


;; 3. Histograms

(t/deftest t-make-histogram-metric
  (t/testing "All fields of a histogram-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::metric-types/metric-name)
                      help              (spec ::metric-types/metric-help)
                      threshold         (spec ::metric-types/threshold)]
                     (let [example-histogram-metric (metric-types/make-histogram-metric metric-name help [threshold])]
                       (t/is                      (metric-types/histogram-metric? example-histogram-metric))
                       (t/is (= metric-name       (metric-types/histogram-metric-name                     example-histogram-metric)))
                       (t/is (= help              (metric-types/histogram-metric-help                     example-histogram-metric)))
                       (t/is (= [threshold]       (metric-types/histogram-metric-thresholds               example-histogram-metric)))))))))

(t/deftest t-metric-type
  (t/testing "Getting the metric type as string works."
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::gen/gauge-metric)
                      example-counter-metric   (spec ::gen/counter-metric)
                      example-histogram-metric (spec ::gen/histogram-metric)]
                     (t/is (= :gauge     (m/metric-type example-gauge-metric)))
                     (t/is (= :counter   (m/metric-type example-counter-metric)))
                     (t/is (= :histogram (m/metric-type example-histogram-metric))))))))

(t/deftest t-metric-name
  (t/testing "Getting the metric name works."
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::gen/gauge-metric)
                      example-counter-metric   (spec ::gen/counter-metric)
                      example-histogram-metric (spec ::gen/histogram-metric)]
                     (t/is (= (m/gauge-metric-name example-gauge-metric)
                              (m/metric-name example-gauge-metric)))

                     (t/is (= (m/counter-metric-name example-counter-metric)
                              (m/metric-name example-counter-metric)))

                     (t/is (= (m/histogram-metric-name example-histogram-metric)
                              (m/metric-name example-histogram-metric))))))))

(t/deftest t-metric-help
  (t/testing "Getting the metric help works."
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::gen/gauge-metric)
                      example-counter-metric   (spec ::gen/counter-metric)
                      example-histogram-metric (spec ::gen/histogram-metric)]
                     (t/is (= (m/gauge-metric-help example-gauge-metric)
                              (m/metric-help example-gauge-metric)))

                     (t/is (= (m/counter-metric-help example-counter-metric)
                              (m/metric-help example-counter-metric)))

                     (t/is (= (m/histogram-metric-help example-histogram-metric)
                              (m/metric-help example-histogram-metric))))))))


