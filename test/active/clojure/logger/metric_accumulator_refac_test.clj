(ns active.clojure.logger.metric-accumulator-refac-test
  (:require [active.clojure.logger.metric-accumulator-refac :as m]
            [clojure.test :as t]
            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]

            [clojure.spec.alpha :as s]

            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.generators :as tgen])
  (:use [active.quickcheck]))

;; (t/use-fixtures :each (fn [f] (m/reset-global-raw-metric-store!) (f)))

(defmacro mock-run-monad
  [& ?args]
  `(do
     (m/reset-global-raw-metric-store!)
     (mock-monad/mock-run-monad ~@?args)))

(stest/instrument)

;; GENERATORS

(defn gen-distinct-metric-labels
  [num-elems]
  (s/spec (s/coll-of ::m/metric-labels :into [])
          :gen (fn []
                 (tgen/list-distinct (s/gen ::m/metric-labels) {:num-elements num-elems}))))

(defn gen-metric-values
  [num-elems]
(s/spec (s/coll-of ::m/metric-value :into [])
          :gen (fn []
                 (tgen/list (s/gen ::metric-value) {:num-elements num-elems}))))

;; -- DATA: raw metrics

;; fresh-metric-store-map
;; fresh-raw-metric-store
;; set-global-raw-metric-store!
;; reset-global-raw-metric-store!

(t/deftest t-make-metric-value
  (t/testing "All fields of a metric-value are set correct."
    (t/is (quickcheck
           (property [value       (spec ::m/metric-value-value)
                      update-time (spec ::m/metric-value-last-update-time-ms)]
                     (let [example-metric-value (m/make-metric-value value update-time)]
                       (t/is                (m/metric-value?                    example-metric-value))
                       (t/is (= value       (m/metric-value-value               example-metric-value)))
                       (t/is (= update-time (m/metric-value-last-update-time-ms example-metric-value)))))))))

;; TODO: better creation of labels-values-map
(t/deftest t-make-gauge-metric
  (t/testing "All field of a gauge-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::m/metric-name)
                      help              (spec ::m/help)
                      labels            (spec (gen-distinct-metric-labels 5))
                      values            (spec (gen-metric-values 5))]
                     (let [labels-values-map (zipmap labels values)
                           example-gauge-metric (m/make-gauge-metric metric-name
                                                                     help
                                                                     labels-values-map)]
                       (t/is                      (m/gauge-metric?                  example-gauge-metric))
                       (t/is (= metric-name       (m/gauge-metric-name              example-gauge-metric)))
                       (t/is (= help              (m/gauge-metric-help              example-gauge-metric)))
                       (t/is (= labels-values-map (m/gauge-metric-labels-values-map example-gauge-metric)))))))))

;; make-counter-metric
;; make-histogram-metric

;; set-metric-value
;; update-metric-value'
;; update-metric-value
;; sum-metric-value
;; inc-metric-value

;; update-gauge-metric
;; update-counter-metric
;; update-histogram-metric

;; update-metric
