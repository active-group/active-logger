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

;; TODO: `list-distinct` is not necessary, but with `tgen/list` `NullPointerException` appear
(defn gen-metric-values
  [num-elems]
  (s/spec (s/coll-of ::m/metric-value :into [])
          :gen (fn []
                 (tgen/list-distinct (s/gen ::m/metric-value) {:num-elements num-elems}))))

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
  (t/testing "All fields of a gauge-metric are set correct."
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

(t/deftest t-make-counter-metric
  (t/testing "All fields of a counter-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::m/metric-name)
                      help              (spec ::m/help)
                      labels            (spec (gen-distinct-metric-labels 5))
                      values            (spec (gen-metric-values 5))]
                     (let [labels-values-map (zipmap labels values)
                           example-counter-metric (m/make-counter-metric metric-name
                                                                         help
                                                                         labels-values-map)]
                       (t/is                      (m/counter-metric?                  example-counter-metric))
                       (t/is (= metric-name       (m/counter-metric-name              example-counter-metric)))
                       (t/is (= help              (m/counter-metric-help              example-counter-metric)))
                       (t/is (= labels-values-map (m/counter-metric-labels-values-map example-counter-metric)))))))))

;; TODO: make this quickcheck
(t/deftest t-make-histogram-metric
  (t/testing "All fields of a histogram-metric are set correct."
    (let [metric-name "metric-name"
          help        "help"
          threshold   1
          map-sum     {{:label-1 1} (m/make-metric-value 1 0.5)}
          map-count   {{:label-1 1} (m/make-metric-value 1 0.5)}
          map-bucket  {{:label-1 0} (m/make-metric-value 0 0.5)}
          example-histogram-metric (m/make-histogram-metric metric-name
                                                            help
                                                            threshold
                                                            map-sum
                                                            map-count
                                                            map-bucket)]
      (t/is             (m/histogram-metric? example-histogram-metric))
      (t/is metric-name (m/histogram-metric-name                     example-histogram-metric))
      (t/is help        (m/histogram-metric-help                     example-histogram-metric))
      (t/is threshold   (m/histogram-metric-threshold                example-histogram-metric))
      (t/is map-sum     (m/histogram-metric-labels-values-map-sum    example-histogram-metric))
      (t/is map-count   (m/histogram-metric-labels-values-map-count  example-histogram-metric))
      (t/is map-bucket  (m/histogram-metric-labels-values-map-bucket example-histogram-metric)))))

;; set-metric-value

;; TODO: make this quickcheck
(t/deftest t-set-metric-value
  (t/testing "Setting a metric value works."
    (let [[labels & old-labelss] [{:a "a"} {:b "b"} {:c "c"} {:d "d"} {:e "e"} {:f "f"}]
          [value  & old-values ] [(m/make-metric-value -1 -1.5)
                                  (m/make-metric-value 0 0.5)
                                  (m/make-metric-value 1 1.5)
                                  (m/make-metric-value 2 2.5)
                                  (m/make-metric-value 3 3.5)
                                  (m/make-metric-value 4 4.5)]
          map-empty {}
          map-prefilled (zipmap old-labelss old-values)
          labels-to-change (nth old-labelss 3)]
      (t/is {labels value} (m/set-metric-value map-empty labels value))
      (t/is {(nth old-labelss 0) (nth old-values 0)
             (nth old-labelss 1) (nth old-values 1)
             (nth old-labelss 2) (nth old-values 2)
             labels              value
             (nth old-labelss 4) (nth old-values 4)}
            (m/set-metric-value map-prefilled labels-to-change value)))))

(t/deftest t-update-metric-value
  ;; TODO: More functions? Not only '+'?
  (t/testing "Basic update of metric-values works."
    (t/is (quickcheck
           (property [example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     ;; TODO: Can we add the nil to the specs? (s/or nil? (spec ::m/metric-value))
                     ;; If there is no base value, just take the second.
                     (t/is (= example-metric-value-2
                              (m/update-metric-value + nil example-metric-value-2)))
                     ;; Add value-1 and value-2. Take update-time from 2.
                     (let [value-1       (m/metric-value-value               example-metric-value-1)
                           value-2       (m/metric-value-value               example-metric-value-2)
                           update-time-2 (m/metric-value-last-update-time-ms example-metric-value-2)]
                     (t/is (= (m/make-metric-value (+ value-1 value-2) update-time-2)
                              (m/update-metric-value + example-metric-value-1 example-metric-value-2)))))))))

(t/deftest t-inc-metric-value
  (t/testing "Increment of metric-values in a labels-value-map works."
    (t/is (quickcheck
           (property [[labels & old-labelss]            (spec (gen-distinct-metric-labels 6))
                      [value  & old-values ]            (spec (gen-metric-values 6))]
                     (let [map-empty {}
                           map-prefilled (zipmap old-labelss old-values)
                           labels-to-change (nth old-labelss 3)
                           value-to-change  (nth old-values  3)]

                       (t/is (= {labels value}
                                (m/inc-metric-value map-empty labels value)))

                       (t/is (= {(nth old-labelss 0) (nth old-values 0)
                                 (nth old-labelss 1) (nth old-values 1)
                                 (nth old-labelss 2) (nth old-values 2)
                                 labels-to-change  (m/make-metric-value
                                                      (+ (m/metric-value-value value-to-change)
                                                         (m/metric-value-value value))
                                                      (m/metric-value-last-update-time-ms value))
                                 (nth old-labelss 4) (nth old-values 4)
                                 }
                                (m/inc-metric-value map-prefilled labels-to-change value)))))))))




;; update-gauge-metric
;; update-counter-metric
;; update-histogram-metric

;; update-metric
