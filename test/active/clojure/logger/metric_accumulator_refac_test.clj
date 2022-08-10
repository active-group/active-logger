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

(defn gen-distinct-metric-names
  [num-elems]
  (s/spec (s/coll-of ::m/metric-name :into [])
          :gen (fn []
                 (tgen/list-distinct (s/gen ::m/metric-name) {:num-elements num-elems}))))

;; TODO: Does not need to be distinct
(defn gen-metric-helps
  [num-elems]
   (s/spec (s/coll-of ::m/metric-help :into [])
           :gen (fn []
                  (tgen/list-distinct (s/gen ::m/metric-help) {:num-elements num-elems}))))

(defn gen-distinct-metric-labels
  [num-elems]
  (s/spec (s/coll-of ::m/metric-labels :into [])
          :gen (fn []
                 (tgen/list-distinct (s/gen ::m/metric-labels) {:num-elements num-elems}))))

;; TODO: Does not need to be distinct
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
                      help              (spec ::m/metric-help)
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
                      help              (spec ::m/metric-help)
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

(t/deftest t-make-histogram-metric
  (t/testing "All fields of a histogram-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::m/metric-name)
                      help              (spec ::m/metric-help)
                      threshold         (spec ::m/metric-value-value)
                      labels            (spec (gen-distinct-metric-labels 5))
                      values            (spec (gen-metric-values          5))]
                     (let [labels-values-map (zipmap labels values)
                           example-histogram-metric (m/make-histogram-metric metric-name
                                                                             help
                                                                             threshold
                                                                             labels-values-map
                                                                             labels-values-map
                                                                             labels-values-map)]
                       (t/is                      (m/histogram-metric? example-histogram-metric))
                       (t/is (= metric-name       (m/histogram-metric-name                     example-histogram-metric)))
                       (t/is (= help              (m/histogram-metric-help                     example-histogram-metric)))
                       (t/is (= threshold         (m/histogram-metric-threshold                example-histogram-metric)))
                       (t/is (= labels-values-map (m/histogram-metric-labels-values-map-sum    example-histogram-metric)))
                       (t/is (= labels-values-map (m/histogram-metric-labels-values-map-count  example-histogram-metric)))
                       (t/is (= labels-values-map (m/histogram-metric-labels-values-map-bucket example-histogram-metric)))))))))

(t/deftest t-make-metric-sample
  (t/testing "All fields of a metric-sample are set correct."
    (t/is (quickcheck
           (property [metric-name (spec ::m/metric-name)
                      labels      (spec ::m/metric-labels)
                      value       (spec ::m/metric-value-value)
                      timestamp   (spec ::m/metric-value-last-update-time-ms)]
                     (let [example-metric-sample (m/make-metric-sample metric-name
                                                                       labels
                                                                       value
                                                                       timestamp)]
                       (t/is                (m/metric-sample?          example-metric-sample))
                       (t/is (= metric-name (m/metric-sample-name      example-metric-sample)))
                       (t/is (= labels      (m/metric-sample-labels    example-metric-sample)))
                       (t/is (= value       (m/metric-sample-value     example-metric-sample)))
                       (t/is (= timestamp   (m/metric-sample-timestamp example-metric-sample)))))))))

(t/deftest t-set-metric-value
  (t/testing "Setting a metric value works."
    (t/is (quickcheck
           (property [[labels & labelss]           (spec (gen-distinct-metric-labels 6))
                      [value  & values ]           (spec (gen-metric-values          6))]
                     (let [map-empty {}
                           map-prefilled (zipmap labelss values)
                           labels-to-change (nth labelss 3)]

                       (t/is (= {labels value} (m/set-metric-value map-empty labels value)))
                       (t/is (= {(nth labelss 0)   (nth values 0)
                                 (nth labelss 1)   (nth values 1)
                                 (nth labelss 2)   (nth values 2)
                                 labels-to-change  value
                                 (nth labelss 4)   (nth values 4)}
                                (m/set-metric-value map-prefilled labels-to-change value)))))))))

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
           (property [[labels & labelss] (spec (gen-distinct-metric-labels 6))
                      [value  & values ] (spec (gen-metric-values          6))]
                     (let [map-empty {}
                           map-prefilled (zipmap labelss values)
                           labels-to-change (nth labelss 3)
                           value-to-change  (nth values  3)]

                       (t/is (= {labels value}
                                (m/inc-metric-value map-empty labels value)))

                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 labels-to-change  (m/make-metric-value
                                                      (+ (m/metric-value-value value-to-change)
                                                         (m/metric-value-value value))
                                                      (m/metric-value-last-update-time-ms value))
                                 (nth labelss 4) (nth values 4)}
                                (m/inc-metric-value map-prefilled labels-to-change value)))))))))

(t/deftest t-update-gauge-metric
  (t/testing "Update of gauge-metric works."
    (t/is (quickcheck
           (property [metric-name  (spec ::m/metric-name)
                      help         (spec ::m/metric-help)
                      [labels & labelss] (spec (gen-distinct-metric-labels 6))
                      [value  & values ] (spec (gen-metric-values          6))]
                     (let [map-empty {}
                           map-prefilled (zipmap labelss values)
                           labels-to-change (nth labelss 3)
                           example-gauge-metric-empty     (m/make-gauge-metric metric-name help map-empty)
                           example-gauge-metric-prefilled (m/make-gauge-metric metric-name help map-prefilled)]
                       (t/is (= (m/make-gauge-metric metric-name help {labels value})
                                (m/update-gauge-metric example-gauge-metric-empty labels value)))
                       (t/is (= (m/make-gauge-metric metric-name help
                                                     {(nth labelss 0) (nth values 0)
                                                      (nth labelss 1) (nth values 1)
                                                      (nth labelss 2) (nth values 2)
                                                      labels-to-change    value
                                                      (nth labelss 4) (nth values 4)})
                                (m/update-gauge-metric example-gauge-metric-prefilled labels-to-change value)))))))))

(t/deftest t-update-counter-metric
  (t/testing "Update of counter-metric works."
    (t/is (quickcheck
           (property [metric-name  (spec ::m/metric-name)
                      help         (spec ::m/metric-help)
                      [labels & labelss] (spec (gen-distinct-metric-labels 6))
                      [value  & values ] (spec (gen-metric-values          6))]
                     (let [map-empty {}
                           map-prefilled (zipmap labelss values)
                           labels-to-change (nth labelss 3)
                           value-to-change  (nth values  3)
                           example-counter-metric-empty     (m/make-counter-metric metric-name help map-empty)
                           example-counter-metric-prefilled (m/make-counter-metric metric-name help map-prefilled)]
                       (t/is (= (m/make-counter-metric metric-name help {labels value})
                                (m/update-counter-metric example-counter-metric-empty labels value)))
                       (t/is (= (m/make-counter-metric metric-name help
                                                     {(nth labelss 0) (nth values 0)
                                                      (nth labelss 1) (nth values 1)
                                                      (nth labelss 2) (nth values 2)
                                                      labels-to-change  (m/make-metric-value
                                                                         (+ (m/metric-value-value value-to-change)
                                                                            (m/metric-value-value value))
                                                                         (m/metric-value-last-update-time-ms value))
                                                      (nth labelss 4) (nth values 4)})
                                (m/update-counter-metric example-counter-metric-prefilled labels-to-change value)))))))))

;; TODO: cleanup
(t/deftest t-update-histogram-metric
  (t/testing "Update of histogram-metric works."
    (t/is (quickcheck
           (property [metric-name  (spec ::m/metric-name)
                      help         (spec ::m/metric-help)
                      threshold    (spec ::m/metric-value-value)
                      [labels & labelss] (spec (gen-distinct-metric-labels 6))
                      [value  & values ] (spec (gen-metric-values          6))]
                     (let [map-empty {}
                           map-prefilled (zipmap labelss values)
                           labels-to-change (nth labelss 3)
                           value-to-change  (nth values  3)
                           value-value       (m/metric-value-value value)
                           value-update-time (m/metric-value-last-update-time-ms value)
                           example-histogram-metric-empty     (m/make-histogram-metric metric-name
                                                                                       help
                                                                                       threshold
                                                                                       map-empty
                                                                                       map-empty
                                                                                       map-empty)
                           example-histogram-metric-prefilled (m/make-histogram-metric metric-name
                                                                                       help
                                                                                       threshold
                                                                                       map-prefilled
                                                                                       map-prefilled
                                                                                       map-prefilled)]
                       (t/is (= (m/make-histogram-metric metric-name
                                                         help
                                                         threshold
                                                         {labels value}
                                                         {labels (m/make-metric-value 1 value-update-time)}
                                                         {labels (m/make-metric-value (if (<= value-value threshold) 1 0)
                                                                                      value-update-time)})
                                (m/update-histogram-metric example-histogram-metric-empty labels value)))
                       (t/is (= (m/make-histogram-metric metric-name
                                                         help
                                                         threshold
                                                         ;; sum
                                                         {(nth labelss 0) (nth values 0)
                                                          (nth labelss 1) (nth values 1)
                                                          (nth labelss 2) (nth values 2)
                                                          labels-to-change  (m/make-metric-value
                                                                             (+ (m/metric-value-value value-to-change)
                                                                                (m/metric-value-value value))
                                                                             (m/metric-value-last-update-time-ms value))
                                                          (nth labelss 4) (nth values 4)}
                                                         ;; count
                                                         {(nth labelss 0) (nth values 0)
                                                          (nth labelss 1) (nth values 1)
                                                          (nth labelss 2) (nth values 2)
                                                          labels-to-change  (m/make-metric-value
                                                                             (+ 1
                                                                                (m/metric-value-value value-to-change))
                                                                             (m/metric-value-last-update-time-ms value))
                                                          (nth labelss 4) (nth values 4)}
                                                         ;; bucket
                                                         {(nth labelss 0) (nth values 0)
                                                          (nth labelss 1) (nth values 1)
                                                          (nth labelss 2) (nth values 2)
                                                          labels-to-change  (m/make-metric-value
                                                                             (if (<= value-value threshold)
                                                                               (+ 1
                                                                                  (m/metric-value-value value-to-change))
                                                                               (m/metric-value-value value-to-change))
                                                                               (m/metric-value-last-update-time-ms value))
                                                          (nth labelss 4) (nth values 4)})
                                (m/update-histogram-metric example-histogram-metric-prefilled labels-to-change value)))

                       (t/is (= (m/make-histogram-metric metric-name
                                                         help
                                                         threshold
                                                         ;; sum
                                                         {(nth labelss 0) (nth values 0)
                                                          (nth labelss 1) (nth values 1)
                                                          (nth labelss 2) (nth values 2)
                                                          labels-to-change  (m/make-metric-value
                                                                             (+ (m/metric-value-value value-to-change)
                                                                                (m/metric-value-value value)
                                                                                (m/metric-value-value value))
                                                                             (m/metric-value-last-update-time-ms value))
                                                          (nth labelss 4) (nth values 4)}
                                                         ;; count
                                                         {(nth labelss 0) (nth values 0)
                                                          (nth labelss 1) (nth values 1)
                                                          (nth labelss 2) (nth values 2)
                                                          labels-to-change  (m/make-metric-value
                                                                             (+ 2
                                                                                (m/metric-value-value value-to-change))
                                                                             (m/metric-value-last-update-time-ms value))
                                                          (nth labelss 4) (nth values 4)}
                                                         ;; bucket
                                                         {(nth labelss 0) (nth values 0)
                                                          (nth labelss 1) (nth values 1)
                                                          (nth labelss 2) (nth values 2)
                                                          labels-to-change  (m/make-metric-value
                                                                             (if (<= value-value threshold)
                                                                               (+ 2
                                                                                  (m/metric-value-value value-to-change))
                                                                               (m/metric-value-value value-to-change))
                                                                               (m/metric-value-last-update-time-ms value))
                                                          (nth labelss 4) (nth values 4)})
                                (m/update-histogram-metric
                                 (m/update-histogram-metric example-histogram-metric-prefilled labels-to-change value)
                                 labels-to-change
                                 value)))))))))

;; update-metric --- currently not used / not needed?

(t/deftest t-get-gauge-metric-sample
  (t/testing "Getting all metric-samples works for a gauge-metric."
    (t/is (quickcheck
           (property [name                (spec ::m/metric-name)
                      help                (spec ::m/metric-help)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 7))
                      values              (spec (gen-metric-values          6))]
                     (let [labels-values-map (zipmap labelss values)
                           labels-to-get     (nth labelss 3)
                           value-to-get      (nth values 3)
                           example-gauge-metric (m/make-gauge-metric name
                                                                     help
                                                                     labels-values-map)]
                       (t/is (= [] (m/get-gauge-metric-sample example-gauge-metric label-x)))
                       (t/is (= [(m/make-metric-sample name
                                                       labels-to-get
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))]
                                (m/get-gauge-metric-sample example-gauge-metric labels-to-get)))))))))

(t/deftest t-get-counter-metric-sample
  (t/testing "Getting all metric-samples works for a counter-metric."
    (t/is (quickcheck
           (property [name                (spec ::m/metric-name)
                      help                (spec ::m/metric-help)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 7))
                      values              (spec (gen-metric-values          6))]
                     (let [labels-values-map (zipmap labelss values)
                           labels-to-get     (nth labelss 2)
                           value-to-get      (nth values 2)
                           example-counter-metric (m/make-counter-metric name
                                                                         help
                                                                         labels-values-map)]
                       (t/is (= [] (m/get-counter-metric-sample example-counter-metric label-x)))
                       (t/is (= [(m/make-metric-sample name
                                                       labels-to-get
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))]
                                (m/get-counter-metric-sample example-counter-metric labels-to-get)))))))))

(t/deftest t-get-histogram-metric-sample
      (t/testing "Getting all metric-samples works for a histogram-metric."
        (t/is (quickcheck
               (property [name                (spec ::m/metric-name)
                          help                (spec ::m/metric-help)
                          threshold           (spec ::m/metric-value-value)
                          [label-x & labelss] (spec (gen-distinct-metric-labels 7))
                          values              (spec (gen-metric-values          6))]
                     (let [labels-values-map (zipmap labelss values)
                           labels-to-get     (nth labelss 0)
                           value-to-get      (nth values 0)
                           example-histogram-metric (m/make-histogram-metric name
                                                                             help
                                                                             threshold
                                                                             labels-values-map
                                                                             labels-values-map
                                                                             labels-values-map)]
                       (t/is (= [] (m/get-histogram-metric-sample example-histogram-metric label-x)))
                       (t/is (= [(m/make-metric-sample (str name "_sum")
                                                       labels-to-get
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))
                                 (m/make-metric-sample (str name "_count")
                                                       labels-to-get
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))
                                 (m/make-metric-sample (str name "_bucket")
                                                       (assoc labels-to-get :le "+Inf")
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))
                                 (m/make-metric-sample (str name "_bucket")
                                                       (assoc labels-to-get :le (str threshold))
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))]
                                (m/get-histogram-metric-sample example-histogram-metric labels-to-get)))))))))

(t/deftest t-get-metric-sample
  (t/testing "Getting a metric-sample works for all metrics."
    (t/is (quickcheck
           (property [name                (spec ::m/metric-name)
                      help                (spec ::m/metric-help)
                      threshold           (spec ::m/metric-value-value)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 7))
                      values              (spec (gen-metric-values          6))]
                     ;; gauge
                     (let [labels-values-map (zipmap labelss values)
                           labels-to-get     (nth labelss 3)
                           value-to-get      (nth values 3)
                           example-gauge-metric (m/make-gauge-metric name
                                                                     help
                                                                     labels-values-map)]
                       (t/is (= [] (m/get-metric-sample example-gauge-metric label-x)))
                       (t/is (= [(m/make-metric-sample name
                                                       labels-to-get
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))]
                                (m/get-metric-sample example-gauge-metric labels-to-get))))
                     ;; counter
                     (let [labels-values-map (zipmap labelss values)
                           labels-to-get     (nth labelss 2)
                           value-to-get      (nth values 2)
                           example-counter-metric (m/make-counter-metric name
                                                                         help
                                                                         labels-values-map)]
                       (t/is (= [] (m/get-metric-sample example-counter-metric label-x)))
                       (t/is (= [(m/make-metric-sample name
                                                       labels-to-get
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))]
                                (m/get-metric-sample example-counter-metric labels-to-get))))

                     ;; histogram
                     (let [labels-values-map (zipmap labelss values)
                           labels-to-get     (nth labelss 0)
                           value-to-get      (nth values 0)
                           example-histogram-metric (m/make-histogram-metric name
                                                                             help
                                                                             threshold
                                                                             labels-values-map
                                                                             labels-values-map
                                                                             labels-values-map)]

                       (t/is (= [] (m/get-metric-sample example-histogram-metric label-x)))
                       (t/is (= [(m/make-metric-sample (str name "_sum")
                                                       labels-to-get
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))
                                 (m/make-metric-sample (str name "_count")
                                                       labels-to-get
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))
                                 (m/make-metric-sample (str name "_bucket")
                                                       (assoc labels-to-get :le "+Inf")
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))
                                 (m/make-metric-sample (str name "_bucket")
                                                       (assoc labels-to-get :le (str threshold))
                                                       (m/metric-value-value               value-to-get)
                                                       (m/metric-value-last-update-time-ms value-to-get))]
                                (m/get-metric-sample example-histogram-metric labels-to-get)))))))))

(t/deftest t-record-get-metric!
  (t/testing "Basic recording and getting metrics works."
    (t/is (quickcheck
           ;; name-x as extra name not used for any metric
           (property [[name-x & names]       (spec (gen-distinct-metric-names  4))
                      helps                  (spec (gen-metric-helps           3))
                      threshold              (spec ::m/metric-value-value)
                      ;; labels-x as extra (not included) set of labels
                      [labels-x & labelss]   (spec (gen-distinct-metric-labels 7))
                      ;; value-x as extra value for updating
                      [value-x & values]     (spec (gen-metric-values          7))]
                     (let [value-1                (nth values 1)
                           value-4                (nth values 4)
                           value-5                (nth values 5)

                           raw-metric-store         (m/fresh-raw-metric-store)
                           example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0)
                                                                             {(nth labelss 0) (nth values 0)})
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1)
                                                                             {(nth labelss 2) (nth values 2)
                                                                              (nth labelss 3) (nth values 3)})
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2)
                                                                             threshold {} {} {})]
                       ;; first set
                       (m/record-metric! raw-metric-store
                                         example-gauge-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               value-1)
                                         (m/metric-value-last-update-time-ms value-1))
                       (m/record-metric! raw-metric-store
                                         example-counter-metric
                                         (nth labelss 4)
                                         (m/metric-value-value value-4))
                       (m/record-metric! raw-metric-store
                                         example-histogram-metric
                                         (nth labelss 5)
                                         (m/metric-value-value               value-5)
                                         (m/metric-value-last-update-time-ms value-5))
                       ;; TODO: both nil, both [] ?
                       ;; name not available
                       (t/is (= nil (m/get-raw-metric-sample! raw-metric-store name-x labels-x)))
                       ;; labels not available
                       (t/is (= [] (m/get-raw-metric-sample! raw-metric-store (nth names 0) labels-x)))

                       (t/is (= [(m/make-metric-sample (nth names 0)
                                                       (nth labelss 1)
                                                       (m/metric-value-value               value-1)
                                                       (m/metric-value-last-update-time-ms value-1))]
                                (m/get-raw-metric-sample! raw-metric-store (nth names 0) (nth labelss 1))))
                       ;; TODO: how to check timestamp that is set internally
                       #_(t/is (= [(m/make-metric-sample (nth names 1)
                                                       (nth labelss 4)
                                                       (m/metric-value-value               value-4)
                                                       ;; timestamp gets set internally --- how to test?
                                                       (m/metric-value-last-update-time-ms value-4))]
                                (m/get-raw-metric-sample! raw-metric-store (nth names 1) (nth labelss 4))))

                       (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                       (nth labelss 5)
                                                       (m/metric-value-value               value-5)
                                                       (m/metric-value-last-update-time-ms value-5))
                                 (m/make-metric-sample (str (nth names 2) "_count")
                                                       (nth labelss 5)
                                                       1
                                                       (m/metric-value-last-update-time-ms value-5))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 5) :le "+Inf")
                                                       1
                                                       (m/metric-value-last-update-time-ms value-5))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 5) :le (str threshold))
                                                       (if (<= (m/metric-value-value value-5) threshold)
                                                         1
                                                         0)
                                                       (m/metric-value-last-update-time-ms value-5))]
                                (m/get-raw-metric-sample! raw-metric-store (nth names 2) (nth labelss 5))))

                       ;; update metrics
                       (m/record-metric! raw-metric-store
                                         example-gauge-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               value-x)
                                         (m/metric-value-last-update-time-ms value-x))
                       (m/record-metric! raw-metric-store
                                         example-counter-metric
                                         (nth labelss 4)
                                         (m/metric-value-value value-x))
                       (m/record-metric! raw-metric-store
                                         example-histogram-metric
                                         (nth labelss 5)
                                         (m/metric-value-value               value-x)
                                         (m/metric-value-last-update-time-ms value-x))

                       (t/is (= [(m/make-metric-sample (nth names 0)
                                                       (nth labelss 1)
                                                       (m/metric-value-value               value-x)
                                                       (m/metric-value-last-update-time-ms value-x))]
                                (m/get-raw-metric-sample! raw-metric-store (nth names 0) (nth labelss 1))))
                       ;; TODO: how to check timestamp that is set internally
                       #_(t/is (= [(m/make-metric-sample (nth names 1)
                                                       (nth labelss 4)
                                                       (+ (m/metric-value-value            value-4)
                                                          (m/metric-value-value            value-x))
                                                       ;; timestamp gets set internally --- how to test?
                                                       (m/metric-value-last-update-time-ms value-4))]
                                (m/get-raw-metric-sample! raw-metric-store (nth names 1) (nth labelss 4))))

                       (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                       (nth labelss 5)
                                                       (m/metric-value-value value-x)
                                                       (m/metric-value-last-update-time-ms value-x))
                                 (m/make-metric-sample (str (nth names 2) "_count")
                                                       (nth labelss 5)
                                                       ;; FIXME: shouldn't be 1 but 2
                                                       1
                                                       (m/metric-value-last-update-time-ms value-x))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 5) :le "+Inf")
                                                       ;; FIXME: shouldn't be 1 but 2
                                                       1
                                                       (m/metric-value-last-update-time-ms value-x))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 5) :le (str threshold))
                                                       (cond (and (<= (m/metric-value-value value-5) threshold)
                                                                  (<= (m/metric-value-value value-x) threshold))
                                                             ;; FIXME: shouldn't be 1 but 2
                                                             1
                                                             (or (<= (m/metric-value-value value-5) threshold)
                                                                  (<= (m/metric-value-value value-x) threshold))
                                                             1
                                                             :else
                                                             0)
                                                       (m/metric-value-last-update-time-ms value-x))]
                                (m/get-raw-metric-sample! raw-metric-store (nth names 2) (nth labelss 5))))))))))

;; get-raw-metric-samples!
