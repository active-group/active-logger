(ns active.clojure.logger.metric-accumulator-test
  (:require [#_active.clojure.logger.benchmark.clojure-map-record active.clojure.logger.metric-accumulator :as m]
            [active.clojure.logger.metric-accumulator :as gen]
            [clojure.test :as t]

            [clojure.spec.alpha :as s]

            [clojure.spec.test.alpha :as stest]

            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad])
  (:use [active.quickcheck]))

(t/use-fixtures :once
  (fn [f]
    (stest/instrument)
    (s/check-asserts true)
    (f)
    (s/check-asserts false)
    (stest/unstrument)))

;; GENERATORS

(defn gen-distinct-metric-names
  [num-elems]
  (s/spec (s/coll-of ::gen/metric-name :distinct true :into [] :count num-elems)))

(defn gen-metric-helps
  [num-elems]
  (s/spec (s/coll-of ::gen/metric-help :distinct false :into [] :count num-elems)))

(defn gen-distinct-metric-labels
  [num-elems]
  (s/spec (s/coll-of ::gen/metric-labels :distinct true :into [] :count num-elems)))

(defn gen-metric-values
  [num-elems]
  (s/spec (s/coll-of ::gen/metric-value :distinct false :into [] :count num-elems)))

(defn gen-filled-gauge-values
  [gauge-values labelss values]
  (if (empty? labelss)
    gauge-values
    (let [[labels & rest-labelss] labelss
          [value & rest-values] values]
      (gen-filled-gauge-values
       (gen/update-gauge-values gauge-values labels value)
       rest-labelss
       rest-values))))

(defn gen-filled-counter-values
  [counter-values labelss values]
  (if (empty? labelss)
    counter-values
    (let [[labels & rest-labelss] labelss
          [value & rest-values] values]
      (gen-filled-counter-values
       (gen/update-counter-values counter-values labels value)
       rest-labelss
       rest-values))))

(defn gen-filled-histogram-values
  [histogram-values labelss values]
  (if (empty? labelss)
    histogram-values
    (let [[labels & rest-labelss] labelss
          [value & rest-values] values]
      (gen-filled-histogram-values
       (gen/update-histogram-values histogram-values labels value)
       rest-labelss
       rest-values))))

(defn gen-filled-metric-store-map
  [metric-store metric labelss values]
  (if (empty? labelss)
    metric-store
    (let [[labels & rest-labelss] labelss
          [value  & rest-values] values]
      (gen-filled-metric-store-map
       (gen/record-metric-1 metric-store metric labels value)
       metric
       rest-labelss
       rest-values))))

(defn gen-metric-samples
  [num-elems]
  (s/spec (s/coll-of ::gen/metric-sample :distinct false :into [] :count num-elems)))

;; -----------------------------------------------

(t/deftest t-fresh-metric-store-map
  (t/testing "Creating a fresh-metric-store-map works."
    (let [example-fresh-metric-store-map (m/fresh-metric-store-map)]
      (t/is (map?   example-fresh-metric-store-map))
      (t/is (empty? example-fresh-metric-store-map)))))

(t/deftest t-fresh-metric-store
  (t/testing "Creating a fresh-metric-store works."
    (let [example-fresh-metric-store (m/fresh-metric-store)]
      (t/is (= (type example-fresh-metric-store) clojure.lang.Atom))
      (t/is (map?  @example-fresh-metric-store))
      (t/is (empty @example-fresh-metric-store)))))

(t/deftest t-reset-and-set-global-metric-store!
  (t/testing "Setting and resetting a global-metric-store works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 3))
                      values    (spec (gen-metric-values          3))]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           example-gauge-values     (gen-filled-gauge-values     (m/make-gauge-values)               labelss values)
                           example-counter-values   (gen-filled-counter-values   (m/make-counter-values)             labelss values)
                           example-histogram-values (gen-filled-histogram-values (m/make-histogram-values [threshold]) labelss values)
                           example-metric-store-map {example-gauge-metric     example-gauge-values
                                                     example-counter-metric   example-counter-values
                                                     example-histogram-metric example-histogram-values}]
                       (m/reset-global-metric-store!)
                       (t/is (= {} @m/metric-store))
                       (m/set-global-metric-store! example-metric-store-map)
                       (t/is (= example-metric-store-map @m/metric-store))
                       (m/reset-global-metric-store!)
                       (t/is (= {} @m/metric-store))))))))

(t/deftest t-make-metric-value
  (t/testing "All fields of a metric-value are set correct."
    (t/is (quickcheck
           (property [value       (spec ::gen/metric-value-value)
                      update-time (spec ::gen/metric-value-last-update-time-ms)]
                     (let [example-metric-value (m/make-metric-value value update-time)]
                       (t/is                   (m/metric-value?                    example-metric-value))
                       (t/is (= (double value) (m/metric-value-value               example-metric-value)))
                       (t/is (= update-time    (m/metric-value-last-update-time-ms example-metric-value)))))))))

(t/deftest t-set-metric-value
  (t/testing "Setting a metric value works."
    (t/is (quickcheck
           (property [[labels & labelss]           (spec (gen-distinct-metric-labels 6))
                      [value  & values]           (spec (gen-metric-values          6))]
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
           (property [example-metric-value-1 (spec ::gen/metric-value)
                      example-metric-value-2 (spec ::gen/metric-value)]
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
                      [value  & values] (spec (gen-metric-values          6))]
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

(t/deftest t-prune-stale-metric-value
  (t/testing "Pruning stale metric values works."
    (t/is (quickcheck
           (property [labelss (spec (gen-distinct-metric-labels 6))]
                     ;; empty labels-value-map
                     (t/is (= {} (m/prune-stale-metric-value {} 1)))
                     ;; not older
                     (t/is (= {(nth labelss 0) (m/make-metric-value 300 1)}
                              (m/prune-stale-metric-value
                               {(nth labelss 0) (m/make-metric-value 300 1)} 0)))
                     ;; the same
                     (t/is (= {(nth labelss 0) (m/make-metric-value 300 1)}
                              (m/prune-stale-metric-value
                               {(nth labelss 0) (m/make-metric-value 300 1)} 1)))
                     ;; older
                     (t/is (= {} (m/prune-stale-metric-value
                                  {(nth labelss 0) (m/make-metric-value 300 1)} 2)))
                     ;; mixture and more labels
                     (t/is (= {(nth labelss 3) (m/make-metric-value 300 4)
                               (nth labelss 4) (m/make-metric-value 300 5)
                               (nth labelss 5) (m/make-metric-value 300 6)}
                              (m/prune-stale-metric-value
                               {(nth labelss 0) (m/make-metric-value 300 1)
                                (nth labelss 1) (m/make-metric-value 300 2)
                                (nth labelss 2) (m/make-metric-value 300 3)
                                (nth labelss 3) (m/make-metric-value 300 4)
                                (nth labelss 4) (m/make-metric-value 300 5)
                                (nth labelss 5) (m/make-metric-value 300 6)} 4))))))))

;; 1. Gauges

(t/deftest t-make-gauge-metric
  (t/testing "All fields of a gauge-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::gen/metric-name)
                      help              (spec ::gen/metric-help)]
                     (let [example-gauge-metric (m/make-gauge-metric metric-name help)]
                       (t/is                      (m/gauge-metric?                  example-gauge-metric))
                       (t/is (= metric-name       (m/gauge-metric-name              example-gauge-metric)))
                       (t/is (= help              (m/gauge-metric-help              example-gauge-metric)))))))))


(t/deftest t-make-gauge-values
  (t/testing "All fields of a metric-value are set correct."
    (let [example-gauge-values (m/make-gauge-values)]
      (t/is                       (m/gauge-values?    example-gauge-values))
      (t/is (= m/empty-values-map (m/gauge-values-map example-gauge-values))))))

(t/deftest t-update-gauge-values
  (t/testing "Updating gauge-values works."
    (t/is (quickcheck
           (property [labelss             (spec (gen-distinct-metric-labels 3))
                      [value-x & values]  (spec (gen-metric-values          4))]
                     (let [example-gauge-values (m/make-gauge-values)]
                       (t/is (= {} (m/gauge-values-map example-gauge-values)))
                       (t/is (= {(nth labelss 0) (nth values 0)}
                                (m/gauge-values-map
                                 (m/update-gauge-values example-gauge-values (nth labelss 0) (nth values 0)))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)}
                                (m/gauge-values-map
                                 (m/update-gauge-values
                                  (m/update-gauge-values example-gauge-values (nth labelss 0) (nth values 0))
                                  (nth labelss 1) (nth values 1)))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)}
                                (m/gauge-values-map
                                 (m/update-gauge-values
                                  (m/update-gauge-values
                                   (m/update-gauge-values example-gauge-values (nth labelss 0) (nth values 0))
                                   (nth labelss 1) (nth values 1))
                                  (nth labelss 2) (nth values  2)))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) value-x
                                 (nth labelss 2) (nth values 2)}
                                (m/gauge-values-map
                                 (m/update-gauge-values
                                  (m/update-gauge-values
                                   (m/update-gauge-values
                                    (m/update-gauge-values example-gauge-values (nth labelss 0) (nth values 0))
                                    (nth labelss 1) (nth values 1))
                                   (nth labelss 2) (nth values  2))
                                  (nth labelss 1) value-x))))))))))

(t/deftest t-gauge-values->metric-samples
  (t/testing "Creating metric-samples from gauge-values works."
    (t/is (quickcheck
           (property [name (spec ::gen/metric-name)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 4))
                      values              (spec (gen-metric-values          3))]
                     (let [empty-gauge-values (m/make-gauge-values)
                           filled-gauge-values (gen-filled-gauge-values empty-gauge-values labelss values)]
                       ;; empty gauge-values-map
                       (t/is (= [] (m/gauge-values->metric-samples name empty-gauge-values label-x)))
                       ;; labels not in gauge-values-map
                       (t/is (= [] (m/gauge-values->metric-samples name filled-gauge-values label-x)))
                       (t/is (= [(m/make-metric-sample name
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values  0))
                                                       (m/metric-value-last-update-time-ms (nth values  0)))]
                                (m/gauge-values->metric-samples name filled-gauge-values (nth labelss 0))))))))))

(t/deftest t-prune-stale-gauge-values
  (t/testing "Pruning stale gauge-values works."
    (t/is (quickcheck
           (property [labelss (spec (gen-distinct-metric-labels 6))]
                     (let [empty-gauge-values (m/make-gauge-values)
                           values [(m/make-metric-value 300 10)
                                   (m/make-metric-value 300 11)
                                   (m/make-metric-value 300 12)
                                   (m/make-metric-value 300 13)
                                   (m/make-metric-value 300 14)
                                   (m/make-metric-value 300 15)]
                           filled-gauge-values (gen-filled-gauge-values empty-gauge-values labelss values)]
                       ;; empty labels-value-map
                       (t/is (= {}
                                (m/gauge-values-map (m/prune-stale-gauge-values empty-gauge-values 1))))
                       ;; not older
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/gauge-values-map (m/prune-stale-gauge-values filled-gauge-values 9))))
                       ;; the same
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/gauge-values-map (m/prune-stale-gauge-values filled-gauge-values 10))))
                       ;; mixture
                       (t/is (= {(nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/gauge-values-map (m/prune-stale-gauge-values filled-gauge-values 13))))))))))

(t/deftest t-empty-gauge-values?
  (t/testing "Checking whether gauge-values-map is empty works."
    (t/is (quickcheck
           (property [labelss (spec (gen-distinct-metric-labels 6))
                      values  (spec (gen-metric-values          6))]
                     (let [empty-gauge-values (m/make-gauge-values)
                           filled-gauge-values (gen-filled-gauge-values empty-gauge-values labelss values)]
                       (t/is (= true  (m/empty-gauge-values? empty-gauge-values)))
                       (t/is (= false (m/empty-gauge-values? filled-gauge-values)))))))))

;; 2. Counters

(t/deftest t-make-counter-metric
  (t/testing "All fields of a counter-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::gen/metric-name)
                      help              (spec ::gen/metric-help)]
                     (let [example-counter-metric (m/make-counter-metric metric-name help)]
                       (t/is                      (m/counter-metric?                  example-counter-metric))
                       (t/is (= metric-name       (m/counter-metric-name              example-counter-metric)))
                       (t/is (= help              (m/counter-metric-help              example-counter-metric)))))))))


(t/deftest t-make-counter-values
  (t/testing "All fields of a metric-value are set correct."
    (let [example-counter-values (m/make-counter-values)]
      (t/is                       (m/counter-values?    example-counter-values))
      (t/is (= m/empty-values-map (m/counter-values-map example-counter-values))))))

(t/deftest t-update-counter-values
  (t/testing "Updating counter-values works."
    (t/is (quickcheck
           (property [labelss             (spec (gen-distinct-metric-labels 3))
                      [value-x & values]  (spec (gen-metric-values          4))]
                     (let [example-counter-values (m/make-counter-values)]
                       (t/is (= {} (m/counter-values-map example-counter-values)))
                       (t/is (= {(nth labelss 0) (nth values 0)}
                                (m/counter-values-map
                                 (m/update-counter-values example-counter-values (nth labelss 0) (nth values 0)))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)}
                                (m/counter-values-map
                                 (m/update-counter-values
                                  (m/update-counter-values example-counter-values (nth labelss 0) (nth values 0))
                                  (nth labelss 1) (nth values 1)))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)}
                                (m/counter-values-map
                                 (m/update-counter-values
                                  (m/update-counter-values
                                   (m/update-counter-values example-counter-values (nth labelss 0) (nth values 0))
                                   (nth labelss 1) (nth values 1))
                                  (nth labelss 2) (nth values  2)))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (m/make-metric-value (+ (m/metric-value-value (nth values 1))
                                                                         (m/metric-value-value value-x))
                                                                      (m/metric-value-last-update-time-ms value-x))
                                 (nth labelss 2) (nth values 2)}
                                (m/counter-values-map
                                 (m/update-counter-values
                                  (m/update-counter-values
                                   (m/update-counter-values
                                    (m/update-counter-values example-counter-values (nth labelss 0) (nth values 0))
                                    (nth labelss 1) (nth values 1))
                                   (nth labelss 2) (nth values  2))
                                  (nth labelss 1) value-x))))))))))

(t/deftest t-counter-values->metric-samples
  (t/testing "Creating metric-samples from counter-values works."
    (t/is (quickcheck
           (property [name (spec ::gen/metric-name)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 4))
                      values              (spec (gen-metric-values          3))]
                     (let [empty-counter-values (m/make-counter-values)
                           filled-counter-values (gen-filled-counter-values empty-counter-values labelss values)]
                       ;; empty counter-values-map
                       (t/is (empty?
                              (m/counter-values->metric-samples name empty-counter-values label-x)))
                       ;; labels not in counter-values-map
                       (t/is (empty?
                              (m/counter-values->metric-samples name filled-counter-values label-x)))
                       (t/is (= [(m/make-metric-sample name
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values  0))
                                                       (m/metric-value-last-update-time-ms (nth values  0)))]
                                (m/counter-values->metric-samples name filled-counter-values (nth labelss 0))))))))))

(t/deftest t-prune-stale-counter-values
  (t/testing "Pruning stale counter-values works."
    (t/is (quickcheck
           (property [labelss (spec (gen-distinct-metric-labels 6))]
                     (let [empty-counter-values (m/make-counter-values)
                           values [(m/make-metric-value 300 10)
                                   (m/make-metric-value 300 11)
                                   (m/make-metric-value 300 12)
                                   (m/make-metric-value 300 13)
                                   (m/make-metric-value 300 14)
                                   (m/make-metric-value 300 15)]
                           filled-counter-values (gen-filled-counter-values empty-counter-values labelss values)]
                       ;; empty labels-value-map
                       (t/is (= {}
                                (m/counter-values-map (m/prune-stale-counter-values empty-counter-values 1))))
                       ;; not older
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/counter-values-map (m/prune-stale-counter-values filled-counter-values 9))))
                       ;; the same
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/counter-values-map (m/prune-stale-counter-values filled-counter-values 10))))
                       ;; mixture
                       (t/is (= {(nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/counter-values-map (m/prune-stale-counter-values filled-counter-values 13))))))))))

(t/deftest t-empty-counter-values?
  (t/testing "Checking whether counter-values-map is empty works."
    (t/is (quickcheck
           (property [labelss (spec (gen-distinct-metric-labels 6))
                      values  (spec (gen-metric-values          6))]
                     (let [empty-counter-values (m/make-counter-values)
                           filled-counter-values (gen-filled-counter-values empty-counter-values labelss values)]
                       (t/is (= true  (m/empty-counter-values? empty-counter-values)))
                       (t/is (= false (m/empty-counter-values? filled-counter-values)))))))))

;; 3. Histograms

(t/deftest t-make-histogram-metric
  (t/testing "All fields of a histogram-metric are set correct."
    (t/is (quickcheck
           (property [metric-name       (spec ::gen/metric-name)
                      help              (spec ::gen/metric-help)
                      threshold         (spec ::gen/threshold)]
                     (let [example-histogram-metric (m/make-histogram-metric metric-name help [threshold])]
                       (t/is                      (m/histogram-metric? example-histogram-metric))
                       (t/is (= metric-name       (m/histogram-metric-name                     example-histogram-metric)))
                       (t/is (= help              (m/histogram-metric-help                     example-histogram-metric)))
                       (t/is (= [threshold]       (m/histogram-metric-thresholds               example-histogram-metric)))))))))

(t/deftest t-make-histogram-values
  (t/testing "All fields of a metric-value are set correct."
    (t/is (quickcheck
           (property [threshold (spec ::gen/threshold)]
                     (let [example-histogram-values (m/make-histogram-values [threshold])]
                       (t/is                       (m/histogram-values?           example-histogram-values))
                       (t/is (= [threshold]        (m/histogram-values-thresholds example-histogram-values)))
                       (t/is (= m/empty-values-map (m/histogram-values-map        example-histogram-values)))))))))

(t/deftest t-histogram-values->metric-samples
  (t/testing "Creating metric-samples from histogram-values works."
    (t/is (quickcheck
           (property [basename            (spec ::gen/metric-name)
                      threshold           (spec ::gen/threshold)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 4))
                      values              (spec (gen-metric-values          3))]
                     (let [empty-histogram-values (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)]
                       ;; empty histogram-values-map
                       (t/is (empty? (m/histogram-values->metric-samples basename empty-histogram-values label-x)))
                       ;; labels not in histogram-values-map
                       (t/is (empty? (m/histogram-values->metric-samples basename filled-histogram-values label-x)))
                       (t/is (= [(m/make-metric-sample (str basename "_sum")
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_count")
                                                       (nth labelss 0)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 0) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 0) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/histogram-values->metric-samples basename filled-histogram-values (nth labelss 0))))))))))

(t/deftest t-prune-stale-histogram-values
  (t/testing "Pruning stale histogram-values works."
    (t/is (quickcheck
           (property [threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 6))]
                     (let [empty-histogram-values (m/make-histogram-values [threshold])
                           values [(m/make-metric-value 300 10)
                                   (m/make-metric-value 300 11)
                                   (m/make-metric-value 300 12)
                                   (m/make-metric-value 300 13)
                                   (m/make-metric-value 300 14)
                                   (m/make-metric-value 300 15)]
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)]
                       ;; empty labels-value-map
                       (t/is (= {}
                                (m/histogram-values-map (m/prune-stale-histogram-values empty-histogram-values 1))))
                       ;; not older
                       (t/is (= {(nth labelss 0) (let [value (nth values 0)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 1) (let [value (nth values 1)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 2) (let [value (nth values 2)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 3) (let [value (nth values 3)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 4) (let [value (nth values 4)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 5) (let [value (nth values 5)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))}
                                (m/histogram-values-map (m/prune-stale-histogram-values filled-histogram-values 9))))
                       ;; the same
                       (t/is (= {(nth labelss 0) (let [value (nth values 0)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 1) (let [value (nth values 1)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 2) (let [value (nth values 2)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 3) (let [value (nth values 3)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 4) (let [value (nth values 4)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 5) (let [value (nth values 5)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))}
                                (m/histogram-values-map (m/prune-stale-histogram-values filled-histogram-values 10))))
                       ;; mixture
                       (t/is (= {(nth labelss 3) (let [value (nth values 3)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 4) (let [value (nth values 4)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 5) (let [value (nth values 5)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))}
                                (m/histogram-values-map (m/prune-stale-histogram-values filled-histogram-values 13))))))))))

(t/deftest t-empty-histogram-values?
  (t/testing "Checking whether histogram-values-map (sum-map) is empty works."
    (t/is (quickcheck
           (property [threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 6))
                      values    (spec (gen-metric-values          6))]
                     (let [empty-histogram-values (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)]
                       (t/is (= true  (m/empty-histogram-values? empty-histogram-values)))
                       (t/is (= false (m/empty-histogram-values? filled-histogram-values)))))))))

;; Primitives on stored values

;; TODO: testing with more elaborated value-counts? That is, not only where each label has the value 1.
;; TODO: testing that threshold for histogram doesn't change.
(t/deftest t-update-stored-values
  (t/testing "Updating stored values works."
    (t/is (quickcheck
           (property [[labels-x & labelss] (spec (gen-distinct-metric-labels 7))
                      [value-x  & values] (spec (gen-metric-values          7))
                      threshold            (spec ::gen/threshold)]
                     (let [filled-gauge-values     (gen-filled-gauge-values     (m/make-gauge-values)               labelss values)
                           filled-counter-values   (gen-filled-counter-values   (m/make-counter-values)             labelss values)
                           filled-histogram-values (gen-filled-histogram-values (m/make-histogram-values [threshold]) labelss values)]
                       ;; Gauge values
                       (t/is (= {labels-x value-x}
                                (m/gauge-values-map (m/update-stored-values (m/make-gauge-values) labels-x value-x))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)
                                 labels-x        value-x}
                                (m/gauge-values-map (m/update-stored-values filled-gauge-values labels-x value-x))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) value-x
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/gauge-values-map (m/update-stored-values filled-gauge-values (nth labelss 2) value-x))))
                       ;; Counter values
                       (t/is (= {labels-x value-x}
                                (m/counter-values-map (m/update-stored-values (m/make-counter-values) labels-x value-x))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)
                                 labels-x        value-x}
                                (m/counter-values-map (m/update-stored-values filled-counter-values labels-x value-x))))
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (m/make-metric-value (+ (m/metric-value-value (nth values 2))
                                                                         (m/metric-value-value value-x))
                                                                      (m/metric-value-last-update-time-ms value-x))
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/counter-values-map (m/update-stored-values filled-counter-values (nth labelss 2) value-x))))
                       ;; Histogram values
                       (t/is (= {labels-x (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value-x)
                                                                          (m/metric-value-value value-x)
                                                                          1.0
                                                                          [(if (<= (m/metric-value-value value-x) threshold) 1.0 0.0)])}
                                (m/histogram-values-map (m/update-stored-values (m/make-histogram-values [threshold]) labels-x value-x))))
                       (t/is (= {(nth labelss 0) (let [value (nth values 0)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 1) (let [value (nth values 1)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 2) (let [value (nth values 2)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 3) (let [value (nth values 3)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 4) (let [value (nth values 4)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 5) (let [value (nth values 5)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 labels-x        (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value-x)
                                                                                 (m/metric-value-value value-x)
                                                                                 1.0
                                                                                 [(if (<= (m/metric-value-value value-x) threshold) 1.0 0.0)])}
                                (m/histogram-values-map (m/update-stored-values filled-histogram-values labels-x value-x))))
                       (t/is (= {(nth labelss 0) (let [value (nth values 0)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 1) (let [value (nth values 1)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 2) (let [value (nth values 2)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value-x)
                                                                                   (+ (m/metric-value-value value) (m/metric-value-value value-x))
                                                                                   2.0
                                                                                   [(+ (if (<= (m/metric-value-value value) threshold) 1.0 0.0)
                                                                                       (if (<= (m/metric-value-value value-x) threshold) 1.0 0.0))]))
                                 (nth labelss 3) (let [value (nth values 3)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 4) (let [value (nth values 4)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 5) (let [value (nth values 5)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))}
                                (m/histogram-values-map (m/update-stored-values filled-histogram-values (nth labelss 2) value-x))))))))))

(t/deftest t-make-stored-values
  (t/testing "Making stored values works."
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::gen/gauge-metric)
                      example-counter-metric   (spec ::gen/counter-metric)
                      example-histogram-metric (spec ::gen/histogram-metric)
                      labels                   (spec ::gen/metric-labels)
                      value                    (spec ::gen/metric-value)]
                     (let [gauge-stored-values     (m/make-stored-values example-gauge-metric     labels value)
                           counter-stored-values   (m/make-stored-values example-counter-metric   labels value)
                           histogram-stored-values (m/make-stored-values example-histogram-metric labels value)]
                       ;; Gauge
                       (t/is (m/gauge-values? gauge-stored-values))
                       (t/is (= {labels value}
                                (m/gauge-values-map gauge-stored-values)))

                       ;; Counter
                       (t/is (m/counter-values? counter-stored-values))
                       (t/is (= {labels value}
                                (m/counter-values-map counter-stored-values)))

                       ;; Histogram
                       (t/is (m/histogram-values? histogram-stored-values))
                       (t/is (= (m/histogram-metric-thresholds example-histogram-metric)
                                (m/histogram-values-thresholds histogram-stored-values)))
                       (t/is (= {labels (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                        (m/metric-value-value value)
                                                                        1.0
                                                                        (mapv (fn [threshold]
                                                                                (if (<= (m/metric-value-value value)
                                                                                        threshold)
                                                                                  1.0 0.0))
                                                                              (m/histogram-metric-thresholds example-histogram-metric)))}
                                (m/histogram-values-map histogram-stored-values)))))))))

(t/deftest t-prune-stale-stored-values
  (t/testing "Pruning stale stored-values works."
    (t/is (quickcheck
           (property [labelss   (spec (gen-distinct-metric-labels 6))
                      threshold (spec ::gen/threshold)]
                     (let [empty-gauge-values (m/make-gauge-values)
                           empty-counter-values (m/make-counter-values)
                           empty-histogram-values (m/make-histogram-values [threshold])
                           values [(m/make-metric-value 300 10)
                                   (m/make-metric-value 300 11)
                                   (m/make-metric-value 300 12)
                                   (m/make-metric-value 300 13)
                                   (m/make-metric-value 300 14)
                                   (m/make-metric-value 300 15)]
                           filled-gauge-values (gen-filled-gauge-values empty-gauge-values labelss values)
                           filled-counter-values (gen-filled-counter-values empty-counter-values labelss values)
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)]
                       ;; Gauge
                       (t/is (= {}
                                (m/gauge-values-map (m/prune-stale-stored-values empty-gauge-values 1))))
                       ;; not older
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/gauge-values-map (m/prune-stale-stored-values filled-gauge-values 9))))
                       ;; the same
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/gauge-values-map (m/prune-stale-stored-values filled-gauge-values 10))))
                       ;; mixture
                       (t/is (= {(nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/gauge-values-map (m/prune-stale-stored-values filled-gauge-values 13))))
                       ;; Counter
                       ;; empty labels-value-map
                       (t/is (= {}
                                (m/counter-values-map (m/prune-stale-stored-values empty-counter-values 1))))
                       ;; not older
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/counter-values-map (m/prune-stale-stored-values filled-counter-values 9))))
                       ;; the same
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/counter-values-map (m/prune-stale-stored-values filled-counter-values 10))))
                       ;; mixture
                       (t/is (= {(nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/counter-values-map (m/prune-stale-stored-values filled-counter-values 13))))

                       ;; Histogram
                       (let [value-value-0 (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                             value-value-1 (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                             value-value-2 (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                             value-value-3 (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                             value-value-4 (if (<= (m/metric-value-value (nth values 4)) threshold) 1.0 0.0)
                             value-value-5 (if (<= (m/metric-value-value (nth values 5)) threshold) 1.0 0.0)]
                       ;; empty labels-value-map
                       (t/is (= {}
                                (m/histogram-values-map (m/prune-stale-histogram-values empty-histogram-values 1))))
                       ;; not older
                       (t/is (= {(nth labelss 0) (let [value (nth values 0)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 1) (let [value (nth values 1)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 2) (let [value (nth values 2)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 3) (let [value (nth values 3)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 4) (let [value (nth values 4)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 5) (let [value (nth values 5)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))}
                                (m/histogram-values-map (m/prune-stale-histogram-values filled-histogram-values 9))))
                       ;; the same
                       (t/is (= {(nth labelss 0) (let [value (nth values 0)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 1) (let [value (nth values 1)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 2) (let [value (nth values 2)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 3) (let [value (nth values 3)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 4) (let [value (nth values 4)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 5) (let [value (nth values 5)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))}
                                (m/histogram-values-map (m/prune-stale-histogram-values filled-histogram-values 10))))
                       ;; mixture
                       (t/is (= {(nth labelss 3) (let [value (nth values 3)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 4) (let [value (nth values 4)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))
                                 (nth labelss 5) (let [value (nth values 5)]
                                                   (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                                   (m/metric-value-value value)
                                                                                   1.0
                                                                                   [(if (<= (m/metric-value-value value) threshold) 1.0 0.0)]))}
                                (m/histogram-values-map (m/prune-stale-histogram-values filled-histogram-values 13)))))))))))

(t/deftest t-empty-stored-values?
  (t/testing "Checking whether a stored-values-map is empty works."
    (t/is (quickcheck
           (property [threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 6))
                      values    (spec (gen-metric-values          6))]
                     (let [empty-gauge-values (m/make-gauge-values)
                           filled-gauge-values (gen-filled-gauge-values empty-gauge-values labelss values)
                           empty-counter-values (m/make-counter-values)
                           filled-counter-values (gen-filled-counter-values empty-counter-values labelss values)
                           empty-histogram-values (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)]

                       ;; Gauge
                       (t/is (= true  (m/empty-stored-values? empty-gauge-values)))
                       (t/is (= false (m/empty-stored-values? filled-gauge-values)))
                       ;; Counter
                       (t/is (= true  (m/empty-stored-values? empty-counter-values)))
                       (t/is (= false (m/empty-stored-values? filled-counter-values)))
                       ;; Histogram
                       (t/is (= true  (m/empty-stored-values? empty-histogram-values)))
                       (t/is (= false (m/empty-stored-values? filled-histogram-values)))))))))

;; Metric samples and sample sets

(t/deftest t-make-metric-sample
  (t/testing "All fields of a metric-sample are set correct."
    (t/is (quickcheck
           (property [metric-name (spec ::gen/metric-name)
                      labels      (spec ::gen/metric-labels)
                      value       (spec ::gen/metric-value-value)
                      timestamp   (spec ::gen/metric-value-last-update-time-ms)]
                     (let [example-metric-sample (m/make-metric-sample metric-name
                                                                       labels
                                                                       value
                                                                       timestamp)]
                       (t/is                (m/metric-sample?          example-metric-sample))
                       (t/is (= metric-name (m/metric-sample-name      example-metric-sample)))
                       (t/is (= labels      (m/metric-sample-labels    example-metric-sample)))
                       (t/is (= value       (m/metric-sample-value     example-metric-sample)))
                       (t/is (= timestamp   (m/metric-sample-timestamp example-metric-sample)))))))))

;; ------------------

;; TODO: More elaborated metric-stores
(t/deftest t-record-metric-1-get-metric-samples-1
  (t/testing "`record-metric-1` and `get-metric-samples-1` work."
    (t/is (quickcheck
           (property [names               (spec (gen-distinct-metric-names  3))
                      helps               (spec (gen-metric-helps           3))
                      threshold           (spec ::gen/threshold)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 7))
                      [value-x & values] (spec (gen-metric-values          7))]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           metric-store (m/fresh-metric-store-map)]
                       ;; Gauge
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples-1 metric-store example-gauge-metric label-x)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples-1
                                    (m/record-metric-1 metric-store example-gauge-metric (nth labelss 0) (nth values 0))
                                    example-gauge-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(m/make-metric-sample (nth names 0)
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples-1
                                 (m/record-metric-1 metric-store example-gauge-metric (nth labelss 0) (nth values 0))
                                 example-gauge-metric (nth labelss 0))))
                       ;; labels are within this metric - map is larger
                       (t/is (= [(m/make-metric-sample (nth names 0)
                                                       (nth labelss 3)
                                                       (m/metric-value-value               (nth values 3))
                                                       (m/metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples-1
                                 (m/record-metric-1
                                  (m/record-metric-1
                                   (m/record-metric-1
                                    (m/record-metric-1
                                     (m/record-metric-1
                                      (m/record-metric-1 metric-store
                                                         example-gauge-metric (nth labelss 0) (nth values 0))
                                      example-gauge-metric (nth labelss 1) (nth values 1))
                                     example-gauge-metric (nth labelss 2) (nth values 2))
                                    example-gauge-metric (nth labelss 3) (nth values 3))
                                   example-gauge-metric (nth labelss 4) (nth values 4))
                                  example-gauge-metric (nth labelss 5) (nth values 5))
                                 example-gauge-metric (nth labelss 3))))

                       ;; Counter
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples-1 metric-store example-counter-metric label-x)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples-1
                                    (m/record-metric-1 metric-store example-counter-metric (nth labelss 0) (nth values 0))
                                    example-counter-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(m/make-metric-sample (nth names 1)
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples-1
                                 (m/record-metric-1 metric-store example-counter-metric (nth labelss 0) (nth values 0))
                                 example-counter-metric (nth labelss 0))))
                       ;; labels are within this metric - map is larger
                       (t/is (= [(m/make-metric-sample (nth names 1)
                                                       (nth labelss 3)
                                                       (m/metric-value-value               (nth values 3))
                                                       (m/metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples-1
                                 (m/record-metric-1
                                  (m/record-metric-1
                                   (m/record-metric-1
                                    (m/record-metric-1
                                     (m/record-metric-1
                                      (m/record-metric-1 metric-store
                                                         example-counter-metric (nth labelss 0) (nth values 0))
                                      example-counter-metric (nth labelss 1) (nth values 1))
                                     example-counter-metric (nth labelss 2) (nth values 2))
                                    example-counter-metric (nth labelss 3) (nth values 3))
                                   example-counter-metric (nth labelss 4) (nth values 4))
                                  example-counter-metric (nth labelss 5) (nth values 5))
                                 example-counter-metric (nth labelss 3))))

                       ;; Histogram
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples-1 metric-store example-histogram-metric label-x)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples-1
                                    (m/record-metric-1 metric-store example-histogram-metric (nth labelss 0) (nth values 0))
                                    example-histogram-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str (nth names 2) "_count")
                                                       (nth labelss 0)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 0) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 0) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples-1
                                 (m/record-metric-1 metric-store example-histogram-metric (nth labelss 0) (nth values 0))
                                 example-histogram-metric (nth labelss 0))))
                       ;; labels are within this metric - map is larger
                       (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                       (nth labelss 3)
                                                       (m/metric-value-value               (nth values 3))
                                                       (m/metric-value-last-update-time-ms (nth values 3)))
                                 (m/make-metric-sample (str (nth names 2) "_count")
                                                       (nth labelss 3)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 3)))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 3) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 3)))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 3) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples-1
                                 (m/record-metric-1
                                  (m/record-metric-1
                                   (m/record-metric-1
                                    (m/record-metric-1
                                     (m/record-metric-1
                                      (m/record-metric-1 metric-store
                                                         example-histogram-metric (nth labelss 0) (nth values 0))
                                      example-histogram-metric (nth labelss 1) (nth values 1))
                                     example-histogram-metric (nth labelss 2) (nth values 2))
                                    example-histogram-metric (nth labelss 3) (nth values 3))
                                   example-histogram-metric (nth labelss 4) (nth values 4))
                                  example-histogram-metric (nth labelss 5) (nth values 5))
                                 example-histogram-metric (nth labelss 3))))))))))

(t/deftest t-record-metric!-get-metric-samples!
  (t/testing "`record-metric!` and `get-metric-samples!` work."
    (t/is (quickcheck
           (property [names               (spec (gen-distinct-metric-names  3))
                      helps               (spec (gen-metric-helps           3))
                      threshold           (spec ::gen/threshold)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 7))
                      [value-x & values] (spec (gen-metric-values          7))]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           a-metric-store (m/fresh-metric-store)]

                       ;; GAUGES
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-gauge-metric label-x)))
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-gauge-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(m/make-metric-sample (nth names 0)
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples! a-metric-store example-gauge-metric (nth labelss 0))))

                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))
                       ;; update-time: nil
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 4)
                                         (m/metric-value-value               (nth values 4))
                                         nil)
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 5)
                                         (m/metric-value-value               (nth values 5))
                                         (m/metric-value-last-update-time-ms (nth values 5)))

                       ;; labels are within this metric - map is larger
                       (t/is (= [(m/make-metric-sample (nth names 0)
                                                       (nth labelss 3)
                                                       (m/metric-value-value               (nth values 3))
                                                       (m/metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples! a-metric-store example-gauge-metric (nth labelss 3))))

                       ;; update-time: nil
                       (let [example-metric-sample (nth (m/get-metric-samples! a-metric-store example-gauge-metric (nth labelss 4)) 0)]
                         (t/is (= (nth names   0) (m/metric-sample-name   example-metric-sample)))
                         (t/is (= (nth labelss 4) (m/metric-sample-labels example-metric-sample)))
                         (t/is (= (m/metric-value-value (nth values 4))
                                  (m/metric-sample-value example-metric-sample)))
                         (t/is (nat-int? (m/metric-sample-timestamp example-metric-sample))))

                       ;; COUNTERS
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-counter-metric label-x)))
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-counter-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(m/make-metric-sample (nth names 1)
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples! a-metric-store example-counter-metric (nth labelss 0))))

                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))
                       ;; update-time: nil
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 4)
                                         (m/metric-value-value               (nth values 4))
                                         nil)
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 5)
                                         (m/metric-value-value               (nth values 5))
                                         (m/metric-value-last-update-time-ms (nth values 5)))

                       ;; labels are within this metric - map is larger
                       (t/is (= [(m/make-metric-sample (nth names 1)
                                                       (nth labelss 3)
                                                       (m/metric-value-value               (nth values 3))
                                                       (m/metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples! a-metric-store example-counter-metric (nth labelss 3))))
                       ;; update-time: nil
                       (let [example-metric-sample (nth (m/get-metric-samples! a-metric-store example-counter-metric (nth labelss 4)) 0)]
                         (t/is (= (nth names   1) (m/metric-sample-name   example-metric-sample)))
                         (t/is (= (nth labelss 4) (m/metric-sample-labels example-metric-sample)))
                         (t/is (= (m/metric-value-value (nth values 4))
                                  (m/metric-sample-value example-metric-sample)))
                         (t/is (nat-int? (m/metric-sample-timestamp example-metric-sample))))

                       ;; HISTOGRAMS
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-histogram-metric label-x)))
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-histogram-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str (nth names 2) "_count")
                                                       (nth labelss 0)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 0) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 0) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples! a-metric-store example-histogram-metric (nth labelss 0))))
                       ;; labels are within this metric - map is larger
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))
                       ;; update-time: nil
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 4)
                                         (m/metric-value-value               (nth values 4))
                                         nil)
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 5)
                                         (m/metric-value-value               (nth values 5))
                                         (m/metric-value-last-update-time-ms (nth values 5)))

                       (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                       (nth labelss 3)
                                                       (m/metric-value-value               (nth values 3))
                                                       (m/metric-value-last-update-time-ms (nth values 3)))
                                 (m/make-metric-sample (str (nth names 2) "_count")
                                                       (nth labelss 3)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 3)))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 3) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 3)))
                                 (m/make-metric-sample (str (nth names 2) "_bucket")
                                                       (assoc (nth labelss 3) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples! a-metric-store example-histogram-metric (nth labelss 3))))))))))

;; t-record-metric!-get-metric-samples!-global-store
;; including timestamp nil and not given for record-metric!

(t/deftest t-stored-value->metric-samples
  (t/testing "Getting metric-samples for a specific label from stored values
  works for all kind of metrics."
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::gen/gauge-metric)
                      example-counter-metric   (spec ::gen/counter-metric)
                      example-histogram-metric (spec ::gen/histogram-metric)
                      [label-x & labelss]      (spec (gen-distinct-metric-labels 4))
                      values                   (spec (gen-metric-values          3))
                      threshold                (spec ::gen/threshold)]
                     (let [empty-gauge-values      (m/make-gauge-values)
                           filled-gauge-values     (gen-filled-gauge-values empty-gauge-values labelss values)
                           empty-counter-values    (m/make-counter-values)
                           filled-counter-values   (gen-filled-counter-values empty-counter-values labelss values)
                           empty-histogram-values  (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)
                           basename                (m/histogram-metric-name example-histogram-metric)]
                       ;; GAUGES
                       ;; empty gauge-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-gauge-metric empty-gauge-values label-x)))
                       ;; labels not in gauge-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-gauge-metric filled-gauge-values label-x)))

                       (t/is (= [(m/make-metric-sample (m/gauge-metric-name example-gauge-metric)
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values  0))
                                                       (m/metric-value-last-update-time-ms (nth values  0)))]
                                (m/stored-value->metric-samples example-gauge-metric filled-gauge-values (nth labelss 0))))
                       ;; COUNTERS
                       ;; empty counter-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-counter-metric empty-counter-values label-x)))
                       ;; labels not in counter-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-counter-metric filled-counter-values label-x)))

                       (t/is (= [(m/make-metric-sample (m/counter-metric-name example-counter-metric)
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values  0))
                                                       (m/metric-value-last-update-time-ms (nth values  0)))]
                                (m/stored-value->metric-samples example-counter-metric filled-counter-values (nth labelss 0))))
                       ;; HISTOGRAMS
                       ;; empty histogram-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-histogram-metric empty-histogram-values label-x)))
                       ;; labels not in histogram-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-histogram-metric filled-histogram-values label-x)))

                       (t/is (= [(m/make-metric-sample (str basename "_sum")
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_count")
                                                       (nth labelss 0)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 0) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 0) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/stored-value->metric-samples example-histogram-metric filled-histogram-values (nth labelss 0))))))))))

(t/deftest t-stored-value->all-metric-samples
  (t/testing "Getting all metric-samples from stored values works for all kind
  of metrics."
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::gen/gauge-metric)
                      example-counter-metric   (spec ::gen/counter-metric)
                      example-histogram-metric (spec ::gen/histogram-metric)
                      labelss                  (spec (gen-distinct-metric-labels 3))
                      values                   (spec (gen-metric-values          3))
                      threshold                (spec ::gen/threshold)]
                     (let [empty-gauge-values      (m/make-gauge-values)
                           filled-gauge-values     (gen-filled-gauge-values empty-gauge-values labelss values)
                           empty-counter-values    (m/make-counter-values)
                           filled-counter-values   (gen-filled-counter-values empty-counter-values labelss values)
                           empty-histogram-values  (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)
                           basename                (m/histogram-metric-name example-histogram-metric)]
                     ;; GAUGES
                       (t/is (empty?
                              (m/stored-value->all-metric-samples example-gauge-metric empty-gauge-values)))
                       (t/is (= [(m/make-metric-sample (m/gauge-metric-name example-gauge-metric)
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values  0))
                                                       (m/metric-value-last-update-time-ms (nth values  0)))
                                 (m/make-metric-sample (m/gauge-metric-name example-gauge-metric)
                                                       (nth labelss 1)
                                                       (m/metric-value-value               (nth values  1))
                                                       (m/metric-value-last-update-time-ms (nth values  1)))
                                 (m/make-metric-sample (m/gauge-metric-name example-gauge-metric)
                                                       (nth labelss 2)
                                                       (m/metric-value-value               (nth values  2))
                                                       (m/metric-value-last-update-time-ms (nth values  2)))]
                                (m/stored-value->all-metric-samples example-gauge-metric filled-gauge-values)))
                     ;; COUNTERS
                       (t/is (empty?
                              (m/stored-value->all-metric-samples example-counter-metric empty-counter-values)))
                       (t/is (= [(m/make-metric-sample (m/counter-metric-name example-counter-metric)
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values  0))
                                                       (m/metric-value-last-update-time-ms (nth values  0)))
                                 (m/make-metric-sample (m/counter-metric-name example-counter-metric)
                                                       (nth labelss 1)
                                                       (m/metric-value-value               (nth values  1))
                                                       (m/metric-value-last-update-time-ms (nth values  1)))
                                 (m/make-metric-sample (m/counter-metric-name example-counter-metric)
                                                       (nth labelss 2)
                                                       (m/metric-value-value               (nth values  2))
                                                       (m/metric-value-last-update-time-ms (nth values  2)))]
                                (m/stored-value->all-metric-samples example-counter-metric filled-counter-values)))
                     ;; HISTOGRAMS
                       (t/is (empty?
                              (m/stored-value->all-metric-samples example-histogram-metric empty-histogram-values)))

                       (t/is (= [(m/make-metric-sample (str basename "_sum")
                                                       (nth labelss 0)
                                                       (m/metric-value-value               (nth values 0))
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_count")
                                                       (nth labelss 0)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 0) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 0) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 0)))
                                 (m/make-metric-sample (str basename "_sum")
                                                       (nth labelss 1)
                                                       (m/metric-value-value               (nth values 1))
                                                       (m/metric-value-last-update-time-ms (nth values 1)))
                                 (m/make-metric-sample (str basename "_count")
                                                       (nth labelss 1)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 1)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 1) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 1)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 1) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 1)))
                                 (m/make-metric-sample (str basename "_sum")
                                                       (nth labelss 2)
                                                       (m/metric-value-value               (nth values 2))
                                                       (m/metric-value-last-update-time-ms (nth values 2)))
                                 (m/make-metric-sample (str basename "_count")
                                                       (nth labelss 2)
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 2)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 2) :le "+Inf")
                                                       1.0
                                                       (m/metric-value-last-update-time-ms (nth values 2)))
                                 (m/make-metric-sample (str basename "_bucket")
                                                       (assoc (nth labelss 2) :le (str threshold))
                                                       (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                       (m/metric-value-last-update-time-ms (nth values 2)))]
                                (m/stored-value->all-metric-samples example-histogram-metric filled-histogram-values)))))))))

(t/deftest t-make-metric-sample-set
  (t/testing "Creating a metric-sample-set works."
    (t/is (quickcheck
           (property [name        (spec ::gen/metric-name)
                      type (spec ::gen/metric-type)
                      help        (spec ::gen/metric-help)
                      samples     (spec (gen-metric-samples 4))]
                     (let [example-metric-sample-set (m/make-metric-sample-set name
                                                                               type
                                                                               help
                                                                               samples)]
                       (t/is                (m/metric-sample-set?            example-metric-sample-set))
                       (t/is (= name        (m/metric-sample-set-name        example-metric-sample-set)))
                       (t/is (= type        (m/metric-sample-set-type example-metric-sample-set)))
                       (t/is (= help        (m/metric-sample-set-help        example-metric-sample-set)))
                       (t/is (= samples     (m/metric-sample-set-samples     example-metric-sample-set)))))))))

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

;; TODO: more elaborated metric-store (mixed-metric-store)
(t/deftest t-get-metric-sample-set-1
  (t/testing "Getting a metric sample set for a metric from a metric store
  works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 4))
                      values    (spec (gen-metric-values          4))]
                     (let [example-gauge-metric          (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric        (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric      (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           empty-metric-store            (m/fresh-metric-store-map)
                           filled-gauge-metric-store     (gen-filled-metric-store-map empty-metric-store
                                                                                      example-gauge-metric
                                                                                      labelss
                                                                                      values)
                           filled-counter-metric-store   (gen-filled-metric-store-map empty-metric-store
                                                                                      example-counter-metric
                                                                                      labelss
                                                                                      values)
                           filled-histogram-metric-store (gen-filled-metric-store-map empty-metric-store
                                                                                      example-histogram-metric
                                                                                      labelss
                                                                                      values)
                           basename (nth names 2)]
                       ;; GAUGES
                       (t/is (= [] (m/get-metric-sample-set-1 empty-metric-store example-gauge-metric)))
                       (t/is (= (m/make-metric-sample-set (nth names 0)
                                                          :gauge
                                                          (nth helps 0)
                                                          [(m/make-metric-sample (nth names 0)
                                                                                 (nth labelss 0)
                                                                                 (m/metric-value-value               (nth values 0))
                                                                                 (m/metric-value-last-update-time-ms (nth values 0)))
                                                           (m/make-metric-sample (nth names 0)
                                                                                 (nth labelss 1)
                                                                                 (m/metric-value-value               (nth values 1))
                                                                                 (m/metric-value-last-update-time-ms (nth values 1)))
                                                           (m/make-metric-sample (nth names 0)
                                                                                 (nth labelss 2)
                                                                                 (m/metric-value-value               (nth values 2))
                                                                                 (m/metric-value-last-update-time-ms (nth values 2)))
                                                           (m/make-metric-sample (nth names 0)
                                                                                 (nth labelss 3)
                                                                                 (m/metric-value-value               (nth values 3))
                                                                                 (m/metric-value-last-update-time-ms (nth values 3)))])
                                (m/get-metric-sample-set-1 filled-gauge-metric-store example-gauge-metric)))
                       ;; COUNTERS
                       (t/is (= [] (m/get-metric-sample-set-1 empty-metric-store example-counter-metric)))
                       (t/is (= (m/make-metric-sample-set (nth names 1)
                                                          :counter
                                                          (nth helps 1)
                                                          [(m/make-metric-sample (nth names 1)
                                                                                 (nth labelss 0)
                                                                                 (m/metric-value-value               (nth values 0))
                                                                                 (m/metric-value-last-update-time-ms (nth values 0)))
                                                           (m/make-metric-sample (nth names 1)
                                                                                 (nth labelss 1)
                                                                                 (m/metric-value-value               (nth values 1))
                                                                                 (m/metric-value-last-update-time-ms (nth values 1)))
                                                           (m/make-metric-sample (nth names 1)
                                                                                 (nth labelss 2)
                                                                                 (m/metric-value-value               (nth values 2))
                                                                                 (m/metric-value-last-update-time-ms (nth values 2)))
                                                           (m/make-metric-sample (nth names 1)
                                                                                 (nth labelss 3)
                                                                                 (m/metric-value-value               (nth values 3))
                                                                                 (m/metric-value-last-update-time-ms (nth values 3)))])
                                (m/get-metric-sample-set-1 filled-counter-metric-store example-counter-metric)))
                       ;; HISTOGRAMS
                       (t/is (= [] (m/get-metric-sample-set-1 empty-metric-store example-histogram-metric)))
                       (t/is (= (m/make-metric-sample-set basename
                                                          :histogram
                                                          (nth helps 2)
                                                          [(m/make-metric-sample (str basename "_sum")
                                                                                 (nth labelss 0)
                                                                                 (m/metric-value-value               (nth values 0))
                                                                                 (m/metric-value-last-update-time-ms (nth values 0)))
                                                           (m/make-metric-sample (str basename "_count")
                                                                                 (nth labelss 0)
                                                                                 1.0
                                                                                 (m/metric-value-last-update-time-ms (nth values 0)))
                                                           (m/make-metric-sample (str basename "_bucket")
                                                                                 (assoc (nth labelss 0) :le "+Inf")
                                                                                 1.0
                                                                                 (m/metric-value-last-update-time-ms (nth values 0)))
                                                           (m/make-metric-sample (str basename "_bucket")
                                                                                 (assoc (nth labelss 0) :le (str threshold))
                                                                                 (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                                 (m/metric-value-last-update-time-ms (nth values 0)))
                                                           (m/make-metric-sample (str basename "_sum")
                                                                                 (nth labelss 1)
                                                                                 (m/metric-value-value               (nth values 1))
                                                                                 (m/metric-value-last-update-time-ms (nth values 1)))
                                                           (m/make-metric-sample (str basename "_count")
                                                                                 (nth labelss 1)
                                                                                 1.0
                                                                                 (m/metric-value-last-update-time-ms (nth values 1)))
                                                           (m/make-metric-sample (str basename "_bucket")
                                                                                 (assoc (nth labelss 1) :le "+Inf")
                                                                                 1.0
                                                                                 (m/metric-value-last-update-time-ms (nth values 1)))
                                                           (m/make-metric-sample (str basename "_bucket")
                                                                                 (assoc (nth labelss 1) :le (str threshold))
                                                                                 (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                                 (m/metric-value-last-update-time-ms (nth values 1)))
                                                           (m/make-metric-sample (str basename "_sum")
                                                                                 (nth labelss 2)
                                                                                 (m/metric-value-value               (nth values 2))
                                                                                 (m/metric-value-last-update-time-ms (nth values 2)))
                                                           (m/make-metric-sample (str basename "_count")
                                                                                 (nth labelss 2)
                                                                                 1.0
                                                                                 (m/metric-value-last-update-time-ms (nth values 2)))
                                                           (m/make-metric-sample (str basename "_bucket")
                                                                                 (assoc (nth labelss 2) :le "+Inf")
                                                                                 1.0
                                                                                 (m/metric-value-last-update-time-ms (nth values 2)))
                                                           (m/make-metric-sample (str basename "_bucket")
                                                                                 (assoc (nth labelss 2) :le (str threshold))
                                                                                 (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                                 (m/metric-value-last-update-time-ms (nth values 2)))
                                                           (m/make-metric-sample (str basename "_sum")
                                                                                 (nth labelss 3)
                                                                                 (m/metric-value-value               (nth values 3))
                                                                                 (m/metric-value-last-update-time-ms (nth values 3)))
                                                           (m/make-metric-sample (str basename "_count")
                                                                                 (nth labelss 3)
                                                                                 1.0
                                                                                 (m/metric-value-last-update-time-ms (nth values 3)))
                                                           (m/make-metric-sample (str basename "_bucket")
                                                                                 (assoc (nth labelss 3) :le "+Inf")
                                                                                 1.0
                                                                                 (m/metric-value-last-update-time-ms (nth values 3)))
                                                           (m/make-metric-sample (str basename "_bucket")
                                                                                 (assoc (nth labelss 3) :le (str threshold))
                                                                                 (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                                 (m/metric-value-last-update-time-ms (nth values 3)))])
                                (m/get-metric-sample-set-1 filled-histogram-metric-store example-histogram-metric)))))))))

(t/deftest t-get-all-metric-sample-sets-1
  (t/testing "Getting all metric sample sets from a metric store works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 4))
                      values    (spec (gen-metric-values          4))]
                     (let [example-gauge-metric          (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric        (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric      (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           empty-metric-store            (m/fresh-metric-store-map)
                           filled-metric-store           (gen-filled-metric-store-map
                                                          (gen-filled-metric-store-map
                                                           (gen-filled-metric-store-map empty-metric-store
                                                                                        example-gauge-metric
                                                                                        labelss
                                                                                        values)
                                                           example-counter-metric
                                                           labelss
                                                           values)
                                                          example-histogram-metric
                                                          labelss
                                                          values)
                           basename (nth names 2)]
                       (t/is (= [] (m/get-all-metric-sample-sets-1 empty-metric-store)))
                       (t/is (= [(m/make-metric-sample-set (nth names 0)
                                                           :gauge
                                                           (nth helps 0)
                                                           [(m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])
                                 (m/make-metric-sample-set (nth names 1)
                                                           :counter
                                                           (nth helps 1)
                                                           [(m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])
                                 (m/make-metric-sample-set basename
                                                           :histogram
                                                           (nth helps 2)
                                                           [(m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 0)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 0) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 0) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 1)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 1) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 1) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 2)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 2) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 2) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 3)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 3) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 3) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])]
                                (m/get-all-metric-sample-sets-1 filled-metric-store)))))))))

(t/deftest t-get-all-metric-sample-sets!-global-store
  (t/testing "Getting all metric sample sets from the metric store (atom) works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 4))
                      values    (spec (gen-metric-values          4))]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           basename (nth names 2)]
                       (m/reset-global-metric-store!)

                       (t/is (= [] (m/get-all-metric-sample-sets!)))

                       (m/record-metric! example-gauge-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! example-counter-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! example-histogram-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (t/is (= [(m/make-metric-sample-set (nth names 0)
                                                           :gauge
                                                           (nth helps 0)
                                                           [(m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])
                                 (m/make-metric-sample-set (nth names 1)
                                                           :counter
                                                           (nth helps 1)
                                                           [(m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])
                                 (m/make-metric-sample-set basename
                                                           :histogram
                                                           (nth helps 2)
                                                           [(m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 0)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 0) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 0) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 1)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 1) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 1) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 2)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 2) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 2) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 3)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 3) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 3) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])]
                                (m/get-all-metric-sample-sets!)))))))))

(t/deftest t-get-all-metric-sample-sets!
  (t/testing "Getting all metric sample sets from the metric store (atom) works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 4))
                      values    (spec (gen-metric-values          4))]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           basename (nth names 2)
                           metric-store (m/fresh-metric-store)]

                       (t/is (= [] (m/get-all-metric-sample-sets! metric-store)))

                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (t/is (= [(m/make-metric-sample-set (nth names 0)
                                                           :gauge
                                                           (nth helps 0)
                                                           [(m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (nth names 0)
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])
                                 (m/make-metric-sample-set (nth names 1)
                                                           :counter
                                                           (nth helps 1)
                                                           [(m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (nth names 1)
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])
                                 (m/make-metric-sample-set basename
                                                           :histogram
                                                           (nth helps 2)
                                                           [(m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 0)
                                                                                  (m/metric-value-value               (nth values 0))
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 0)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 0) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 0) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 0)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 1)
                                                                                  (m/metric-value-value               (nth values 1))
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 1)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 1) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 1) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 1)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 2)
                                                                                  (m/metric-value-value               (nth values 2))
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 2)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 2) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 2) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 2)))
                                                            (m/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 3)
                                                                                  (m/metric-value-value               (nth values 3))
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 3)
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 3) :le "+Inf")
                                                                                  1.0
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))
                                                            (m/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 3) :le (str threshold))
                                                                                  (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                                  (m/metric-value-last-update-time-ms (nth values 3)))])]
                                (m/get-all-metric-sample-sets! metric-store)))))))))

;; >>> HELPER

(defn t-prune-stale-metrics!-prune-nothing
  "Metric sample set where nothing is pruned."
  [names helps labelss values basename threshold]
  [(m/make-metric-sample-set (nth names 0)
                             :gauge
                             (nth helps 0)
                             [(m/make-metric-sample (nth names 0)
                                                    (nth labelss 0)
                                                    (m/metric-value-value               (nth values 0))
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (nth names 0)
                                                    (nth labelss 1)
                                                    (m/metric-value-value               (nth values 1))
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (nth names 0)
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (nth names 0)
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])
   (m/make-metric-sample-set (nth names 1)
                             :counter
                             (nth helps 1)
                             [(m/make-metric-sample (nth names 1)
                                                    (nth labelss 0)
                                                    (m/metric-value-value               (nth values 0))
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (nth names 1)
                                                    (nth labelss 1)
                                                    (m/metric-value-value               (nth values 1))
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (nth names 1)
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (nth names 1)
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])
   (m/make-metric-sample-set basename
                             :histogram
                             (nth helps 2)
                             [(m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 0)
                                                    (m/metric-value-value               (nth values 0))
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 0)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 0) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 0) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 1)
                                                    (m/metric-value-value               (nth values 1))
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 1)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 1) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 1) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 2)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 2) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 2) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 3)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 3) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 3) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])])

(defn t-prune-stale-metrics!-prune-nothing
  "Metric sample set where nothing is pruned."
  [names helps labelss values basename threshold]
  [(m/make-metric-sample-set (nth names 0)
                             :gauge
                             (nth helps 0)
                             [(m/make-metric-sample (nth names 0)
                                                    (nth labelss 0)
                                                    (m/metric-value-value               (nth values 0))
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (nth names 0)
                                                    (nth labelss 1)
                                                    (m/metric-value-value               (nth values 1))
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (nth names 0)
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (nth names 0)
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])
   (m/make-metric-sample-set (nth names 1)
                             :counter
                             (nth helps 1)
                             [(m/make-metric-sample (nth names 1)
                                                    (nth labelss 0)
                                                    (m/metric-value-value               (nth values 0))
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (nth names 1)
                                                    (nth labelss 1)
                                                    (m/metric-value-value               (nth values 1))
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (nth names 1)
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (nth names 1)
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])
   (m/make-metric-sample-set basename
                             :histogram
                             (nth helps 2)
                             [(m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 0)
                                                    (m/metric-value-value               (nth values 0))
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 0)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 0) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 0) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                              (m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 1)
                                                    (m/metric-value-value               (nth values 1))
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 1)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 1) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 1) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                              (m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 2)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 2) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 2) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 3)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 3) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 3) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])])

(defn t-prune-stale-metrics!-prune-some
  "Metric sample set where some were pruned."
  [names helps labelss values basename threshold]
  [(m/make-metric-sample-set (nth names 0)
                             :gauge
                             (nth helps 0)
                             [(m/make-metric-sample (nth names 0)
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (nth names 0)
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])
   (m/make-metric-sample-set (nth names 1)
                             :counter
                             (nth helps 1)
                             [(m/make-metric-sample (nth names 1)
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (nth names 1)
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])
   (m/make-metric-sample-set basename
                             :histogram
                             (nth helps 2)
                             [(m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 2)
                                                    (m/metric-value-value               (nth values 2))
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 2)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 2) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 2) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                              (m/make-metric-sample (str basename "_sum")
                                                    (nth labelss 3)
                                                    (m/metric-value-value               (nth values 3))
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_count")
                                                    (nth labelss 3)
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 3) :le "+Inf")
                                                    1.0
                                                    (m/metric-value-last-update-time-ms (nth values 3)))
                              (m/make-metric-sample (str basename "_bucket")
                                                    (assoc (nth labelss 3) :le (str threshold))
                                                    (if (<= (m/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                    (m/metric-value-last-update-time-ms (nth values 3)))])])

;; <<< HELPER

(t/deftest t-prune-stale-metrics!-global-store
  (t/testing "Pruning all metrics in a metric store works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 4))]
                     (let [values                        [(m/make-metric-value 300 10)
                                                          (m/make-metric-value 300 12)
                                                          (m/make-metric-value 300 13)
                                                          (m/make-metric-value 300 15)]
                           example-gauge-metric          (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric        (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric      (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           basename (nth names 2)]

                       (m/reset-global-metric-store!)
                       (m/prune-stale-metrics! 1)
                       (t/is (= [] (m/get-all-metric-sample-sets!)))

                       (m/record-metric! example-gauge-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! example-counter-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! example-histogram-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))
                       ;; ;; not older
                       (m/prune-stale-metrics! 9)
                       (t/is (= (t-prune-stale-metrics!-prune-nothing names helps labelss values basename threshold)
                                (m/get-all-metric-sample-sets!)))
                       ;; ;; the same
                       (m/prune-stale-metrics! 10)
                       (t/is (= (t-prune-stale-metrics!-prune-nothing names helps labelss values basename threshold)
                                (m/get-all-metric-sample-sets!)))

                       ;; ;; mixture
                       (m/prune-stale-metrics! 13)
                       (t/is (= (t-prune-stale-metrics!-prune-some names helps labelss values basename threshold)
                                (m/get-all-metric-sample-sets!)))))))))

(t/deftest t-prune-stale-metrics!
  (t/testing "Pruning all metrics in a metric store works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labelss   (spec (gen-distinct-metric-labels 4))]
                     (let [values                        [(m/make-metric-value 300 10)
                                                          (m/make-metric-value 300 12)
                                                          (m/make-metric-value 300 13)
                                                          (m/make-metric-value 300 15)]
                           example-gauge-metric          (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric        (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric      (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           basename (nth names 2)
                           metric-store (m/fresh-metric-store)]

                       (m/prune-stale-metrics! metric-store 1)
                       (t/is (= [] (m/get-all-metric-sample-sets! metric-store)))

                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 0)
                                         (m/metric-value-value               (nth values 0))
                                         (m/metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 1)
                                         (m/metric-value-value               (nth values 1))
                                         (m/metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 2)
                                         (m/metric-value-value               (nth values 2))
                                         (m/metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 3)
                                         (m/metric-value-value               (nth values 3))
                                         (m/metric-value-last-update-time-ms (nth values 3)))
                       ;; ;; not older
                       (m/prune-stale-metrics! metric-store 9)

                       (t/is (= (t-prune-stale-metrics!-prune-nothing names helps labelss values basename threshold)
                                (m/get-all-metric-sample-sets! metric-store)))
                       ;; ;; the same
                       (m/prune-stale-metrics! metric-store 10)
                       (t/is (= (t-prune-stale-metrics!-prune-nothing names helps labelss values basename threshold)
                                (m/get-all-metric-sample-sets! metric-store)))

                       ;; ;; mixture
                       (m/prune-stale-metrics! metric-store 13)
                       (t/is (= (t-prune-stale-metrics!-prune-some names helps labelss values basename threshold)
                                (m/get-all-metric-sample-sets! metric-store)))))))))

;; COMMANDS on raw metrics

 (t/deftest t-record-metric
  (t/testing "Creating the `RecordMetric` record type works."
    (t/is (quickcheck
           (property [names       (spec (gen-distinct-metric-names  3))
                      helps       (spec (gen-metric-helps           3))
                      threshold   (spec ::gen/threshold)
                      labels      (spec ::gen/metric-labels)
                      value-value (spec ::gen/metric-value-value)
                      update-time (spec ::gen/metric-value-last-update-time-ms)]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])

                           example-gauge-recorded-metric-0 (m/record-metric example-gauge-metric labels value-value)
                           example-gauge-recorded-metric-1 (m/record-metric example-gauge-metric labels value-value update-time)

                           example-counter-recorded-metric-0 (m/record-metric example-counter-metric labels value-value)
                           example-counter-recorded-metric-1 (m/record-metric example-counter-metric labels value-value update-time)

                           example-histogram-recorded-metric-0 (m/record-metric example-histogram-metric labels value-value)
                           example-histogram-recorded-metric-1 (m/record-metric example-histogram-metric labels value-value update-time)]

                       (t/is (m/record-metric? example-gauge-recorded-metric-0))
                       (t/is (m/record-metric? example-gauge-recorded-metric-1))

                       (t/is (m/record-metric? example-counter-recorded-metric-0))
                       (t/is (m/record-metric? example-counter-recorded-metric-1))

                       (t/is (m/record-metric? example-histogram-recorded-metric-0))
                       (t/is (m/record-metric? example-histogram-recorded-metric-1))

                       (t/is (= example-gauge-metric (m/record-metric-metric example-gauge-recorded-metric-0)))
                       (t/is (= example-gauge-metric (m/record-metric-metric example-gauge-recorded-metric-1)))

                       (t/is (= example-counter-metric (m/record-metric-metric example-counter-recorded-metric-0)))
                       (t/is (= example-counter-metric (m/record-metric-metric example-counter-recorded-metric-1)))

                       (t/is (= example-histogram-metric (m/record-metric-metric example-histogram-recorded-metric-0)))
                       (t/is (= example-histogram-metric (m/record-metric-metric example-histogram-recorded-metric-1)))

                       (t/is (= labels (m/record-metric-labels example-gauge-recorded-metric-0)))
                       (t/is (= labels (m/record-metric-labels example-gauge-recorded-metric-1)))

                       (t/is (= labels (m/record-metric-labels example-counter-recorded-metric-0)))
                       (t/is (= labels (m/record-metric-labels example-counter-recorded-metric-1)))

                       (t/is (= labels (m/record-metric-labels example-histogram-recorded-metric-0)))
                       (t/is (= labels (m/record-metric-labels example-histogram-recorded-metric-1)))

                       (t/is (= value-value (m/record-metric-value example-gauge-recorded-metric-0)))
                       (t/is (= value-value (m/record-metric-value example-gauge-recorded-metric-1)))

                       (t/is (= value-value (m/record-metric-value example-counter-recorded-metric-0)))
                       (t/is (= value-value (m/record-metric-value example-counter-recorded-metric-1)))

                       (t/is (= value-value (m/record-metric-value example-histogram-recorded-metric-0)))
                       (t/is (= value-value (m/record-metric-value example-histogram-recorded-metric-1)))

                       (t/is (= nil         (m/record-metric-last-update example-gauge-recorded-metric-0)))
                       (t/is (= update-time (m/record-metric-last-update example-gauge-recorded-metric-1)))

                       (t/is (= nil         (m/record-metric-last-update example-counter-recorded-metric-0)))
                       (t/is (= update-time (m/record-metric-last-update example-counter-recorded-metric-1)))

                       (t/is (= nil         (m/record-metric-last-update example-histogram-recorded-metric-0)))
                       (t/is (= update-time (m/record-metric-last-update example-histogram-recorded-metric-1)))))))))

(t/deftest t-prune-stale-metrics
  (t/testing "Creating the `PruneStaleMetrics` record type works."
    (t/is (quickcheck
           (property [update-time (spec ::gen/metric-value-last-update-time-ms)]
                     (let [example-prune-stale-metrics  (m/prune-stale-metrics update-time)]

                       (t/is                (m/prune-stale-metrics?        example-prune-stale-metrics))
                       (t/is (= update-time (m/prune-stale-metrics-time-ms example-prune-stale-metrics)))))))))

(t/deftest t-get-metric-samples
  (t/testing "Creating the `GetMetricSamples` record type works."
    (t/is (quickcheck
           (property [names       (spec (gen-distinct-metric-names  3))
                      helps       (spec (gen-metric-helps           3))
                      threshold   (spec ::gen/threshold)
                      labels      (spec ::gen/metric-labels)]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])

                           example-gauge-get-metric-samples     (m/get-metric-samples example-gauge-metric labels)
                           example-counter-get-metric-samples   (m/get-metric-samples example-counter-metric labels)
                           example-histogram-get-metric-samples (m/get-metric-samples example-histogram-metric labels)]

                       (t/is (m/get-metric-samples? example-gauge-get-metric-samples))
                       (t/is (m/get-metric-samples? example-counter-get-metric-samples))
                       (t/is (m/get-metric-samples? example-histogram-get-metric-samples))

                       (t/is (= example-gauge-metric     (m/get-metric-samples-metric example-gauge-get-metric-samples)))
                       (t/is (= example-counter-metric   (m/get-metric-samples-metric example-counter-get-metric-samples)))
                       (t/is (= example-histogram-metric (m/get-metric-samples-metric example-histogram-get-metric-samples)))

                       (t/is (= labels (m/get-metric-samples-labels example-gauge-get-metric-samples)))
                       (t/is (= labels (m/get-metric-samples-labels example-counter-get-metric-samples)))
                       (t/is (= labels (m/get-metric-samples-labels example-histogram-get-metric-samples)))))))))

(t/deftest t-get-all-metric-sample-sets
  (t/testing "Creating the `GetAllMetricSampleSets` record type works."
    (t/is (m/get-all-metric-sample-sets? (m/get-all-metric-sample-sets)))))

;; run-metrics - will not get tested

(t/deftest t-record-and-get!-global-store
  (t/testing "Recording and getting the metric samples for the recorded metric
  with the given labels works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labels    (spec ::gen/metric-labels)
                      values    (spec (gen-metric-values          2))]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])]

                       (m/reset-global-metric-store!)

                       ;; GAUGES
                       (let [example-record-and-get! (m/record-and-get! example-gauge-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 0))
                                                                        (m/metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(m/make-metric-sample (nth names 0)
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))
                       (let [example-record-and-get! (m/record-and-get! example-gauge-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 1))
                                                                        (m/metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(m/make-metric-sample (nth names 0)
                                                         labels
                                                         (m/metric-value-value               (nth values 1))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))

                       ;; COUNTERS
                       (let [example-record-and-get! (m/record-and-get! example-counter-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 0))
                                                                        (m/metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(m/make-metric-sample (nth names 1)
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))
                       (let [example-record-and-get! (m/record-and-get! example-counter-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 1))
                                                                        (m/metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(m/make-metric-sample (nth names 1)
                                                         labels
                                                         (+ (m/metric-value-value (nth values 0))
                                                            (m/metric-value-value (nth values 1)))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))

                       ;; HISTOGRAMS
                       (let [example-record-and-get! (m/record-and-get! example-histogram-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 0))
                                                                        (m/metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_count")
                                                         labels
                                                         1.0
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le "+Inf")
                                                         1.0
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le (str threshold))
                                                         (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))

                       (let [example-record-and-get! (m/record-and-get! example-histogram-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 1))
                                                                        (m/metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                         labels
                                                         (+ (m/metric-value-value (nth values 0))
                                                            (m/metric-value-value (nth values 1)))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_count")
                                                         labels
                                                         2.0
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le "+Inf")
                                                         2.0
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le (str threshold))
                                                         (+ (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                            (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))))))))

(t/deftest t-record-and-get!
  (t/testing "Recording and getting the metric samples for the recorded metric
  with the given labels works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labels    (spec ::gen/metric-labels)
                      values    (spec (gen-metric-values          2))]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           metric-store             (m/fresh-metric-store)]

                       ;; GAUGES
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-gauge-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 0))
                                                                        (m/metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(m/make-metric-sample (nth names 0)
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-gauge-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 1))
                                                                        (m/metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(m/make-metric-sample (nth names 0)
                                                         labels
                                                         (m/metric-value-value               (nth values 1))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))

                       ;; COUNTERS
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-counter-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 0))
                                                                        (m/metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(m/make-metric-sample (nth names 1)
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-counter-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 1))
                                                                        (m/metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(m/make-metric-sample (nth names 1)
                                                         labels
                                                         (+ (m/metric-value-value (nth values 0))
                                                            (m/metric-value-value (nth values 1)))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))

                       ;; HISTOGRAMS
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-histogram-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 0))
                                                                        (m/metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_count")
                                                         labels
                                                         1.0
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le "+Inf")
                                                         1.0
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le (str threshold))
                                                         (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))

                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-histogram-metric
                                                                        labels
                                                                        (m/metric-value-value               (nth values 1))
                                                                        (m/metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                         labels
                                                         (+ (m/metric-value-value (nth values 0))
                                                            (m/metric-value-value (nth values 1)))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_count")
                                                         labels
                                                         2.0
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le "+Inf")
                                                         2.0
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le (str threshold))
                                                         (+ (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                            (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))))))))

(t/deftest t-record-and-get
  (t/testing "Monadically recording and getting the metric samples for the
  recorded metric with the given labels works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::gen/threshold)
                      labels    (spec ::gen/metric-labels)
                      values    (spec (gen-metric-values          2))]
                     (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])]

                       (m/reset-global-metric-store!)

                       ;; GAUGES
                       (let [[example-record-and-get-1 example-record-and-get-2]
                             (mock-monad/mock-run-monad
                              m/monad-command-config
                              []
                              (monad/monadic
                               [example-record-and-get-1
                                (m/record-and-get example-gauge-metric
                                                  labels
                                                  (m/metric-value-value               (nth values 0))
                                                  (m/metric-value-last-update-time-ms (nth values 0)))]
                               [example-record-and-get-2
                                (m/record-and-get example-gauge-metric
                                                  labels
                                                  (m/metric-value-value               (nth values 1))
                                                  (m/metric-value-last-update-time-ms (nth values 1)))]
                               (monad/return [example-record-and-get-1 example-record-and-get-2])))]
                         (t/is (= [(m/make-metric-sample (nth names 0)
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get-1))
                         (t/is (= [(m/make-metric-sample (nth names 0)
                                                         labels
                                                         (m/metric-value-value               (nth values 1))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get-2)))

                       (m/reset-global-metric-store!)

                       ;; COUNTERS
                       (let [[example-record-and-get-1 example-record-and-get-2]
                             (mock-monad/mock-run-monad
                              m/monad-command-config
                              []
                              (monad/monadic
                               [example-record-and-get-1
                                (m/record-and-get example-counter-metric
                                                  labels
                                                  (m/metric-value-value               (nth values 0))
                                                  (m/metric-value-last-update-time-ms (nth values 0)))]
                               [example-record-and-get-2
                                (m/record-and-get example-counter-metric
                                                  labels
                                                  (m/metric-value-value               (nth values 1))
                                                  (m/metric-value-last-update-time-ms (nth values 1)))]

                               (monad/return [example-record-and-get-1 example-record-and-get-2])))]
                         (t/is (= [(m/make-metric-sample (nth names 1)
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get-1))
                         (t/is (= [(m/make-metric-sample (nth names 1)
                                                         labels
                                                         (+ (m/metric-value-value (nth values 0))
                                                            (m/metric-value-value (nth values 1)))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get-2)))

                       (m/reset-global-metric-store!)

                       ;; HISTOGRAMS
                       (let [[example-record-and-get-1 example-record-and-get-2]
                             (mock-monad/mock-run-monad
                              m/monad-command-config
                              []
                              (monad/monadic
                               [example-record-and-get-1

                                (m/record-and-get example-histogram-metric
                                                  labels
                                                  (m/metric-value-value               (nth values 0))
                                                  (m/metric-value-last-update-time-ms (nth values 0)))]
                               [example-record-and-get-2
                                (m/record-and-get example-histogram-metric
                                                  labels
                                                  (m/metric-value-value               (nth values 1))
                                                  (m/metric-value-last-update-time-ms (nth values 1)))]
                               (monad/return [example-record-and-get-1 example-record-and-get-2])))]
                         (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                         labels
                                                         (m/metric-value-value               (nth values 0))
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_count")
                                                         labels
                                                         1.0
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le "+Inf")
                                                         1.0
                                                         (m/metric-value-last-update-time-ms (nth values 0)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le (str threshold))
                                                         (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                         (m/metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get-1))

                         (t/is (= [(m/make-metric-sample (str (nth names 2) "_sum")
                                                         labels
                                                         (+ (m/metric-value-value (nth values 0))
                                                            (m/metric-value-value (nth values 1)))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_count")
                                                         labels
                                                         2.0
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le "+Inf")
                                                         2.0
                                                         (m/metric-value-last-update-time-ms (nth values 1)))
                                   (m/make-metric-sample (str (nth names 2) "_bucket")
                                                         (assoc labels :le (str threshold))
                                                         (+ (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                            (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0))
                                                         (m/metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get-2)))))))))

(t/deftest t-histogram-with-many-buckets
  (let [metric-name "http_request_duration_seconds"
        histogram-metric (m/make-histogram-metric metric-name "HELP" [0.1 0.2 0.3 0.45])
        recorded-metrics [0.25 0.35 0.25 0.2 0.15 0.5 0.5]
        expected-result [["_sum"     nil   (double (apply + recorded-metrics))]
                         ["_count"   nil   (double (count recorded-metrics))]
                         ["_bucket" "+Inf" (double (count recorded-metrics))]
                         ["_bucket" "0.1"  (double (count (filter #(<= % 0.1) recorded-metrics)))]
                         ["_bucket" "0.2"  (double (count (filter #(<= % 0.2) recorded-metrics)))]
                         ["_bucket" "0.3"  (double (count (filter #(<= % 0.3) recorded-metrics)))]
                         ["_bucket" "0.45" (double (count (filter #(<= % 0.45) recorded-metrics)))]]
        expected-sample-set
        (m/make-metric-sample-set metric-name :histogram "HELP"
                                  (mapv (fn [[postfix maybe-le value]]
                                          (m/make-metric-sample (str metric-name postfix) (if maybe-le {:le maybe-le} {}) value 0))
                                        expected-result))]
    (m/reset-global-metric-store!)
    (doseq [value recorded-metrics]
      (m/record-and-get! histogram-metric {} value 0))
    (t/is (= [expected-sample-set] (m/get-all-metric-sample-sets!)))
    (m/reset-global-metric-store!)))

(t/deftest t-use-counter-like-gauge
  (let [metric-name "http_request_total"
        counter-metric (m/make-counter-metric metric-name "HELP" true)]
    (m/reset-global-metric-store!)
    (m/record-and-get! counter-metric {} 23 0)
    (m/record-and-get! counter-metric {} 42 1)
    (t/is (= [(m/make-metric-sample-set "http_request_total" :counter "HELP"
                                      [(m/make-metric-sample "http_request_total" {} 42.0 1)])]
             (m/get-all-metric-sample-sets!)))
    (m/reset-global-metric-store!)))

;; prometheus seems to be okay for counters to jump from a high number back to 0
;; it looks like that we will stay at Double/MAX_VALUE or run/stay in #Inf
(t/deftest t-counter-dealing-with-max-value
  (let [metric-name "counter-limit"
        counter-metric (m/make-counter-metric metric-name "HELP")]
    (m/reset-global-metric-store!)

    (m/record-and-get! counter-metric {} Double/MAX_VALUE 0)

    (t/is (= [(m/make-metric-sample-set "counter-limit" :counter "HELP"
                                        [(m/make-metric-sample "counter-limit" {} Double/MAX_VALUE  0)])]
             (m/get-all-metric-sample-sets!)))

    (m/record-and-get! counter-metric {} 2 1)

    (t/is (= [(m/make-metric-sample-set "counter-limit" :counter "HELP"
                                        [(m/make-metric-sample "counter-limit" {} Double/MAX_VALUE 1)])]
             (m/get-all-metric-sample-sets!)))))

;; we are storing doubles in MetricValue
(t/deftest t-histogram-dealing-with-big-ints
  (let [histogram-metric (m/make-histogram-metric "name" "help" [5])
        metric-store     (m/fresh-metric-store)]

    (m/record-and-get! metric-store histogram-metric {} 999999999999999999999999 0)
    (m/record-and-get! metric-store histogram-metric {} 999999999999999999999999 0)

    ;; no exception raised
    (t/is true)))

;; we are storing doubles in MetricValue
(t/deftest t-histogram-dealing-with-longs
  (let [histogram-metric (m/make-histogram-metric "name" "help" [5])
        metric-store     (m/fresh-metric-store)]

    (m/record-and-get! metric-store histogram-metric {} 8999999999999999991 0)
    (m/record-and-get! metric-store histogram-metric {}  999999999999999999 0)

    ;; no exception raised
    (t/is true)))
