(ns active.clojure.logger.metric-values-test
  (:require [active.clojure.logger.metric-values :as m]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-accumulator-test :as util]
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

;; GENERATORS

(defn gen-metric-values
  [num-elems]
  (s/spec (s/coll-of ::m/metric-value :distinct false :into [] :count num-elems)))

(defn gen-filled-singular-values
  [singular-values labelss values]
  (if (empty? labelss)
    singular-values
    (let [[labels & rest-labelss] labelss
          [value & rest-values] values]
      (gen-filled-singular-values
       (m/set-singular-values singular-values labels value)
       rest-labelss
       rest-values))))

(defn gen-filled-histogram-values
  [histogram-values labelss values]
  (if (empty? labelss)
    histogram-values
    (let [[labels & rest-labelss] labelss
          [value & rest-values] values]
      (gen-filled-histogram-values
       (m/update-histogram-values histogram-values labels value)
       rest-labelss
       rest-values))))

(t/deftest t-make-metric-value
  (t/testing "All fields of a metric-value are set correct."
    (t/is (quickcheck
           (property [value       (spec ::m/metric-value-value)
                      update-time (spec ::m/metric-value-last-update-time-ms)]
                     (let [example-metric-value (m/make-metric-value value update-time)]
                       (and (m/metric-value?                    example-metric-value)
                            (= (double value) (m/metric-value-value               example-metric-value))
                            (= update-time    (m/metric-value-last-update-time-ms example-metric-value)))))))))

(t/deftest t-set-metric-value
  (t/testing "Setting a metric value works."
    (t/is (quickcheck
           (property [[labels & labelss]           (spec (util/gen-distinct-metric-labels 6))
                      [value  & values]           (spec (gen-metric-values          6))]
                     (let [map-empty {}
                           map-prefilled (zipmap labelss values)
                           labels-to-change (nth labelss 3)]

                       (and (= {labels value} (m/set-metric-value map-empty labels value))
                            (= {(nth labelss 0)   (nth values 0)
                                (nth labelss 1)   (nth values 1)
                                (nth labelss 2)   (nth values 2)
                                labels-to-change  value
                                (nth labelss 4)   (nth values 4)}
                               (m/set-metric-value map-prefilled labels-to-change value)))))))))

(t/deftest t-update-metric-value
  ;; TODO: More functions? Not only '+'?
  (t/testing "Basic update of metric-values works."
    (t/is (quickcheck
           (property [value (spec ::m/metric-value)]
                     ;; If there is no base value, just take the second.
                     (= value
                        (m/update-metric-value + nil value)))))
    (t/is (quickcheck
           (property [example-metric-value-1 (spec ::m/metric-value)
                      example-metric-value-2 (spec ::m/metric-value)]
                     ;; Add value-1 and value-2. Take update-time from 2.
                     (let [value-1       (m/metric-value-value               example-metric-value-1)
                           value-2       (m/metric-value-value               example-metric-value-2)
                           update-time-2 (m/metric-value-last-update-time-ms example-metric-value-2)]
                       (= (m/make-metric-value (+ value-1 value-2) update-time-2)
                          (m/update-metric-value + example-metric-value-1 example-metric-value-2))))))))

(t/deftest t-inc-metric-value
  (t/testing "Increment of metric-values in a labels-value-map works."
    (t/is (quickcheck
           (property [[labels & labelss] (spec (util/gen-distinct-metric-labels 6))
                      [value  & values] (spec (gen-metric-values          6))]
                     (let [map-empty {}
                           map-prefilled (zipmap labelss values)
                           labels-to-change (nth labelss 3)
                           value-to-change  (nth values  3)]

                       (and (= {labels value}
                               (m/inc-metric-value map-empty labels value))
                            (= {(nth labelss 0) (nth values 0)
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
           (property [labelss (spec (util/gen-distinct-metric-labels 6))]
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

(t/deftest t-make-singular-values
  (t/testing "All fields of a metric-value are set correct."
    (let [example-singular-values (m/make-singular-values)]
      (t/is                       (m/singular-values?    example-singular-values))
      (t/is (= m/empty-values-map (m/singular-values-map example-singular-values))))))

(t/deftest t-set-singular-values
  (t/testing "Setting new values works"
    (t/is (quickcheck
           (property [labels (spec ::metric-types/metric-labels)
                      value (spec ::m/metric-value)]
                     (= value
                        (get (m/singular-values-map (m/set-singular-values m/empty-singular-values labels value))
                             labels))))))
  (t/testing "Setting existing value works"
    (t/is (quickcheck
           (property [singular-values (spec (s/and ::m/singular-values #(not (m/empty-singular-values? %))))
                      value (spec ::m/metric-value)]
                     (let [labels (first (keys (m/singular-values-map singular-values)))]
                       (= value
                          (get (m/singular-values-map (m/set-singular-values singular-values labels value))
                               labels))))))))

(t/deftest t-inc-singular-values
  (t/testing "Inc empty singular-values works"
    (t/is (quickcheck
           (property [labels (spec ::metric-types/metric-labels)
                      value (spec ::m/metric-value)]
                     (= value
                        (get (m/singular-values-map (m/inc-singular-values m/empty-singular-values labels value))
                             labels))))))
  (t/testing "Inc filled singular-values works"
    (t/is (quickcheck
           (property [singular-values (spec (s/and ::m/singular-values #(not (m/empty-singular-values? %))))
                      value (spec ::m/metric-value)]
                     (let [labels (first (keys (m/singular-values-map singular-values)))
                           current-value (get (m/singular-values-map singular-values) labels)]
                       (= (m/update-metric-value + current-value value)
                          (get (m/singular-values-map (m/inc-singular-values singular-values labels value))
                               labels))))))))

(t/deftest t-singular-values->metric-samples
  (t/testing "Creating metric-samples from singular-values works."
    (t/is (quickcheck
           (property [name (spec ::metric-types/metric-name)
                      labels (spec ::metric-types/metric-labels)]
                     (= [] (m/singular-values->metric-samples name m/empty-singular-values labels)))))
    
    (t/is (quickcheck
           (property [name (spec ::metric-types/metric-name)
                      [label-x & labelss] (spec (util/gen-distinct-metric-labels 4))
                      values              (spec (gen-metric-values          3))]
                     (let [filled-singular-values (gen-filled-singular-values m/empty-singular-values labelss values)]
                       ;; a label not in singular-values-map
                       (and (= [] (m/singular-values->metric-samples name filled-singular-values label-x))
                            ;; and a label in the map
                            (= [(metric-samples/make-metric-sample name
                                                                   (nth labelss 0)
                                                                   (m/metric-value-value               (nth values  0))
                                                                   (m/metric-value-last-update-time-ms (nth values  0)))]
                               (m/singular-values->metric-samples name filled-singular-values (nth labelss 0))))))))))

(t/deftest t-prune-stale-singular-values
  (t/testing "Pruning stale singular-values works."
    (t/is (quickcheck
           (property [labelss (spec (util/gen-distinct-metric-labels 6))]
                     (let [values [(m/make-metric-value 300 10)
                                   (m/make-metric-value 300 11)
                                   (m/make-metric-value 300 12)
                                   (m/make-metric-value 300 13)
                                   (m/make-metric-value 300 14)
                                   (m/make-metric-value 300 15)]
                           filled-singular-values (gen-filled-singular-values m/empty-singular-values labelss values)]
                       ;; empty labels-value-map
                       (t/is (= {}
                                (m/singular-values-map (m/prune-stale-singular-values m/empty-singular-values 1))))
                       ;; not older
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/singular-values-map (m/prune-stale-singular-values filled-singular-values 9))))
                       ;; the same
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/singular-values-map (m/prune-stale-singular-values filled-singular-values 10))))
                       ;; mixture
                       (t/is (= {(nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/singular-values-map (m/prune-stale-singular-values filled-singular-values 13))))))))))

(t/deftest t-empty-singular-values?
  (t/testing "Checking whether singular-values-map is empty works."
    (t/is (quickcheck
           (property [labelss (spec (util/gen-distinct-metric-labels 6))
                      values  (spec (gen-metric-values          6))]
                     (let [filled-singular-values (gen-filled-singular-values m/empty-singular-values labelss values)]
                       (t/is (= true  (m/empty-singular-values? m/empty-singular-values)))
                       (t/is (= false (m/empty-singular-values? filled-singular-values)))))))))

(t/deftest t-make-histogram-values
  (t/testing "All fields of a metric-value are set correct."
    (t/is (quickcheck
           (property [threshold (spec ::metric-types/threshold)]
                     (let [example-histogram-values (m/make-histogram-values [threshold])]
                       (t/is                       (m/histogram-values?           example-histogram-values))
                       (t/is (= [threshold]        (m/histogram-values-thresholds example-histogram-values)))
                       (t/is (= m/empty-values-map (m/histogram-values-map        example-histogram-values)))))))))

(t/deftest t-histogram-values->metric-samples
  (t/testing "Creating metric-samples from histogram-values works."
    (t/is (quickcheck
           (property [basename            (spec ::metric-types/metric-name)
                      threshold           (spec ::metric-types/threshold)
                      [label-x & labelss] (spec (util/gen-distinct-metric-labels 4))
                      values              (spec (gen-metric-values          3))]
                     (let [empty-histogram-values (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)]
                       ;; empty histogram-values-map
                       (t/is (empty? (m/histogram-values->metric-samples basename empty-histogram-values label-x)))
                       ;; labels not in histogram-values-map
                       (t/is (empty? (m/histogram-values->metric-samples basename filled-histogram-values label-x)))
                       (t/is (= [(metric-samples/make-metric-sample (str basename "_sum")
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values 0))
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_count")
                                                                    (nth labelss 0)
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 0) :le "+Inf")
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 0) :le (str threshold))
                                                                    (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/histogram-values->metric-samples basename filled-histogram-values (nth labelss 0))))))))))

(t/deftest t-prune-stale-histogram-values
  (t/testing "Pruning stale histogram-values works."
    (t/is (quickcheck
           (property [threshold (spec ::metric-types/threshold)
                      labelss   (spec (util/gen-distinct-metric-labels 6))]
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
  (t/testing "Checking whether histogram-values is empty works."
    (t/is (quickcheck
           (property [threshold (spec ::metric-types/threshold)
                      labelss   (spec (util/gen-distinct-metric-labels 6))
                      values    (spec (gen-metric-values          6))]
                     (let [empty-histogram-values (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)]
                       (t/is (= true  (m/empty-histogram-values? empty-histogram-values)))
                       (t/is (= false (m/empty-histogram-values? filled-histogram-values)))))))))


;; TODO: testing with more elaborated value-counts? That is, not only where each label has the value 1.
;; TODO: testing that threshold for histogram doesn't change.
(t/deftest t-update-or-make-stored-values
  (t/testing "Updating stored values works."
    (t/is (quickcheck
           (property [[labels-x & labelss] (spec (util/gen-distinct-metric-labels 7))
                      [value-x  & values]  (spec (gen-metric-values          7))
                      threshold            (spec ::metric-types/threshold)]
                     (let [filled-singular-values  (gen-filled-singular-values  m/empty-singular-values               labelss values)
                           filled-histogram-values (gen-filled-histogram-values (m/make-histogram-values [threshold]) labelss values)

                           counter-metric (metric-types/make-counter-metric "foo" "")
                           gauge-metric (metric-types/make-gauge-metric "foo" "")
                           histogram-metric (metric-types/make-histogram-metric "foo" "" [threshold])]
                       
                       ;; Singular gauge values
                       (t/is (= {labels-x value-x}
                                (m/singular-values-map (m/update-or-make-stored-values m/empty-singular-values gauge-metric labels-x value-x))))
                       (t/is (= (assoc (m/singular-values-map filled-singular-values)
                                       labels-x value-x)
                                (m/singular-values-map (m/update-or-make-stored-values filled-singular-values gauge-metric labels-x value-x))))
                       (t/is (= (assoc (m/singular-values-map filled-singular-values)
                                       (nth labelss 2) value-x)
                                (m/singular-values-map (m/update-or-make-stored-values filled-singular-values gauge-metric (nth labelss 2) value-x))))
                       ;; Singular counter values
                       (t/is (= {labels-x value-x}
                                (m/singular-values-map (m/update-or-make-stored-values m/empty-singular-values counter-metric labels-x value-x))))
                       (t/is (= (assoc (m/singular-values-map filled-singular-values)
                                       labels-x value-x)
                                (m/singular-values-map (m/update-or-make-stored-values filled-singular-values counter-metric labels-x value-x))))
                       (t/is (= (m/inc-metric-value (m/singular-values-map filled-singular-values) (nth labelss 2) value-x)
                                (m/singular-values-map (m/update-or-make-stored-values filled-singular-values counter-metric (nth labelss 2) value-x))))
                       
                       ;; Histogram values
                       (t/is (= {labels-x (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value-x)
                                                                          (m/metric-value-value value-x)
                                                                          1.0
                                                                          [(if (<= (m/metric-value-value value-x) threshold) 1.0 0.0)])}
                                (m/histogram-values-map (m/update-or-make-stored-values (m/make-histogram-values [threshold]) histogram-metric labels-x value-x))))
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
                                (m/histogram-values-map (m/update-or-make-stored-values filled-histogram-values histogram-metric labels-x value-x))))
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
                                (m/histogram-values-map (m/update-or-make-stored-values filled-histogram-values histogram-metric (nth labelss 2) value-x))))))))))

#_(t/deftest t-make-stored-values
  (t/testing "Making stored values works."
    ;; TODO: breaks any abstraction; should not be tested like this.
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::metric-types/gauge-metric)
                      example-counter-metric   (spec ::metric-types/counter-metric)
                      example-histogram-metric (spec ::metric-types/histogram-metric)
                      labels                   (spec ::metric-types/metric-labels)
                      value                    (spec ::m/metric-value)]
                     (let [gauge-stored-values     (m/make-stored-values example-gauge-metric     labels value)
                           counter-stored-values   (m/make-stored-values example-counter-metric   labels value)
                           histogram-stored-values (m/make-stored-values example-histogram-metric labels value)]
                       ;; Gauge
                       (t/is (m/singular-values? gauge-stored-values))
                       (t/is (= {labels value}
                                (m/singular-values-map gauge-stored-values)))

                       ;; Counter
                       (t/is (m/singular-values? counter-stored-values))
                       (t/is (= {labels value}
                                (m/singular-values-map counter-stored-values)))

                       ;; Histogram
                       (t/is (m/histogram-values? histogram-stored-values))
                       (t/is (= (metric-types/histogram-metric-thresholds example-histogram-metric)
                                (m/histogram-values-thresholds histogram-stored-values)))
                       (t/is (= {labels (m/make-histogram-metric-values (m/metric-value-last-update-time-ms value)
                                                                        (m/metric-value-value value)
                                                                        1.0
                                                                        (mapv (fn [threshold]
                                                                                (if (<= (m/metric-value-value value)
                                                                                        threshold)
                                                                                  1.0 0.0))
                                                                              (metric-types/histogram-metric-thresholds example-histogram-metric)))}
                                (m/histogram-values-map histogram-stored-values)))))))))

(t/deftest t-prune-stale-stored-values
  (t/testing "Pruning stale stored-values works."
    (t/is (quickcheck
           (property [labelss   (spec (util/gen-distinct-metric-labels 6))
                      threshold (spec ::metric-types/threshold)]
                     (let [empty-histogram-values (m/make-histogram-values [threshold])
                           values [(m/make-metric-value 300 10)
                                   (m/make-metric-value 300 11)
                                   (m/make-metric-value 300 12)
                                   (m/make-metric-value 300 13)
                                   (m/make-metric-value 300 14)
                                   (m/make-metric-value 300 15)]
                           filled-singular-values (gen-filled-singular-values m/empty-singular-values labelss values)
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)]
                       ;; Singular values
                       (t/is (= {}
                                (m/singular-values-map (m/prune-stale-stored-values m/empty-singular-values 1))))
                       ;; not older
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/singular-values-map (m/prune-stale-stored-values filled-singular-values 9))))
                       ;; the same
                       (t/is (= {(nth labelss 0) (nth values 0)
                                 (nth labelss 1) (nth values 1)
                                 (nth labelss 2) (nth values 2)
                                 (nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/singular-values-map (m/prune-stale-stored-values filled-singular-values 10))))
                       ;; mixture
                       (t/is (= {(nth labelss 3) (nth values 3)
                                 (nth labelss 4) (nth values 4)
                                 (nth labelss 5) (nth values 5)}
                                (m/singular-values-map (m/prune-stale-stored-values filled-singular-values 13))))

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
  ;; Note: it's actually more a test of update-or-make-stored-value
  ;; FIXME: this does not hold for some seeds (but they are random; which makes it flacky)
  (t/testing "Checking whether a stored-values-map is empty works."
    (t/is (quickcheck
           (property [stored-values (spec ::m/stored-values)
                      metric (spec ::metric-types/metric)
                      labels (spec ::metric-types/metric-labels)
                      value (spec ::m/metric-value)]
                     (not (m/empty-stored-values? (m/update-or-make-stored-values stored-values metric labels value))))))))


(t/deftest t-stored-value->metric-samples
  (t/testing "Getting metric-samples for a specific label from stored values
  works for all kind of metrics."
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::metric-types/gauge-metric)
                      example-counter-metric   (spec ::metric-types/counter-metric)
                      example-histogram-metric (spec ::metric-types/histogram-metric)
                      [label-x & labelss]      (spec (util/gen-distinct-metric-labels 4))
                      values                   (spec (gen-metric-values          3))
                      threshold                (spec ::metric-types/threshold)]
                     (let [empty-gauge-values      m/empty-singular-values
                           filled-gauge-values     (gen-filled-singular-values empty-gauge-values labelss values)
                           empty-counter-values    m/empty-singular-values
                           filled-counter-values   (gen-filled-singular-values empty-counter-values labelss values)
                           empty-histogram-values  (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)
                           basename                (metric-types/histogram-metric-name example-histogram-metric)]
                       ;; GAUGES
                       ;; empty gauge-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-gauge-metric empty-gauge-values label-x)))
                       ;; labels not in gauge-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-gauge-metric filled-gauge-values label-x)))

                       (t/is (= [(metric-samples/make-metric-sample (metric-types/gauge-metric-name example-gauge-metric)
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values  0))
                                                                    (m/metric-value-last-update-time-ms (nth values  0)))]
                                (m/stored-value->metric-samples example-gauge-metric filled-gauge-values (nth labelss 0))))
                       ;; COUNTERS
                       ;; empty counter-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-counter-metric empty-counter-values label-x)))
                       ;; labels not in counter-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-counter-metric filled-counter-values label-x)))

                       (t/is (= [(metric-samples/make-metric-sample (metric-types/counter-metric-name example-counter-metric)
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values  0))
                                                                    (m/metric-value-last-update-time-ms (nth values  0)))]
                                (m/stored-value->metric-samples example-counter-metric filled-counter-values (nth labelss 0))))
                       ;; HISTOGRAMS
                       ;; empty histogram-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-histogram-metric empty-histogram-values label-x)))
                       ;; labels not in histogram-values-map
                       (t/is (= [] (m/stored-value->metric-samples example-histogram-metric filled-histogram-values label-x)))

                       (t/is (= [(metric-samples/make-metric-sample (str basename "_sum")
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values 0))
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_count")
                                                                    (nth labelss 0)
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 0) :le "+Inf")
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 0) :le (str threshold))
                                                                    (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))]
                                (m/stored-value->metric-samples example-histogram-metric filled-histogram-values (nth labelss 0))))))))))

(t/deftest t-stored-value->all-metric-samples
  (t/testing "Getting all metric-samples from stored values works for all kind
  of metrics."
    (t/is (quickcheck
           (property [example-gauge-metric     (spec ::metric-types/gauge-metric)
                      example-counter-metric   (spec ::metric-types/counter-metric)
                      example-histogram-metric (spec ::metric-types/histogram-metric)
                      labelss                  (spec (util/gen-distinct-metric-labels 3))
                      values                   (spec (gen-metric-values          3))
                      threshold                (spec ::metric-types/threshold)]
                     (let [empty-gauge-values      m/empty-singular-values
                           filled-gauge-values     (gen-filled-singular-values empty-gauge-values labelss values)
                           empty-counter-values    m/empty-singular-values
                           filled-counter-values   (gen-filled-singular-values empty-counter-values labelss values)
                           empty-histogram-values  (m/make-histogram-values [threshold])
                           filled-histogram-values (gen-filled-histogram-values empty-histogram-values labelss values)
                           basename                (metric-types/histogram-metric-name example-histogram-metric)]
                       ;; GAUGES
                       (t/is (empty?
                              (m/stored-value->all-metric-samples example-gauge-metric empty-gauge-values)))
                       (t/is (= [(metric-samples/make-metric-sample (metric-types/gauge-metric-name example-gauge-metric)
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values  0))
                                                                    (m/metric-value-last-update-time-ms (nth values  0)))
                                 (metric-samples/make-metric-sample (metric-types/gauge-metric-name example-gauge-metric)
                                                                    (nth labelss 1)
                                                                    (m/metric-value-value               (nth values  1))
                                                                    (m/metric-value-last-update-time-ms (nth values  1)))
                                 (metric-samples/make-metric-sample (metric-types/gauge-metric-name example-gauge-metric)
                                                                    (nth labelss 2)
                                                                    (m/metric-value-value               (nth values  2))
                                                                    (m/metric-value-last-update-time-ms (nth values  2)))]
                                (m/stored-value->all-metric-samples example-gauge-metric filled-gauge-values)))
                       ;; COUNTERS
                       (t/is (empty?
                              (m/stored-value->all-metric-samples example-counter-metric empty-counter-values)))
                       (t/is (= [(metric-samples/make-metric-sample (metric-types/counter-metric-name example-counter-metric)
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values  0))
                                                                    (m/metric-value-last-update-time-ms (nth values  0)))
                                 (metric-samples/make-metric-sample (metric-types/counter-metric-name example-counter-metric)
                                                                    (nth labelss 1)
                                                                    (m/metric-value-value               (nth values  1))
                                                                    (m/metric-value-last-update-time-ms (nth values  1)))
                                 (metric-samples/make-metric-sample (metric-types/counter-metric-name example-counter-metric)
                                                                    (nth labelss 2)
                                                                    (m/metric-value-value               (nth values  2))
                                                                    (m/metric-value-last-update-time-ms (nth values  2)))]
                                (m/stored-value->all-metric-samples example-counter-metric filled-counter-values)))
                       ;; HISTOGRAMS
                       (t/is (empty?
                              (m/stored-value->all-metric-samples example-histogram-metric empty-histogram-values)))

                       (t/is (= [(metric-samples/make-metric-sample (str basename "_sum")
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values 0))
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_count")
                                                                    (nth labelss 0)
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 0) :le "+Inf")
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 0) :le (str threshold))
                                                                    (if (<= (m/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                    (m/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str basename "_sum")
                                                                    (nth labelss 1)
                                                                    (m/metric-value-value               (nth values 1))
                                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                                 (metric-samples/make-metric-sample (str basename "_count")
                                                                    (nth labelss 1)
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 1) :le "+Inf")
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 1) :le (str threshold))
                                                                    (if (<= (m/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                    (m/metric-value-last-update-time-ms (nth values 1)))
                                 (metric-samples/make-metric-sample (str basename "_sum")
                                                                    (nth labelss 2)
                                                                    (m/metric-value-value               (nth values 2))
                                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                                 (metric-samples/make-metric-sample (str basename "_count")
                                                                    (nth labelss 2)
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 2) :le "+Inf")
                                                                    1.0
                                                                    (m/metric-value-last-update-time-ms (nth values 2)))
                                 (metric-samples/make-metric-sample (str basename "_bucket")
                                                                    (assoc (nth labelss 2) :le (str threshold))
                                                                    (if (<= (m/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                    (m/metric-value-last-update-time-ms (nth values 2)))]
                                (m/stored-value->all-metric-samples example-histogram-metric filled-histogram-values)))))))))
