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

(defn gen-filled-gauge-values
  [gauge-values labelss values]
  (if (empty? labelss)
    gauge-values
    (let [[labels & rest-labelss] labelss
          [value & rest-values] values]
      (gen-filled-gauge-values
       (m/update-gauge-values gauge-values labels value)
       rest-labelss
       rest-values))))

(defn gen-filled-counter-values
  [counter-values labelss values]
  (if (empty? labelss)
    counter-values
    (let [[labels & rest-labelss] labelss
          [value & rest-values] values]
      (gen-filled-counter-values
       (m/update-counter-values counter-values labels value)
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

(t/deftest t-make-gauge-values
  (t/testing "All fields of a metric-value are set correct."
    (let [example-gauge-values (m/make-gauge-values)]
      (t/is                       (m/gauge-values?    example-gauge-values))
      (t/is (= m/empty-values-map (m/gauge-values-map example-gauge-values))))))

(t/deftest t-update-gauge-values
  (t/testing "Updating gauge-values works."
    (t/is (quickcheck
           (property [labelss             (spec (util/gen-distinct-metric-labels 3))
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
           (property [name (spec ::metric-types/metric-name)
                      [label-x & labelss] (spec (util/gen-distinct-metric-labels 4))
                      values              (spec (gen-metric-values          3))]
                     (let [empty-gauge-values (m/make-gauge-values)
                           filled-gauge-values (gen-filled-gauge-values empty-gauge-values labelss values)]
                       ;; empty gauge-values-map
                       (t/is (= [] (m/gauge-values->metric-samples name empty-gauge-values label-x)))
                       ;; labels not in gauge-values-map
                       (t/is (= [] (m/gauge-values->metric-samples name filled-gauge-values label-x)))
                       (t/is (= [(metric-samples/make-metric-sample name
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values  0))
                                                                    (m/metric-value-last-update-time-ms (nth values  0)))]
                                (m/gauge-values->metric-samples name filled-gauge-values (nth labelss 0))))))))))

(t/deftest t-prune-stale-gauge-values
  (t/testing "Pruning stale gauge-values works."
    (t/is (quickcheck
           (property [labelss (spec (util/gen-distinct-metric-labels 6))]
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
           (property [labelss (spec (util/gen-distinct-metric-labels 6))
                      values  (spec (gen-metric-values          6))]
                     (let [empty-gauge-values (m/make-gauge-values)
                           filled-gauge-values (gen-filled-gauge-values empty-gauge-values labelss values)]
                       (t/is (= true  (m/empty-gauge-values? empty-gauge-values)))
                       (t/is (= false (m/empty-gauge-values? filled-gauge-values)))))))))

(t/deftest t-make-counter-values
  (t/testing "All fields of a metric-value are set correct."
    (let [example-counter-values (m/make-counter-values)]
      (t/is                       (m/counter-values?    example-counter-values))
      (t/is (= m/empty-values-map (m/counter-values-map example-counter-values))))))

(t/deftest t-update-counter-values
  (t/testing "Updating counter-values works."
    (t/is (quickcheck
           (property [labelss             (spec (util/gen-distinct-metric-labels 3))
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
           (property [name (spec ::metric-types/metric-name)
                      [label-x & labelss] (spec (util/gen-distinct-metric-labels 4))
                      values              (spec (gen-metric-values          3))]
                     (let [empty-counter-values (m/make-counter-values)
                           filled-counter-values (gen-filled-counter-values empty-counter-values labelss values)]
                       ;; empty counter-values-map
                       (t/is (empty?
                              (m/counter-values->metric-samples name empty-counter-values label-x)))
                       ;; labels not in counter-values-map
                       (t/is (empty?
                              (m/counter-values->metric-samples name filled-counter-values label-x)))
                       (t/is (= [(metric-samples/make-metric-sample name
                                                                    (nth labelss 0)
                                                                    (m/metric-value-value               (nth values  0))
                                                                    (m/metric-value-last-update-time-ms (nth values  0)))]
                                (m/counter-values->metric-samples name filled-counter-values (nth labelss 0))))))))))

(t/deftest t-prune-stale-counter-values
  (t/testing "Pruning stale counter-values works."
    (t/is (quickcheck
           (property [labelss (spec (util/gen-distinct-metric-labels 6))]
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
           (property [labelss (spec (util/gen-distinct-metric-labels 6))
                      values  (spec (gen-metric-values          6))]
                     (let [empty-counter-values (m/make-counter-values)
                           filled-counter-values (gen-filled-counter-values empty-counter-values labelss values)]
                       (t/is (= true  (m/empty-counter-values? empty-counter-values)))
                       (t/is (= false (m/empty-counter-values? filled-counter-values)))))))))

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
  (t/testing "Checking whether histogram-values-map (sum-map) is empty works."
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
(t/deftest t-update-stored-values
  (t/testing "Updating stored values works."
    (t/is (quickcheck
           (property [[labels-x & labelss] (spec (util/gen-distinct-metric-labels 7))
                      [value-x  & values] (spec (gen-metric-values          7))
                      threshold            (spec ::metric-types/threshold)]
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
           (property [example-gauge-metric     (spec ::metric-types/gauge-metric)
                      example-counter-metric   (spec ::metric-types/counter-metric)
                      example-histogram-metric (spec ::metric-types/histogram-metric)
                      labels                   (spec ::metric-types/metric-labels)
                      value                    (spec ::m/metric-value)]
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
           (property [threshold (spec ::metric-types/threshold)
                      labelss   (spec (util/gen-distinct-metric-labels 6))
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
                     (let [empty-gauge-values      (m/make-gauge-values)
                           filled-gauge-values     (gen-filled-gauge-values empty-gauge-values labelss values)
                           empty-counter-values    (m/make-counter-values)
                           filled-counter-values   (gen-filled-counter-values empty-counter-values labelss values)
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
                     (let [empty-gauge-values      (m/make-gauge-values)
                           filled-gauge-values     (gen-filled-gauge-values empty-gauge-values labelss values)
                           empty-counter-values    (m/make-counter-values)
                           filled-counter-values   (gen-filled-counter-values empty-counter-values labelss values)
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
