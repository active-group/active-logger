(ns active.clojure.logger.metric-store-test
  (:require [active.clojure.logger.metric-store :as m]
            ;; [active.clojure.logger.metric-values :as metric-values]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-types :as metric-types]
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

(t/deftest t-fresh-metric-store-map
  (t/testing "Creating a fresh-metric-store-map works."
    (let [example-fresh-metric-store-map (m/fresh-metric-store-map)]
      (t/is (map?   example-fresh-metric-store-map))
      (t/is (empty? example-fresh-metric-store-map)))))

(defn record-metric* [store metric labels value]
  (m/record-metric store metric labels (util/metric-value-value value) (util/metric-value-last-update-time-ms value)))

(defn gen-filled-metric-store-map
  [metric-store metric labelss values]
  (if (empty? labelss)
    metric-store
    (let [[labels & rest-labelss] labelss
          [value  & rest-values] values]
      (gen-filled-metric-store-map
       (record-metric* metric-store metric labels value)
       metric
       rest-labelss
       rest-values))))

;; TODO: More elaborated metric-stores
(t/deftest t-record-metric-get-metric-samples
  (t/testing "`record-metric` and `get-metric-samples` work."
    (t/is (quickcheck
           (property [names               (spec (util/gen-distinct-metric-names  3))
                      helps               (spec (util/gen-metric-helps           3))
                      threshold           (spec ::metric-types/threshold)
                      [label-x & labelss] (spec (util/gen-distinct-metric-labels 7))
                      [value-x & values] (spec (util/gen-metric-values          7))]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           metric-store (m/fresh-metric-store-map)]
                       ;; Gauge
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples metric-store example-gauge-metric label-x)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples
                                    (record-metric* metric-store example-gauge-metric (nth labelss 0) (nth values 0))
                                    example-gauge-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                    (nth labelss 0)
                                                                    (util/metric-value-value               (nth values 0))
                                                                    (util/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples
                                 (record-metric* metric-store example-gauge-metric (nth labelss 0) (nth values 0))
                                 example-gauge-metric (nth labelss 0))))
                       ;; labels are within this metric - map is larger
                       (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                    (nth labelss 3)
                                                                    (util/metric-value-value               (nth values 3))
                                                                    (util/metric-value-last-update-time-ms (nth values 3)))]
                                (-> metric-store
                                    (record-metric* example-gauge-metric (nth labelss 0) (nth values 0))
                                    (record-metric* example-gauge-metric (nth labelss 1) (nth values 1))
                                    (record-metric* example-gauge-metric (nth labelss 2) (nth values 2))
                                    (record-metric* example-gauge-metric (nth labelss 3) (nth values 3))
                                    (record-metric* example-gauge-metric (nth labelss 4) (nth values 4))
                                    (record-metric* example-gauge-metric (nth labelss 5) (nth values 5))
                                    (m/get-metric-samples example-gauge-metric (nth labelss 3)))))

                       ;; Counter
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples metric-store example-counter-metric label-x)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples
                                    (record-metric* metric-store example-counter-metric (nth labelss 0) (nth values 0))
                                    example-counter-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                    (nth labelss 0)
                                                                    (util/metric-value-value               (nth values 0))
                                                                    (util/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples
                                 (record-metric* metric-store example-counter-metric (nth labelss 0) (nth values 0))
                                 example-counter-metric (nth labelss 0))))
                       ;; labels are within this metric - map is larger
                       (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                    (nth labelss 3)
                                                                    (util/metric-value-value               (nth values 3))
                                                                    (util/metric-value-last-update-time-ms (nth values 3)))]
                                (-> metric-store
                                    (record-metric* example-counter-metric (nth labelss 0) (nth values 0))
                                    (record-metric* example-counter-metric (nth labelss 1) (nth values 1))
                                    (record-metric* example-counter-metric (nth labelss 2) (nth values 2))
                                    (record-metric* example-counter-metric (nth labelss 3) (nth values 3))
                                    (record-metric* example-counter-metric (nth labelss 4) (nth values 4))
                                    (record-metric* example-counter-metric (nth labelss 5) (nth values 5))
                                    (m/get-metric-samples example-counter-metric (nth labelss 3)))))

                       ;; Histogram
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples metric-store example-histogram-metric label-x)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples
                                    (record-metric* metric-store example-histogram-metric (nth labelss 0) (nth values 0))
                                    example-histogram-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                    (nth labelss 0)
                                                                    (util/metric-value-value               (nth values 0))
                                                                    (util/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                    (nth labelss 0)
                                                                    1.0
                                                                    (util/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                    (assoc (nth labelss 0) :le "+Inf")
                                                                    1.0
                                                                    (util/metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                    (assoc (nth labelss 0) :le (str threshold))
                                                                    (if (<= (util/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                    (util/metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples
                                 (record-metric* metric-store example-histogram-metric (nth labelss 0) (nth values 0))
                                 example-histogram-metric (nth labelss 0))))
                       ;; labels are within this metric - map is larger
                       (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                    (nth labelss 3)
                                                                    (util/metric-value-value               (nth values 3))
                                                                    (util/metric-value-last-update-time-ms (nth values 3)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                    (nth labelss 3)
                                                                    1.0
                                                                    (util/metric-value-last-update-time-ms (nth values 3)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                    (assoc (nth labelss 3) :le "+Inf")
                                                                    1.0
                                                                    (util/metric-value-last-update-time-ms (nth values 3)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                    (assoc (nth labelss 3) :le (str threshold))
                                                                    (if (<= (util/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                    (util/metric-value-last-update-time-ms (nth values 3)))]
                                (-> metric-store
                                    (record-metric*  example-histogram-metric (nth labelss 0) (nth values 0))
                                    (record-metric* example-histogram-metric (nth labelss 1) (nth values 1))
                                    (record-metric* example-histogram-metric (nth labelss 2) (nth values 2))
                                    (record-metric* example-histogram-metric (nth labelss 3) (nth values 3))
                                    (record-metric* example-histogram-metric (nth labelss 4) (nth values 4))
                                    (record-metric* example-histogram-metric (nth labelss 5) (nth values 5))
                                    (m/get-metric-samples example-histogram-metric (nth labelss 3)))
                                ))))))))


;; TODO: more elaborated metric-store (mixed-metric-store)
(t/deftest t-get-metric-sample-set
  (t/testing "Getting a metric sample set for a metric from a metric store
  works."
    (t/is (quickcheck
           (property [names     (spec (util/gen-distinct-metric-names  3))
                      helps     (spec (util/gen-metric-helps           3))
                      threshold (spec ::metric-types/threshold)
                      labelss   (spec (util/gen-distinct-metric-labels 4))
                      values    (spec (util/gen-metric-values          4))]
                     (let [example-gauge-metric          (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric        (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric      (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
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
                       (t/is (= [] (m/get-metric-sample-set empty-metric-store example-gauge-metric)))
                       (t/is (= (metric-samples/make-metric-sample-set (nth names 0)
                                                                       :gauge
                                                                       (nth helps 0)
                                                                       [(metric-samples/make-metric-sample (nth names 0)
                                                                                                           (nth labelss 0)
                                                                                                           (util/metric-value-value               (nth values 0))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 0)))
                                                                        (metric-samples/make-metric-sample (nth names 0)
                                                                                                           (nth labelss 1)
                                                                                                           (util/metric-value-value               (nth values 1))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 1)))
                                                                        (metric-samples/make-metric-sample (nth names 0)
                                                                                                           (nth labelss 2)
                                                                                                           (util/metric-value-value               (nth values 2))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 2)))
                                                                        (metric-samples/make-metric-sample (nth names 0)
                                                                                                           (nth labelss 3)
                                                                                                           (util/metric-value-value               (nth values 3))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 3)))])
                                (m/get-metric-sample-set filled-gauge-metric-store example-gauge-metric)))
                       ;; COUNTERS
                       (t/is (= [] (m/get-metric-sample-set empty-metric-store example-counter-metric)))
                       (t/is (= (metric-samples/make-metric-sample-set (nth names 1)
                                                                       :counter
                                                                       (nth helps 1)
                                                                       [(metric-samples/make-metric-sample (nth names 1)
                                                                                                           (nth labelss 0)
                                                                                                           (util/metric-value-value               (nth values 0))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 0)))
                                                                        (metric-samples/make-metric-sample (nth names 1)
                                                                                                           (nth labelss 1)
                                                                                                           (util/metric-value-value               (nth values 1))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 1)))
                                                                        (metric-samples/make-metric-sample (nth names 1)
                                                                                                           (nth labelss 2)
                                                                                                           (util/metric-value-value               (nth values 2))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 2)))
                                                                        (metric-samples/make-metric-sample (nth names 1)
                                                                                                           (nth labelss 3)
                                                                                                           (util/metric-value-value               (nth values 3))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 3)))])
                                (m/get-metric-sample-set filled-counter-metric-store example-counter-metric)))
                       ;; HISTOGRAMS
                       (t/is (= [] (m/get-metric-sample-set empty-metric-store example-histogram-metric)))
                       (t/is (= (metric-samples/make-metric-sample-set basename
                                                                       :histogram
                                                                       (nth helps 2)
                                                                       [(metric-samples/make-metric-sample (str basename "_sum")
                                                                                                           (nth labelss 0)
                                                                                                           (util/metric-value-value               (nth values 0))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 0)))
                                                                        (metric-samples/make-metric-sample (str basename "_count")
                                                                                                           (nth labelss 0)
                                                                                                           1.0
                                                                                                           (util/metric-value-last-update-time-ms (nth values 0)))
                                                                        (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                           (assoc (nth labelss 0) :le "+Inf")
                                                                                                           1.0
                                                                                                           (util/metric-value-last-update-time-ms (nth values 0)))
                                                                        (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                           (assoc (nth labelss 0) :le (str threshold))
                                                                                                           (if (<= (util/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                                                           (util/metric-value-last-update-time-ms (nth values 0)))
                                                                        (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                           (nth labelss 1)
                                                                                                           (util/metric-value-value               (nth values 1))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 1)))
                                                                        (metric-samples/make-metric-sample (str basename "_count")
                                                                                                           (nth labelss 1)
                                                                                                           1.0
                                                                                                           (util/metric-value-last-update-time-ms (nth values 1)))
                                                                        (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                           (assoc (nth labelss 1) :le "+Inf")
                                                                                                           1.0
                                                                                                           (util/metric-value-last-update-time-ms (nth values 1)))
                                                                        (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                           (assoc (nth labelss 1) :le (str threshold))
                                                                                                           (if (<= (util/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                                                           (util/metric-value-last-update-time-ms (nth values 1)))
                                                                        (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                           (nth labelss 2)
                                                                                                           (util/metric-value-value               (nth values 2))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 2)))
                                                                        (metric-samples/make-metric-sample (str basename "_count")
                                                                                                           (nth labelss 2)
                                                                                                           1.0
                                                                                                           (util/metric-value-last-update-time-ms (nth values 2)))
                                                                        (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                           (assoc (nth labelss 2) :le "+Inf")
                                                                                                           1.0
                                                                                                           (util/metric-value-last-update-time-ms (nth values 2)))
                                                                        (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                           (assoc (nth labelss 2) :le (str threshold))
                                                                                                           (if (<= (util/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                                                           (util/metric-value-last-update-time-ms (nth values 2)))
                                                                        (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                           (nth labelss 3)
                                                                                                           (util/metric-value-value               (nth values 3))
                                                                                                           (util/metric-value-last-update-time-ms (nth values 3)))
                                                                        (metric-samples/make-metric-sample (str basename "_count")
                                                                                                           (nth labelss 3)
                                                                                                           1.0
                                                                                                           (util/metric-value-last-update-time-ms (nth values 3)))
                                                                        (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                           (assoc (nth labelss 3) :le "+Inf")
                                                                                                           1.0
                                                                                                           (util/metric-value-last-update-time-ms (nth values 3)))
                                                                        (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                           (assoc (nth labelss 3) :le (str threshold))
                                                                                                           (if (<= (util/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                                                           (util/metric-value-last-update-time-ms (nth values 3)))])
                                (m/get-metric-sample-set filled-histogram-metric-store example-histogram-metric)))))))))

(t/deftest t-get-all-metric-sample-sets
  (t/testing "Getting all metric sample sets from a metric store works."
    (t/is (quickcheck
           (property [names     (spec (util/gen-distinct-metric-names  3))
                      helps     (spec (util/gen-metric-helps           3))
                      threshold (spec ::metric-types/threshold)
                      labelss   (spec (util/gen-distinct-metric-labels 4))
                      values    (spec (util/gen-metric-values          4))]
                     (let [example-gauge-metric          (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric        (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric      (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
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
                       (t/is (= [] (m/get-all-metric-sample-sets empty-metric-store)))
                       (t/is (= [(metric-samples/make-metric-sample-set (nth names 0)
                                                                        :gauge
                                                                        (nth helps 0)
                                                                        [(metric-samples/make-metric-sample (nth names 0)
                                                                                                            (nth labelss 0)
                                                                                                            (util/metric-value-value               (nth values 0))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (nth names 0)
                                                                                                            (nth labelss 1)
                                                                                                            (util/metric-value-value               (nth values 1))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (nth names 0)
                                                                                                            (nth labelss 2)
                                                                                                            (util/metric-value-value               (nth values 2))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (nth names 0)
                                                                                                            (nth labelss 3)
                                                                                                            (util/metric-value-value               (nth values 3))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 3)))])
                                 (metric-samples/make-metric-sample-set (nth names 1)
                                                                        :counter
                                                                        (nth helps 1)
                                                                        [(metric-samples/make-metric-sample (nth names 1)
                                                                                                            (nth labelss 0)
                                                                                                            (util/metric-value-value               (nth values 0))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (nth names 1)
                                                                                                            (nth labelss 1)
                                                                                                            (util/metric-value-value               (nth values 1))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (nth names 1)
                                                                                                            (nth labelss 2)
                                                                                                            (util/metric-value-value               (nth values 2))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (nth names 1)
                                                                                                            (nth labelss 3)
                                                                                                            (util/metric-value-value               (nth values 3))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 3)))])
                                 (metric-samples/make-metric-sample-set basename
                                                                        :histogram
                                                                        (nth helps 2)
                                                                        [(metric-samples/make-metric-sample (str basename "_sum")
                                                                                                            (nth labelss 0)
                                                                                                            (util/metric-value-value               (nth values 0))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (str basename "_count")
                                                                                                            (nth labelss 0)
                                                                                                            1.0
                                                                                                            (util/metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 0) :le "+Inf")
                                                                                                            1.0
                                                                                                            (util/metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 0) :le (str threshold))
                                                                                                            (if (<= (util/metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                                                            (util/metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                            (nth labelss 1)
                                                                                                            (util/metric-value-value               (nth values 1))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (str basename "_count")
                                                                                                            (nth labelss 1)
                                                                                                            1.0
                                                                                                            (util/metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 1) :le "+Inf")
                                                                                                            1.0
                                                                                                            (util/metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 1) :le (str threshold))
                                                                                                            (if (<= (util/metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                                                            (util/metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                            (nth labelss 2)
                                                                                                            (util/metric-value-value               (nth values 2))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (str basename "_count")
                                                                                                            (nth labelss 2)
                                                                                                            1.0
                                                                                                            (util/metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 2) :le "+Inf")
                                                                                                            1.0
                                                                                                            (util/metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 2) :le (str threshold))
                                                                                                            (if (<= (util/metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                                                            (util/metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                            (nth labelss 3)
                                                                                                            (util/metric-value-value               (nth values 3))
                                                                                                            (util/metric-value-last-update-time-ms (nth values 3)))
                                                                         (metric-samples/make-metric-sample (str basename "_count")
                                                                                                            (nth labelss 3)
                                                                                                            1.0
                                                                                                            (util/metric-value-last-update-time-ms (nth values 3)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 3) :le "+Inf")
                                                                                                            1.0
                                                                                                            (util/metric-value-last-update-time-ms (nth values 3)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 3) :le (str threshold))
                                                                                                            (if (<= (util/metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                                                            (util/metric-value-last-update-time-ms (nth values 3)))])]
                                (m/get-all-metric-sample-sets filled-metric-store)))))))))
