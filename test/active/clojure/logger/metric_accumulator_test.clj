(ns active.clojure.logger.metric-accumulator-test
  (:require [active.clojure.logger.metric-accumulator :as m]
            [active.clojure.logger.metric-store :as metric-store]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-samples :as metric-samples]

            [active.clojure.record :as r]
            
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

#_(defn gen-filled-metric-store-maps
  [metric-store metric labelss values]
  (if (empty? labelss)
    metric-store
    (let [[labels & rest-labelss] labelss
          [value  & rest-values] values]
      (gen-filled-metric-store-map
       (metric-store/record-metric metric-store metric labels value)
       metric
       rest-labelss
       rest-values))))

(defn gen-distinct-metric-names
  [num-elems]
  (s/spec (s/coll-of ::metric-types/metric-name :distinct true :into [] :count num-elems)))

(defn gen-metric-helps
  [num-elems]
  (s/spec (s/coll-of ::metric-types/metric-help :distinct false :into [] :count num-elems)))

(defn gen-distinct-metric-labels
  [num-elems]
  (s/spec (s/coll-of ::metric-types/metric-labels :distinct true :into [] :count num-elems)))

(r/define-record-type ValueAndTime
  ;; similar to metric-values/MetricValue, but actually just a utility to write the tests.
  really-make-metric-value metric-value?
  [value metric-value-value
   last-update-time-ms metric-value-last-update-time-ms])

(defn make-metric-value [value time-ms]
  (really-make-metric-value (double value) time-ms))


(s/def ::value-and-time
  (s/spec
   (partial instance? ValueAndTime)
   :gen (fn []
          (sgen/fmap (fn [[metric-value-value-double metric-value-last-update-time-ms]]
                       (make-metric-value metric-value-value-double metric-value-last-update-time-ms))
                     (s/gen (s/tuple (s/and double? #(not (Double/isNaN %))) ::metric-types/metric-last-update-time-ms))))))

(defn gen-metric-values
  [num-elems]
  (s/spec (s/coll-of ::value-and-time :distinct true :into [] :count num-elems)))

;; -----------------------------------------------

(t/deftest t-fresh-metric-store
  (t/testing "Creating a fresh-metric-store works."
    (let [example-fresh-metric-store (m/fresh-metric-store)]
      (t/is (= (type example-fresh-metric-store) clojure.lang.Atom))
      (t/is (map?  @example-fresh-metric-store))
      (t/is (empty @example-fresh-metric-store)))))

;; set is deprecated; TODO: test reset alone
;; (t/deftest t-reset-and-set-global-metric-store!
;;   (t/testing "Setting and resetting a global-metric-store works."
;;     (t/is (quickcheck
;;            (property [names     (spec (gen-distinct-metric-names  3))
;;                       helps     (spec (gen-metric-helps           3))
;;                       threshold (spec ::gen/threshold)
;;                       labelss   (spec (gen-distinct-metric-labels 3))
;;                       values    (spec (gen-metric-values          3))]
;;                      (let [example-gauge-metric     (m/make-gauge-metric     (nth names 0) (nth helps 0))
;;                            example-counter-metric   (m/make-counter-metric   (nth names 1) (nth helps 1))
;;                            example-histogram-metric (m/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
;;                            example-gauge-values     (gen-filled-gauge-values     (m/make-gauge-values)               labelss values)
;;                            example-counter-values   (gen-filled-counter-values   (m/make-counter-values)             labelss values)
;;                            example-histogram-values (gen-filled-histogram-values (m/make-histogram-values [threshold]) labelss values)
;;                            example-metric-store-map {example-gauge-metric     example-gauge-values
;;                                                      example-counter-metric   example-counter-values
;;                                                      example-histogram-metric example-histogram-values}]
;;                        (m/reset-global-metric-store!)
;;                        (t/is (= {} @m/metric-store))
;;                        (m/set-global-metric-store! example-metric-store-map)
;;                        (t/is (= example-metric-store-map @m/metric-store))
;;                        (m/reset-global-metric-store!)
;;                        (t/is (= {} @m/metric-store))))))))


;; ------------------

(t/deftest t-record-metric!-get-metric-samples!
  (t/testing "`record-metric!` and `get-metric-samples!` work."
    (t/is (quickcheck
           (property [names               (spec (gen-distinct-metric-names  3))
                      helps               (spec (gen-metric-helps           3))
                      threshold           (spec ::metric-types/threshold)
                      [label-x & labelss] (spec (gen-distinct-metric-labels 7))
                      [value-x & values] (spec (gen-metric-values          7))]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           a-metric-store (m/fresh-metric-store)]

                       ;; GAUGES
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-gauge-metric label-x)))
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-gauge-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                    (nth labelss 0)
                                                                    (metric-value-value               (nth values 0))
                                                                    (metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples! a-metric-store example-gauge-metric (nth labelss 0))))

                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))
                       ;; update-time: nil
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 4)
                                         (metric-value-value               (nth values 4))
                                         nil)
                       (m/record-metric! a-metric-store
                                         example-gauge-metric
                                         (nth labelss 5)
                                         (metric-value-value               (nth values 5))
                                         (metric-value-last-update-time-ms (nth values 5)))

                       ;; labels are within this metric - map is larger
                       (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                    (nth labelss 3)
                                                                    (metric-value-value               (nth values 3))
                                                                    (metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples! a-metric-store example-gauge-metric (nth labelss 3))))

                       ;; update-time: nil
                       (let [example-metric-sample (nth (m/get-metric-samples! a-metric-store example-gauge-metric (nth labelss 4)) 0)]
                         (t/is (= (nth names   0) (metric-samples/metric-sample-name   example-metric-sample)))
                         (t/is (= (nth labelss 4) (metric-samples/metric-sample-labels example-metric-sample)))
                         (t/is (= (metric-value-value (nth values 4))
                                  (metric-samples/metric-sample-value example-metric-sample)))
                         (t/is (nat-int? (metric-samples/metric-sample-timestamp example-metric-sample))))

                       ;; COUNTERS
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-counter-metric label-x)))
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-counter-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                    (nth labelss 0)
                                                                    (metric-value-value               (nth values 0))
                                                                    (metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples! a-metric-store example-counter-metric (nth labelss 0))))

                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))
                       ;; update-time: nil
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 4)
                                         (metric-value-value               (nth values 4))
                                         nil)
                       (m/record-metric! a-metric-store
                                         example-counter-metric
                                         (nth labelss 5)
                                         (metric-value-value               (nth values 5))
                                         (metric-value-last-update-time-ms (nth values 5)))

                       ;; labels are within this metric - map is larger
                       (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                    (nth labelss 3)
                                                                    (metric-value-value               (nth values 3))
                                                                    (metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples! a-metric-store example-counter-metric (nth labelss 3))))
                       ;; update-time: nil
                       (let [example-metric-sample (nth (m/get-metric-samples! a-metric-store example-counter-metric (nth labelss 4)) 0)]
                         (t/is (= (nth names   1) (metric-samples/metric-sample-name   example-metric-sample)))
                         (t/is (= (nth labelss 4) (metric-samples/metric-sample-labels example-metric-sample)))
                         (t/is (= (metric-value-value (nth values 4))
                                  (metric-samples/metric-sample-value example-metric-sample)))
                         (t/is (nat-int? (metric-samples/metric-sample-timestamp example-metric-sample))))

                       ;; HISTOGRAMS
                       ;; metric is not in the store
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-histogram-metric label-x)))
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       ;; labels are not in this metric
                       (t/is (= [] (m/get-metric-samples! a-metric-store example-histogram-metric label-x)))
                       ;; labels are within this metric
                       (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                    (nth labelss 0)
                                                                    (metric-value-value               (nth values 0))
                                                                    (metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                    (nth labelss 0)
                                                                    1.0
                                                                    (metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                    (assoc (nth labelss 0) :le "+Inf")
                                                                    1.0
                                                                    (metric-value-last-update-time-ms (nth values 0)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                    (assoc (nth labelss 0) :le (str threshold))
                                                                    (if (<= (metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                    (metric-value-last-update-time-ms (nth values 0)))]
                                (m/get-metric-samples! a-metric-store example-histogram-metric (nth labelss 0))))
                       ;; labels are within this metric - map is larger
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))
                       ;; update-time: nil
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 4)
                                         (metric-value-value               (nth values 4))
                                         nil)
                       (m/record-metric! a-metric-store
                                         example-histogram-metric
                                         (nth labelss 5)
                                         (metric-value-value               (nth values 5))
                                         (metric-value-last-update-time-ms (nth values 5)))

                       (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                    (nth labelss 3)
                                                                    (metric-value-value               (nth values 3))
                                                                    (metric-value-last-update-time-ms (nth values 3)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                    (nth labelss 3)
                                                                    1.0
                                                                    (metric-value-last-update-time-ms (nth values 3)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                    (assoc (nth labelss 3) :le "+Inf")
                                                                    1.0
                                                                    (metric-value-last-update-time-ms (nth values 3)))
                                 (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                    (assoc (nth labelss 3) :le (str threshold))
                                                                    (if (<= (metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                    (metric-value-last-update-time-ms (nth values 3)))]
                                (m/get-metric-samples! a-metric-store example-histogram-metric (nth labelss 3))))))))))

;; t-record-metric!-get-metric-samples!-global-store
;; including timestamp nil and not given for record-metric!

(t/deftest t-get-all-metric-sample-sets!-global-store
  (t/testing "Getting all metric sample sets from the metric store (atom) works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::metric-types/threshold)
                      labelss   (spec (gen-distinct-metric-labels 4))
                      values    (spec (gen-metric-values          4))]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           basename (nth names 2)]
                       (m/reset-global-metric-store!)

                       (t/is (= [] (m/get-all-metric-sample-sets!)))

                       (m/record-metric! example-gauge-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-gauge-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! example-counter-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-counter-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! example-histogram-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! example-histogram-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))

                       (t/is (= [(metric-samples/make-metric-sample-set (nth names 0)
                                                                        :gauge
                                                                        (nth helps 0)
                                                                        [(metric-samples/make-metric-sample (nth names 0)
                                                                                                            (nth labelss 0)
                                                                                                            (metric-value-value               (nth values 0))
                                                                                                            (metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (nth names 0)
                                                                                                            (nth labelss 1)
                                                                                                            (metric-value-value               (nth values 1))
                                                                                                            (metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (nth names 0)
                                                                                                            (nth labelss 2)
                                                                                                            (metric-value-value               (nth values 2))
                                                                                                            (metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (nth names 0)
                                                                                                            (nth labelss 3)
                                                                                                            (metric-value-value               (nth values 3))
                                                                                                            (metric-value-last-update-time-ms (nth values 3)))])
                                 (metric-samples/make-metric-sample-set (nth names 1)
                                                                        :counter
                                                                        (nth helps 1)
                                                                        [(metric-samples/make-metric-sample (nth names 1)
                                                                                                            (nth labelss 0)
                                                                                                            (metric-value-value               (nth values 0))
                                                                                                            (metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (nth names 1)
                                                                                                            (nth labelss 1)
                                                                                                            (metric-value-value               (nth values 1))
                                                                                                            (metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (nth names 1)
                                                                                                            (nth labelss 2)
                                                                                                            (metric-value-value               (nth values 2))
                                                                                                            (metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (nth names 1)
                                                                                                            (nth labelss 3)
                                                                                                            (metric-value-value               (nth values 3))
                                                                                                            (metric-value-last-update-time-ms (nth values 3)))])
                                 (metric-samples/make-metric-sample-set basename
                                                                        :histogram
                                                                        (nth helps 2)
                                                                        [(metric-samples/make-metric-sample (str basename "_sum")
                                                                                                            (nth labelss 0)
                                                                                                            (metric-value-value               (nth values 0))
                                                                                                            (metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (str basename "_count")
                                                                                                            (nth labelss 0)
                                                                                                            1.0
                                                                                                            (metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 0) :le "+Inf")
                                                                                                            1.0
                                                                                                            (metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 0) :le (str threshold))
                                                                                                            (if (<= (metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                                                            (metric-value-last-update-time-ms (nth values 0)))
                                                                         (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                            (nth labelss 1)
                                                                                                            (metric-value-value               (nth values 1))
                                                                                                            (metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (str basename "_count")
                                                                                                            (nth labelss 1)
                                                                                                            1.0
                                                                                                            (metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 1) :le "+Inf")
                                                                                                            1.0
                                                                                                            (metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 1) :le (str threshold))
                                                                                                            (if (<= (metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                                                            (metric-value-last-update-time-ms (nth values 1)))
                                                                         (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                            (nth labelss 2)
                                                                                                            (metric-value-value               (nth values 2))
                                                                                                            (metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (str basename "_count")
                                                                                                            (nth labelss 2)
                                                                                                            1.0
                                                                                                            (metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 2) :le "+Inf")
                                                                                                            1.0
                                                                                                            (metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 2) :le (str threshold))
                                                                                                            (if (<= (metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                                                            (metric-value-last-update-time-ms (nth values 2)))
                                                                         (metric-samples/make-metric-sample (str basename "_sum")
                                                                                                            (nth labelss 3)
                                                                                                            (metric-value-value               (nth values 3))
                                                                                                            (metric-value-last-update-time-ms (nth values 3)))
                                                                         (metric-samples/make-metric-sample (str basename "_count")
                                                                                                            (nth labelss 3)
                                                                                                            1.0
                                                                                                            (metric-value-last-update-time-ms (nth values 3)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 3) :le "+Inf")
                                                                                                            1.0
                                                                                                            (metric-value-last-update-time-ms (nth values 3)))
                                                                         (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                                            (assoc (nth labelss 3) :le (str threshold))
                                                                                                            (if (<= (metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                                                            (metric-value-last-update-time-ms (nth values 3)))])]
                                (m/get-all-metric-sample-sets!)))))))))

(t/deftest t-get-all-metric-sample-sets!
  (t/testing "Getting all metric sample sets from the metric store (atom) works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::metric-types/threshold)
                      labelss   (spec (gen-distinct-metric-labels 4))
                      values    (spec (gen-metric-values          4))]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           basename (nth names 2)
                           metric-store (m/fresh-metric-store)]

                       (t/is (= [] (m/get-all-metric-sample-sets! metric-store)))

                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-gauge-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-counter-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))

                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 0)
                                         (metric-value-value               (nth values 0))
                                         (metric-value-last-update-time-ms (nth values 0)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 1)
                                         (metric-value-value               (nth values 1))
                                         (metric-value-last-update-time-ms (nth values 1)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 2)
                                         (metric-value-value               (nth values 2))
                                         (metric-value-last-update-time-ms (nth values 2)))
                       (m/record-metric! metric-store
                                         example-histogram-metric
                                         (nth labelss 3)
                                         (metric-value-value               (nth values 3))
                                         (metric-value-last-update-time-ms (nth values 3)))

                       (t/is (= [(metric-samples/make-metric-sample-set (nth names 0)
                                                           :gauge
                                                           (nth helps 0)
                                                           [(metric-samples/make-metric-sample (nth names 0)
                                                                                  (nth labelss 0)
                                                                                  (metric-value-value               (nth values 0))
                                                                                  (metric-value-last-update-time-ms (nth values 0)))
                                                            (metric-samples/make-metric-sample (nth names 0)
                                                                                  (nth labelss 1)
                                                                                  (metric-value-value               (nth values 1))
                                                                                  (metric-value-last-update-time-ms (nth values 1)))
                                                            (metric-samples/make-metric-sample (nth names 0)
                                                                                  (nth labelss 2)
                                                                                  (metric-value-value               (nth values 2))
                                                                                  (metric-value-last-update-time-ms (nth values 2)))
                                                            (metric-samples/make-metric-sample (nth names 0)
                                                                                  (nth labelss 3)
                                                                                  (metric-value-value               (nth values 3))
                                                                                  (metric-value-last-update-time-ms (nth values 3)))])
                                 (metric-samples/make-metric-sample-set (nth names 1)
                                                           :counter
                                                           (nth helps 1)
                                                           [(metric-samples/make-metric-sample (nth names 1)
                                                                                  (nth labelss 0)
                                                                                  (metric-value-value               (nth values 0))
                                                                                  (metric-value-last-update-time-ms (nth values 0)))
                                                            (metric-samples/make-metric-sample (nth names 1)
                                                                                  (nth labelss 1)
                                                                                  (metric-value-value               (nth values 1))
                                                                                  (metric-value-last-update-time-ms (nth values 1)))
                                                            (metric-samples/make-metric-sample (nth names 1)
                                                                                  (nth labelss 2)
                                                                                  (metric-value-value               (nth values 2))
                                                                                  (metric-value-last-update-time-ms (nth values 2)))
                                                            (metric-samples/make-metric-sample (nth names 1)
                                                                                  (nth labelss 3)
                                                                                  (metric-value-value               (nth values 3))
                                                                                  (metric-value-last-update-time-ms (nth values 3)))])
                                 (metric-samples/make-metric-sample-set basename
                                                           :histogram
                                                           (nth helps 2)
                                                           [(metric-samples/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 0)
                                                                                  (metric-value-value               (nth values 0))
                                                                                  (metric-value-last-update-time-ms (nth values 0)))
                                                            (metric-samples/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 0)
                                                                                  1.0
                                                                                  (metric-value-last-update-time-ms (nth values 0)))
                                                            (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 0) :le "+Inf")
                                                                                  1.0
                                                                                  (metric-value-last-update-time-ms (nth values 0)))
                                                            (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 0) :le (str threshold))
                                                                                  (if (<= (metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                                  (metric-value-last-update-time-ms (nth values 0)))
                                                            (metric-samples/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 1)
                                                                                  (metric-value-value               (nth values 1))
                                                                                  (metric-value-last-update-time-ms (nth values 1)))
                                                            (metric-samples/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 1)
                                                                                  1.0
                                                                                  (metric-value-last-update-time-ms (nth values 1)))
                                                            (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 1) :le "+Inf")
                                                                                  1.0
                                                                                  (metric-value-last-update-time-ms (nth values 1)))
                                                            (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 1) :le (str threshold))
                                                                                  (if (<= (metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                                  (metric-value-last-update-time-ms (nth values 1)))
                                                            (metric-samples/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 2)
                                                                                  (metric-value-value               (nth values 2))
                                                                                  (metric-value-last-update-time-ms (nth values 2)))
                                                            (metric-samples/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 2)
                                                                                  1.0
                                                                                  (metric-value-last-update-time-ms (nth values 2)))
                                                            (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 2) :le "+Inf")
                                                                                  1.0
                                                                                  (metric-value-last-update-time-ms (nth values 2)))
                                                            (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 2) :le (str threshold))
                                                                                  (if (<= (metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                                  (metric-value-last-update-time-ms (nth values 2)))
                                                            (metric-samples/make-metric-sample (str basename "_sum")
                                                                                  (nth labelss 3)
                                                                                  (metric-value-value               (nth values 3))
                                                                                  (metric-value-last-update-time-ms (nth values 3)))
                                                            (metric-samples/make-metric-sample (str basename "_count")
                                                                                  (nth labelss 3)
                                                                                  1.0
                                                                                  (metric-value-last-update-time-ms (nth values 3)))
                                                            (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 3) :le "+Inf")
                                                                                  1.0
                                                                                  (metric-value-last-update-time-ms (nth values 3)))
                                                            (metric-samples/make-metric-sample (str basename "_bucket")
                                                                                  (assoc (nth labelss 3) :le (str threshold))
                                                                                  (if (<= (metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                                  (metric-value-last-update-time-ms (nth values 3)))])]
                                (m/get-all-metric-sample-sets! metric-store)))))))))

;; >>> HELPER

(defn t-prune-stale-metrics!-prune-nothing
  "Metric sample set where nothing is pruned."
  [names helps labelss values basename threshold]
  [(metric-samples/make-metric-sample-set (nth names 0)
                                          :gauge
                                          (nth helps 0)
                                          [(metric-samples/make-metric-sample (nth names 0)
                                                                              (nth labelss 0)
                                                                              (metric-value-value               (nth values 0))
                                                                              (metric-value-last-update-time-ms (nth values 0)))
                                           (metric-samples/make-metric-sample (nth names 0)
                                                                              (nth labelss 1)
                                                                              (metric-value-value               (nth values 1))
                                                                              (metric-value-last-update-time-ms (nth values 1)))
                                           (metric-samples/make-metric-sample (nth names 0)
                                                                              (nth labelss 2)
                                                                              (metric-value-value               (nth values 2))
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (nth names 0)
                                                                              (nth labelss 3)
                                                                              (metric-value-value               (nth values 3))
                                                                              (metric-value-last-update-time-ms (nth values 3)))])
   (metric-samples/make-metric-sample-set (nth names 1)
                                          :counter
                                          (nth helps 1)
                                          [(metric-samples/make-metric-sample (nth names 1)
                                                                              (nth labelss 0)
                                                                              (metric-value-value               (nth values 0))
                                                                              (metric-value-last-update-time-ms (nth values 0)))
                                           (metric-samples/make-metric-sample (nth names 1)
                                                                              (nth labelss 1)
                                                                              (metric-value-value               (nth values 1))
                                                                              (metric-value-last-update-time-ms (nth values 1)))
                                           (metric-samples/make-metric-sample (nth names 1)
                                                                              (nth labelss 2)
                                                                              (metric-value-value               (nth values 2))
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (nth names 1)
                                                                              (nth labelss 3)
                                                                              (metric-value-value               (nth values 3))
                                                                              (metric-value-last-update-time-ms (nth values 3)))])
   (metric-samples/make-metric-sample-set basename
                                          :histogram
                                          (nth helps 2)
                                          [(metric-samples/make-metric-sample (str basename "_sum")
                                                                              (nth labelss 0)
                                                                              (metric-value-value               (nth values 0))
                                                                              (metric-value-last-update-time-ms (nth values 0)))
                                           (metric-samples/make-metric-sample (str basename "_count")
                                                                              (nth labelss 0)
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 0)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 0) :le "+Inf")
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 0)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 0) :le (str threshold))
                                                                              (if (<= (metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                              (metric-value-last-update-time-ms (nth values 0)))
                                           (metric-samples/make-metric-sample (str basename "_sum")
                                                                              (nth labelss 1)
                                                                              (metric-value-value               (nth values 1))
                                                                              (metric-value-last-update-time-ms (nth values 1)))
                                           (metric-samples/make-metric-sample (str basename "_count")
                                                                              (nth labelss 1)
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 1)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 1) :le "+Inf")
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 1)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 1) :le (str threshold))
                                                                              (if (<= (metric-value-value (nth values 1)) threshold) 1.0 0.0)
                                                                              (metric-value-last-update-time-ms (nth values 1)))
                                           (metric-samples/make-metric-sample (str basename "_sum")
                                                                              (nth labelss 2)
                                                                              (metric-value-value               (nth values 2))
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (str basename "_count")
                                                                              (nth labelss 2)
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 2) :le "+Inf")
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 2) :le (str threshold))
                                                                              (if (<= (metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (str basename "_sum")
                                                                              (nth labelss 3)
                                                                              (metric-value-value               (nth values 3))
                                                                              (metric-value-last-update-time-ms (nth values 3)))
                                           (metric-samples/make-metric-sample (str basename "_count")
                                                                              (nth labelss 3)
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 3)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 3) :le "+Inf")
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 3)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 3) :le (str threshold))
                                                                              (if (<= (metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                              (metric-value-last-update-time-ms (nth values 3)))])])

(defn t-prune-stale-metrics!-prune-some
  "Metric sample set where some were pruned."
  [names helps labelss values basename threshold]
  [(metric-samples/make-metric-sample-set (nth names 0)
                                          :gauge
                                          (nth helps 0)
                                          [(metric-samples/make-metric-sample (nth names 0)
                                                                              (nth labelss 2)
                                                                              (metric-value-value               (nth values 2))
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (nth names 0)
                                                                              (nth labelss 3)
                                                                              (metric-value-value               (nth values 3))
                                                                              (metric-value-last-update-time-ms (nth values 3)))])
   (metric-samples/make-metric-sample-set (nth names 1)
                                          :counter
                                          (nth helps 1)
                                          [(metric-samples/make-metric-sample (nth names 1)
                                                                              (nth labelss 2)
                                                                              (metric-value-value               (nth values 2))
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (nth names 1)
                                                                              (nth labelss 3)
                                                                              (metric-value-value               (nth values 3))
                                                                              (metric-value-last-update-time-ms (nth values 3)))])
   (metric-samples/make-metric-sample-set basename
                                          :histogram
                                          (nth helps 2)
                                          [(metric-samples/make-metric-sample (str basename "_sum")
                                                                              (nth labelss 2)
                                                                              (metric-value-value               (nth values 2))
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (str basename "_count")
                                                                              (nth labelss 2)
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 2) :le "+Inf")
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 2) :le (str threshold))
                                                                              (if (<= (metric-value-value (nth values 2)) threshold) 1.0 0.0)
                                                                              (metric-value-last-update-time-ms (nth values 2)))
                                           (metric-samples/make-metric-sample (str basename "_sum")
                                                                              (nth labelss 3)
                                                                              (metric-value-value               (nth values 3))
                                                                              (metric-value-last-update-time-ms (nth values 3)))
                                           (metric-samples/make-metric-sample (str basename "_count")
                                                                              (nth labelss 3)
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 3)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 3) :le "+Inf")
                                                                              1.0
                                                                              (metric-value-last-update-time-ms (nth values 3)))
                                           (metric-samples/make-metric-sample (str basename "_bucket")
                                                                              (assoc (nth labelss 3) :le (str threshold))
                                                                              (if (<= (metric-value-value (nth values 3)) threshold) 1.0 0.0)
                                                                              (metric-value-last-update-time-ms (nth values 3)))])])

;; <<< HELPER

(t/deftest t-prune-stale-metrics!-global-store
  (t/testing "Pruning all metrics in the global metric store works."
    (let [names ["n1" "n2" "n3"]
          helps ["h1" "h2" "h3"]
          threshold 42
          labelss [{} {:a "a"} {:a "a" :b "b"} {:b "b"}]
                           
          values                        [(make-metric-value 300 10)
                                         (make-metric-value 300 12)
                                         (make-metric-value 300 13)
                                         (make-metric-value 300 15)]
          example-gauge-metric          (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
          example-counter-metric        (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
          example-histogram-metric      (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
          basename (nth names 2)

          record-examples! (fn [metric]
                             (doseq [[label value] (map vector labelss values)]
                               (m/record-metric! metric label (metric-value-value value) (metric-value-last-update-time-ms value))))]

      (m/reset-global-metric-store!)
      (m/prune-stale-metrics! 1)
      (t/is (= [] (m/get-all-metric-sample-sets!)))

      (record-examples! example-gauge-metric)
      (record-examples! example-counter-metric)
      (record-examples! example-histogram-metric)

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
               (m/get-all-metric-sample-sets!))))))

(t/deftest t-prune-stale-metrics!
  (t/testing "Pruning all metrics in a metric store works."
    (let [names ["n1" "n2" "n3"]
          helps ["h1" "h2" "h3"]
          threshold 42
          labelss [{} {:a "a"} {:a "a" :b "b"} {:b "b"}]
          values                        [(make-metric-value 300 10)
                                         (make-metric-value 300 12)
                                         (make-metric-value 300 13)
                                         (make-metric-value 300 15)]
          example-gauge-metric          (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
          example-counter-metric        (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
          example-histogram-metric      (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
          basename (nth names 2)
          metric-store (m/fresh-metric-store)

          record-examples! (fn [metric]
                             (doseq [[label value] (map vector labelss values)]
                               (m/record-metric! metric-store metric label (metric-value-value value) (metric-value-last-update-time-ms value))))]

      (m/prune-stale-metrics! metric-store 1)
      (t/is (= [] (m/get-all-metric-sample-sets! metric-store)))

      (record-examples! example-gauge-metric)
      (record-examples! example-counter-metric)
      (record-examples! example-histogram-metric)
                       
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
               (m/get-all-metric-sample-sets! metric-store))))))

;; COMMANDS on raw metrics

(t/deftest t-record-and-get!-global-store
  (t/testing "Recording and getting the metric samples for the recorded metric
  with the given labels works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::metric-types/threshold)
                      labels    (spec ::metric-types/metric-labels)
                      values    (spec (gen-metric-values          2))]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])]

                       (m/reset-global-metric-store!)

                       ;; GAUGES
                       (let [example-record-and-get! (m/record-and-get! example-gauge-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 0))
                                                                        (metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                      labels
                                                                      (metric-value-value               (nth values 0))
                                                                      (metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))
                       (let [example-record-and-get! (m/record-and-get! example-gauge-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 1))
                                                                        (metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                      labels
                                                                      (metric-value-value               (nth values 1))
                                                                      (metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))

                       ;; COUNTERS
                       (let [example-record-and-get! (m/record-and-get! example-counter-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 0))
                                                                        (metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                      labels
                                                                      (metric-value-value               (nth values 0))
                                                                      (metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))
                       (let [example-record-and-get! (m/record-and-get! example-counter-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 1))
                                                                        (metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                      labels
                                                                      (+ (metric-value-value (nth values 0))
                                                                         (metric-value-value (nth values 1)))
                                                                      (metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))

                       ;; HISTOGRAMS
                       (let [example-record-and-get! (m/record-and-get! example-histogram-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 0))
                                                                        (metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                      labels
                                                                      (metric-value-value               (nth values 0))
                                                                      (metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                      labels
                                                                      1.0
                                                                      (metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le "+Inf")
                                                                      1.0
                                                                      (metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le (str threshold))
                                                                      (if (<= (metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                      (metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))

                       (let [example-record-and-get! (m/record-and-get! example-histogram-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 1))
                                                                        (metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                      labels
                                                                      (+ (metric-value-value (nth values 0))
                                                                         (metric-value-value (nth values 1)))
                                                                      (metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                      labels
                                                                      2.0
                                                                      (metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le "+Inf")
                                                                      2.0
                                                                      (metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le (str threshold))
                                                                      (+ (if (<= (metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                         (if (<= (metric-value-value (nth values 1)) threshold) 1.0 0.0))
                                                                      (metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))))))))

(t/deftest t-record-and-get!
  (t/testing "Recording and getting the metric samples for the recorded metric
  with the given labels works."
    (t/is (quickcheck
           (property [names     (spec (gen-distinct-metric-names  3))
                      helps     (spec (gen-metric-helps           3))
                      threshold (spec ::metric-types/threshold)
                      labels    (spec ::metric-types/metric-labels)
                      values    (spec (gen-metric-values          2))]
                     (let [example-gauge-metric     (metric-types/make-gauge-metric     (nth names 0) (nth helps 0))
                           example-counter-metric   (metric-types/make-counter-metric   (nth names 1) (nth helps 1))
                           example-histogram-metric (metric-types/make-histogram-metric (nth names 2) (nth helps 2) [threshold])
                           metric-store             (m/fresh-metric-store)]

                       ;; GAUGES
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-gauge-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 0))
                                                                        (metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                      labels
                                                                      (metric-value-value               (nth values 0))
                                                                      (metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-gauge-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 1))
                                                                        (metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 0)
                                                                      labels
                                                                      (metric-value-value               (nth values 1))
                                                                      (metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))

                       ;; COUNTERS
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-counter-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 0))
                                                                        (metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                      labels
                                                                      (metric-value-value               (nth values 0))
                                                                      (metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-counter-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 1))
                                                                        (metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(metric-samples/make-metric-sample (nth names 1)
                                                                      labels
                                                                      (+ (metric-value-value (nth values 0))
                                                                         (metric-value-value (nth values 1)))
                                                                      (metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))

                       ;; HISTOGRAMS
                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-histogram-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 0))
                                                                        (metric-value-last-update-time-ms (nth values 0)))]
                         (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                      labels
                                                                      (metric-value-value               (nth values 0))
                                                                      (metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                      labels
                                                                      1.0
                                                                      (metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le "+Inf")
                                                                      1.0
                                                                      (metric-value-last-update-time-ms (nth values 0)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le (str threshold))
                                                                      (if (<= (metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                      (metric-value-last-update-time-ms (nth values 0)))]
                                  example-record-and-get!)))

                       (let [example-record-and-get! (m/record-and-get! metric-store
                                                                        example-histogram-metric
                                                                        labels
                                                                        (metric-value-value               (nth values 1))
                                                                        (metric-value-last-update-time-ms (nth values 1)))]
                         (t/is (= [(metric-samples/make-metric-sample (str (nth names 2) "_sum")
                                                                      labels
                                                                      (+ (metric-value-value (nth values 0))
                                                                         (metric-value-value (nth values 1)))
                                                                      (metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_count")
                                                                      labels
                                                                      2.0
                                                                      (metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le "+Inf")
                                                                      2.0
                                                                      (metric-value-last-update-time-ms (nth values 1)))
                                   (metric-samples/make-metric-sample (str (nth names 2) "_bucket")
                                                                      (assoc labels :le (str threshold))
                                                                      (+ (if (<= (metric-value-value (nth values 0)) threshold) 1.0 0.0)
                                                                         (if (<= (metric-value-value (nth values 1)) threshold) 1.0 0.0))
                                                                      (metric-value-last-update-time-ms (nth values 1)))]
                                  example-record-and-get!)))))))))

(t/deftest t-histogram-with-many-buckets
  (let [metric-name "http_request_duration_seconds"
        histogram-metric (metric-types/make-histogram-metric metric-name "HELP" [0.1 0.2 0.3 0.45])
        recorded-metrics [0.25 0.35 0.25 0.2 0.15 0.5 0.5]
        expected-result [["_sum"     nil   (double (apply + recorded-metrics))]
                         ["_count"   nil   (double (count recorded-metrics))]
                         ["_bucket" "+Inf" (double (count recorded-metrics))]
                         ["_bucket" "0.1"  (double (count (filter #(<= % 0.1) recorded-metrics)))]
                         ["_bucket" "0.2"  (double (count (filter #(<= % 0.2) recorded-metrics)))]
                         ["_bucket" "0.3"  (double (count (filter #(<= % 0.3) recorded-metrics)))]
                         ["_bucket" "0.45" (double (count (filter #(<= % 0.45) recorded-metrics)))]]
        expected-sample-set
        (metric-samples/make-metric-sample-set metric-name :histogram "HELP"
                                               (mapv (fn [[postfix maybe-le value]]
                                                       (metric-samples/make-metric-sample (str metric-name postfix) (if maybe-le {:le maybe-le} {}) value 0))
                                                     expected-result))]
    (m/reset-global-metric-store!)
    (doseq [value recorded-metrics]
      (m/record-and-get! histogram-metric {} value 0))
    (t/is (= [expected-sample-set] (m/get-all-metric-sample-sets!)))
    (m/reset-global-metric-store!)))

(t/deftest t-use-counter-like-gauge
  (let [metric-name "http_request_total"
        counter-metric (metric-types/make-counter-metric metric-name "HELP" true)]
    (m/reset-global-metric-store!)
    (m/record-and-get! counter-metric {} 23 0)
    (m/record-and-get! counter-metric {} 42 1)
    (t/is (= [(metric-samples/make-metric-sample-set "http_request_total" :counter "HELP"
                                                     [(metric-samples/make-metric-sample "http_request_total" {} 42.0 1)])]
             (m/get-all-metric-sample-sets!)))
    (m/reset-global-metric-store!)))

;; prometheus seems to be okay for counters to jump from a high number back to 0
;; it looks like that we will stay at Double/MAX_VALUE or run/stay in #Inf
(t/deftest t-counter-dealing-with-max-value
  (let [metric-name "counter-limit"
        counter-metric (metric-types/make-counter-metric metric-name "HELP")]
    (m/reset-global-metric-store!)

    (m/record-and-get! counter-metric {} Double/MAX_VALUE 0)

    (t/is (= [(metric-samples/make-metric-sample-set "counter-limit" :counter "HELP"
                                                     [(metric-samples/make-metric-sample "counter-limit" {} Double/MAX_VALUE  0)])]
             (m/get-all-metric-sample-sets!)))

    (m/record-and-get! counter-metric {} 2 1)

    (t/is (= [(metric-samples/make-metric-sample-set "counter-limit" :counter "HELP"
                                                     [(metric-samples/make-metric-sample "counter-limit" {} Double/MAX_VALUE 1)])]
             (m/get-all-metric-sample-sets!)))))

;; we are storing doubles in MetricValue
(t/deftest t-histogram-dealing-with-big-ints
  (let [histogram-metric (metric-types/make-histogram-metric "name" "help" [5])
        metric-store     (m/fresh-metric-store)]

    (m/record-and-get! metric-store histogram-metric {} 999999999999999999999999 0)
    (m/record-and-get! metric-store histogram-metric {} 999999999999999999999999 0)

    ;; no exception raised
    (t/is true)))

;; we are storing doubles in MetricValue
(t/deftest t-histogram-dealing-with-longs
  (let [histogram-metric (metric-types/make-histogram-metric "name" "help" [5])
        metric-store     (m/fresh-metric-store)]

    (m/record-and-get! metric-store histogram-metric {} 8999999999999999991 0)
    (m/record-and-get! metric-store histogram-metric {}  999999999999999999 0)

    ;; no exception raised
    (t/is true)))
