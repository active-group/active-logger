(ns active.clojure.logger.metric-accumulator-test
  (:require [active.clojure.logger.metric-accumulator :as m]
            [active.clojure.logger.metric-store :as metric-store]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-samples :as metric-samples]

            [active.clojure.record :as r]
            
            [clojure.test :as t]
            [clojure.math :as math]

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

(t/deftest t-record-metric
  (t/testing "record-metric! records something and mutates the store."
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labels (spec ::metric-types/metric-labels)
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [store (m/fresh-metric-store)
                           before (m/get-all-metric-sample-sets! store)]
                       (m/record-metric! store metric labels value time-ms)
                       (let [after (m/get-all-metric-sample-sets! store)]
                         (not= before after))))))))

(t/deftest t-get-metric-samples
  (t/testing "get-metric-samples! returns something for recorded metrics and labels"
    #_(let [metric (metric-types/make-counter-metric "" "")
            labels {}
            value 42.0
            time-ms 0])
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labels (spec ::metric-types/metric-labels)
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [store (m/fresh-metric-store)
                           before (m/get-metric-samples! store metric labels)]
                       (m/record-metric! store metric labels value time-ms)
                       (let [after (m/get-metric-samples! store metric labels)]
                         (and (empty? before)
                              (not= before after)))))))))

(t/deftest t-get-all-metric-sample-sets
  (t/testing "get-all-metric-sample-sets! returns something for recorded metrics and labels"
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labels (spec ::metric-types/metric-labels)
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [store (m/fresh-metric-store)
                           before (m/get-all-metric-sample-sets! store)]
                       (m/record-metric! store metric labels value time-ms)
                       (let [after (m/get-all-metric-sample-sets! store)]
                         (not= before after))))))))

(t/deftest t-prune-stale-metrics
  (t/testing "prune-stale-metrics! prunes stale metrics from the store"
    (t/is (quickcheck
           (property [metric (spec ::metric-types/metric)
                      labelss (spec (s/coll-of ::metric-types/metric-labels :distinct true :count 2))
                      value (spec ::metric-types/metric-value)
                      time-ms (spec ::metric-types/metric-last-update-time-ms)]
                     (let [[labels1 labels2] labelss
                           store (m/fresh-metric-store)
                           other (m/fresh-metric-store)]
                       (m/record-metric! store metric labels1 value time-ms)
                       (m/record-metric! store metric labels2 value (inc time-ms))
                       (m/prune-stale-metrics! store (inc time-ms))

                       (m/record-metric! other metric labels2 value (inc time-ms))

                       (= (m/get-all-metric-sample-sets! store)
                          (m/get-all-metric-sample-sets! other))))))))

;; -----------------------------------------------

(defn- concurrently! [values f]
  (doall (pmap f values)))

(def random-ints (repeatedly (comp int #(* 1000 %) math/random)))

(t/deftest t-concurrency-test
  ;; Note: for gauge metrics, there are no guarantees, other than 'the stored value corresponds to one of recorded values'

  (t/testing "For counter metrics, all values are counted"
    ;; Note: because of small differences in floating point ops depending on the order; we use small random integers; which should reduce the problem.
    (let [store (m/fresh-metric-store)
          counter-metric (metric-types/make-counter-metric "foo" "")
          labels {:bar "baz"}
          time-ms 12
          values (take 10000 random-ints)]
      (concurrently! values
                     #(m/record-metric! store counter-metric labels % time-ms))
      (let [other (m/fresh-metric-store)]
        (m/record-metric! other counter-metric labels (reduce + values) time-ms)

        ;; recording many concurrently, is (approximately) the same as recording the sum once.
        (t/is (= (m/get-metric-samples! other counter-metric labels)
                 (m/get-metric-samples! store counter-metric labels))))))

  (t/testing "For histogram metrics, the final value has the correct count and sum"
    (let [store (m/fresh-metric-store)
          histogram-metric (metric-types/make-histogram-metric "foo" "" [0 100])
          labels {:bar "baz"}
          time-ms 12
          values (take 10000 random-ints)]
      (concurrently! values
                     #(m/record-metric! store histogram-metric labels % time-ms))
      (let [samples (m/get-metric-samples! store histogram-metric labels)]
        (t/is (= (double (count values))
                 (metric-samples/metric-sample-value (first (filter #(= "foo_count" (metric-samples/metric-sample-name %))
                                                                    samples)))))
        (t/is (= (double (reduce + values))
                 (metric-samples/metric-sample-value (first (filter #(= "foo_sum" (metric-samples/metric-sample-name %))
                                                                    samples))))))))
  )

;; -----------------------------------------------

(t/deftest t-use-counter-like-gauge
  (let [metric-name "http_request_total"
        counter-metric (metric-types/make-counter-metric metric-name "HELP" true)]
    (m/reset-global-metric-store!)
    (m/record-metric! counter-metric {} 23 0)
    (m/record-metric! counter-metric {} 42 1)
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

    (m/record-metric! counter-metric {} Double/MAX_VALUE 0)

    (t/is (= [(metric-samples/make-metric-sample-set "counter-limit" :counter "HELP"
                                                     [(metric-samples/make-metric-sample "counter-limit" {} Double/MAX_VALUE  0)])]
             (m/get-all-metric-sample-sets!)))

    (m/record-metric! counter-metric {} 2 1)

    (t/is (= [(metric-samples/make-metric-sample-set "counter-limit" :counter "HELP"
                                                     [(metric-samples/make-metric-sample "counter-limit" {} Double/MAX_VALUE 1)])]
             (m/get-all-metric-sample-sets!)))))

;; we are storing doubles in MetricValue
(t/deftest t-histogram-dealing-with-big-ints
  (let [histogram-metric (metric-types/make-histogram-metric "name" "help" [5])
        metric-store     (m/fresh-metric-store)]

    (m/record-metric! metric-store histogram-metric {} 999999999999999999999999 0)
    (m/record-metric! metric-store histogram-metric {} 999999999999999999999999 0)

    ;; no exception raised
    (t/is true)))

;; we are storing doubles in MetricValue
(t/deftest t-histogram-dealing-with-longs
  (let [histogram-metric (metric-types/make-histogram-metric "name" "help" [5])
        metric-store     (m/fresh-metric-store)]

    (m/record-metric! metric-store histogram-metric {} 8999999999999999991 0)
    (m/record-metric! metric-store histogram-metric {}  999999999999999999 0)

    ;; no exception raised
    (t/is true)))
