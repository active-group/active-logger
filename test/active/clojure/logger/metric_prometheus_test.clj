(ns active.clojure.logger.metric-prometheus-test
  (:require [active.clojure.logger.metric-prometheus :as m]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-singular-value :as metric-singular-value]
            [active.clojure.logger.metric-histogram-value :as metric-histogram-value]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [clojure.test :as t]))

(t/deftest t-render-metric-sets
  (t/is (= ["# HELP name_with_blanks help"
            "# TYPE name_with_blanks counter"
            "name_with_blanks{label_with_dashes=\"a\"} 23.0"
            "# HELP name help"
            "# TYPE name histogram"
            "name_sum{label=\"a\"} 23.0"
            "name_count{label=\"a\"} 1.0"
            "name_bucket{label=\"a\",le=\"+Inf\"} 1.0"
            "name_bucket{label=\"a\",le=\"20\"} 0.0"]
           (m/render-metric-sets [(metric-samples/make-metric-sample-set "name with blanks" :counter "help"
                                                                         [(metric-samples/make-metric-sample "name with blanks" {:label-with*dashes "a"} 23 0)])
                                  (metric-samples/make-metric-sample-set "name" :histogram "help"
                                                                         [(metric-samples/make-metric-sample "name_sum" {:label "a"} 23 0)
                                                                          (metric-samples/make-metric-sample "name_count" {:label "a"} 1 0)
                                                                          (metric-samples/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
                                                                          (metric-samples/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)])]))))

(t/deftest t-render-metrics!
  (t/is (= [] (m/render-metrics! []))))

(t/deftest t-wrap-prometheus-metrics-ring-handler
  (t/is (= "ELSE"
           (:body ((m/wrap-prometheus-metrics-ring-handler (constantly {:body "ELSE"})) {:uri "/something-else"}))))
  (t/is (not= "ELSE"
              (:body ((m/wrap-prometheus-metrics-ring-handler (constantly "ELSE")) {:uri "/metrics"})))))

(t/deftest t-render-big-ints
  (t/is (= ["# HELP name_with_blanks help"
            "# TYPE name_with_blanks counter"
            "name_with_blanks{label_with_dashes=\"a\"} 1.0E24"]
           (m/render-metric-sets [(metric-samples/make-metric-sample-set
                                   "name with blanks"
                                   :counter
                                   "help"
                                   [(metric-samples/make-metric-sample
                                     "name with blanks"
                                     {:label-with*dashes "a"}
                                     ;; MetricValue converts all values to double
                                     (double 999999999999999999999999)
                                     0)])]))))

(t/deftest t-render-longs
  (t/is (= ["# HELP name_with_blanks help"
            "# TYPE name_with_blanks counter"
            "name_with_blanks{label_with_dashes=\"a\"} 9.0E18"]
           (m/render-metric-sets [(metric-samples/make-metric-sample-set
                                   "name with blanks"
                                   :counter
                                   "help"
                                   [(metric-samples/make-metric-sample
                                     "name with blanks"
                                     {:label-with*dashes "a"}
                                     ;; MetricValue converts all values to double
                                     (double 8999999999999999991)
                                     0)])]))))

(t/deftest benchmark-test
  ;; plain mapping/string.join: 840 msecs; lazy-seq: 940 msecs
  ;; Note: weird how it's a bit slower with lazy sequences; but it might be worth it to create less memory pressure.
  (let [nmetrics 20000
        nlabels 10
        thresholds [5.0 10.0 50.0 80.0 99.0]
        values [3892.0 230498.0 60000.0 38234.5 2339349.4]]
    (metric-accumulator/reset-global-metric-store!)
    (doseq [m (range nmetrics)]
      (let [t (case (mod m 3)
                0 :counter
                1 :gauge
                2 :histogram)
            nbuckets (mod m 5)
            metric (case t
                     :counter (metric-types/make-counter-metric (str m) "")
                     :gauge (metric-types/make-gauge-metric (str m) "")
                     :histogram (metric-types/make-histogram-metric (str m) ""
                                                                    (take nbuckets thresholds)))]
        (doseq [l (range nlabels)]
          ;; assuming the absolute values don't matter much for the rendering
          (case t
            :histogram (metric-accumulator/record-metric! metric {:label l} (nth values nbuckets) 209384039)
            (metric-accumulator/record-metric! metric {:label l} 982374.0 209384039)))))

    (time
     (t/is (= 506640 (count (m/render-metrics!)))))))

