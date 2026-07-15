(ns active.clojure.logger.metric-prometheus-test
  (:require [active.clojure.logger.metric-prometheus :as m]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-singular-value :as metric-singular-value]
            [active.clojure.logger.metric-histogram-value :as metric-histogram-value]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [clojure.test :as t]))

(t/deftest t-render-metric-sets
  (t/is (= "# HELP name_with_blanks help\n# TYPE name_with_blanks counter\nname_with_blanks{label_with_dashes=\"a\"} 23.0\n# HELP name help\n# TYPE name histogram\nname_sum{label=\"a\"} 23.0\nname_count{label=\"a\"} 1.0\nname_bucket{label=\"a\",le=\"+Inf\"} 1.0\nname_bucket{label=\"a\",le=\"20\"} 0.0"
           (m/render-metric-sets [(metric-samples/make-metric-sample-set "name with blanks" :counter "help"
                                                                         [(metric-samples/make-metric-sample "name with blanks" {:label-with*dashes "a"} 23 0)])
                                  (metric-samples/make-metric-sample-set "name" :histogram "help"
                                                                         [(metric-samples/make-metric-sample "name_sum" {:label "a"} 23 0)
                                                                          (metric-samples/make-metric-sample "name_count" {:label "a"} 1 0)
                                                                          (metric-samples/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
                                                                          (metric-samples/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)])]))))

(t/deftest t-render-metrics!
  (t/is (= "" (m/render-metrics! []))))

(t/deftest t-wrap-prometheus-metrics-ring-handler
  (t/is (= "ELSE"
           (:body ((m/wrap-prometheus-metrics-ring-handler (constantly {:body "ELSE"})) {:uri "/something-else"}))))
  (t/is (not= "ELSE"
              (:body ((m/wrap-prometheus-metrics-ring-handler (constantly "ELSE")) {:uri "/metrics"})))))

(t/deftest t-render-big-ints
  (t/is (= "# HELP name_with_blanks help\n# TYPE name_with_blanks counter\nname_with_blanks{label_with_dashes=\"a\"} 1.0E24"
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
  (t/is (= "# HELP name_with_blanks help\n# TYPE name_with_blanks counter\nname_with_blanks{label_with_dashes=\"a\"} 9.0E18"
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
  ;; plain mapping/string.join: 1061.141 msecs
  (let [nmetrics 20000
        nlabels 10
        thresholds [5.0 10.0 50.0 80.0 99.0]
        values [3892.0 230498.0 60000.0 38234.5 2339349.4]
        ;; data is a map: {metric {labels histo-or-singular}}
        data (into {}
                   (map (fn [m]
                          (let [t (case (mod m 3)
                                    0 :counter
                                    1 :gauge
                                    2 :histogram)
                                nbuckets (mod m 5)]
                            [(case t
                               :counter (metric-types/make-counter-metric (str m) "")
                               :gauge (metric-types/make-gauge-metric (str m) "")
                               :histogram (metric-types/make-histogram-metric (str m) ""
                                                                              (take nbuckets thresholds)))
                             (into {}
                                   (map (fn [l]
                                          ;; assuming the absolute values don't matter much for the rendering
                                          [{:label (str l)
                                            :info "host:999"}
                                           (case t
                                             :counter (metric-singular-value/make-metric-value 2.1 100)
                                             :gauge (metric-singular-value/make-metric-value 982374.0 209384039)
                                             :histogram (metric-histogram-value/make-histogram-metric-values 10000 4748823.0 4839 (take nbuckets values)))])
                                        (range nlabels)))]))
                        (range nmetrics)))]
    (time
     (let [s (-> data
                 (metric-accumulator/all-snapshots->all-metric-sample-sets)
                 (m/render-metric-sets))]
       (t/is (string? s))))))
