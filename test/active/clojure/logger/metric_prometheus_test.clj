(ns active.clojure.logger.metric-prometheus-test
  (:require [active.clojure.logger.metric-prometheus :as m]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [clojure.test :as t]))


(t/deftest t-render-metric-sets
  (t/is (= "# HELP name_with_blanks help\n# TYPE name_with_blanks counter\nname_with_blanks{label_with_dashes=\"a\"} 23.0 0\n# HELP name help\n# TYPE name histogram\nname_sum{label=\"a\"} 23.0 0\nname_count{label=\"a\"} 1.0 0\nname_bucket{label=\"a\",le=\"+Inf\"} 1.0 0\nname_bucket{label=\"a\",le=\"20\"} 0.0 0"
           (m/render-metric-sets [(metric-accumulator/make-metric-sample-set "name with blanks" :counter "help"
                                                                             [(metric-accumulator/make-metric-sample "name with blanks" {:label-with*dashes "a"} 23 0)])
                                  (metric-accumulator/make-metric-sample-set "name" :histogram "help"
                                                                             [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
                                                                              (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
                                                                              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
                                                                              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)])]))))

(t/deftest t-render-metrics!
  (t/is (= "" (m/render-metrics!))))

(t/deftest t-wrap-prometheus-metrics-ring-handler
  (t/is (= "ELSE"
           (:body ((m/wrap-prometheus-metrics-ring-handler (constantly {:body "ELSE"})) {:uri "/something-else"}))))
  (t/is (= (m/render-metrics!)
           (:body ((m/wrap-prometheus-metrics-ring-handler (constantly "ELSE")) {:uri "/metrics"})))))
