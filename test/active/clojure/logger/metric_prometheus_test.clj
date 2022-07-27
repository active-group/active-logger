(ns active.clojure.logger.metric-prometheus-test
  (:require [active.clojure.logger.metric-prometheus :as m]
            [clojure.test :as t]
            [clojure.spec.test.alpha :as stest]))

(t/deftest t-render-metrics!
  (t/is (= "FIXME\n[]" (m/render-metrics!))))

(t/deftest t-wrap-prometheus-metrics-ring-handler
  (t/is (= "ELSE"
           (:body ((m/wrap-prometheus-metrics-ring-handler (constantly {:body "ELSE"})) {:uri "/something-else"}))))
  (t/is (= (m/render-metrics!)
           (:body ((m/wrap-prometheus-metrics-ring-handler (constantly "ELSE")) {:uri "/metrics"})))))
