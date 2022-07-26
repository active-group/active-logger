(ns active.clojure.logger.metric-prometheus-test
  (:require [active.clojure.logger.metric-prometheus :as m]
            [clojure.test :as t]
            [clojure.spec.test.alpha :as stest]))

(t/deftest t-render-metrics!
  (t/is (= "FIXME\n[]" (m/render-metrics!))))
