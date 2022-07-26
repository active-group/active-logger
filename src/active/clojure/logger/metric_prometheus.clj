(ns active.clojure.logger.metric-prometheus
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]))

(defn render-metrics!
  []
  (let [all-metrics (metric-accumulator/get-metric-samples!)]
    (str "FIXME" "\n"
         (pr-str all-metrics))))

