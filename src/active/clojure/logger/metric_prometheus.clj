(ns active.clojure.logger.metric-prometheus
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]))

(defn render-metrics!
  []
  (let [all-metrics (metric-accumulator/get-metric-samples!)]
    (str "FIXME" "\n"
         (pr-str all-metrics))))

(defn wrap-prometheus-metrics-ring-handler
  [handler]
  (fn [req]
    (if (re-matches #"^/metrics" (:uri req))
      {:status 200 :headers {"Content-Type" "text/plain"} :body (render-metrics!)}
      (handler req))))
