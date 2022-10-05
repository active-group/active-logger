(ns active.clojure.logger.metric-prometheus
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-prometheus-util :as util]
            [clojure.string :as string]))

(defn render-metric-sample
  [metric-sample]
  (str (util/cleanup-non-prometheus-label-characters (metric-accumulator/metric-sample-name metric-sample))
       (util/render-labels (metric-accumulator/metric-sample-labels metric-sample))
       " " (util/render-value (metric-accumulator/metric-sample-value metric-sample))
       (util/maybe-render-timestamp (metric-accumulator/metric-sample-timestamp metric-sample))))

(defn render-metric-type
  [metric-type]
  (case metric-type
    :gauge "gauge"
    :counter "counter"
    :histogram "histogram"))

(defn render-metric-set
  [metric-sample-set]
  (let [set-name (util/cleanup-non-prometheus-label-characters (metric-accumulator/metric-sample-set-name metric-sample-set))]
    (string/join "\n"
                 (concat
                  [(str "# HELP " set-name " "
                        (metric-accumulator/metric-sample-set-help metric-sample-set))
                   (str "# TYPE " set-name " "
                        (render-metric-type (metric-accumulator/metric-sample-set-type metric-sample-set)))]
                  (mapv render-metric-sample (metric-accumulator/metric-sample-set-samples metric-sample-set))))))

(defn render-metric-sets
  [ms]
  (string/join "\n"
               (mapv render-metric-set ms)))

(defn render-metrics!
  []
  (let [all-metric-sets (metric-accumulator/get-all-metric-sample-sets!)]
    (render-metric-sets all-metric-sets)))

(defn wrap-prometheus-metrics-ring-handler
  [handler]
  (fn [req]
    (if (re-matches #"^/metrics" (:uri req))
      {:status 200 :headers {"Content-Type" "text/plain"} :body (render-metrics!)}
      (handler req))))
