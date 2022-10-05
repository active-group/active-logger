(ns active.clojure.logger.metric-prometheus
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.timed-metric :as timed-metrics]
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
  [& [metric-sets]]
  (metric-accumulator/record-metric!
   (metric-accumulator/make-counter-metric "active_clojure_logger_metric_prometheus_render_metrics_total"
                                           "Total number of calls to `render-metrics`.") {}
   1)
  (let [duration-metric (metric-accumulator/make-histogram-metric "active_clojure_logger_metric_render_metrics_duration_milliseconds"
                                                                  "Duration of rendering metrics." [])
        all-metric-sets (timed-metrics/log-time-metric!
                         #(metric-accumulator/record-metric! duration-metric {:slice "get"} %)
                         (or metric-sets (metric-accumulator/get-all-metric-sample-sets!)))
        sorted-metric-sets (timed-metrics/log-time-metric!
                            #(metric-accumulator/record-metric! duration-metric {:slice "sort"} %)
                            (sort-by metric-accumulator/metric-sample-set-name all-metric-sets))]
    (timed-metrics/log-time-metric!
     #(metric-accumulator/record-metric! duration-metric {:slice "count"} %)
     (do
       (metric-accumulator/record-metric!
        (metric-accumulator/make-gauge-metric "active_clojure_logger_metric_prometheus_metric_sets_total"
                                              "Total number stored metric sets.") {}
        (count sorted-metric-sets))
       (metric-accumulator/record-metric!
        (metric-accumulator/make-gauge-metric "active_clojure_logger_metric_prometheus_metric_samples_total"
                                              "Total number stored metric samples.") {}
        (reduce + 0 (map #(count (metric-accumulator/metric-sample-set-samples %)) sorted-metric-sets)))))
    (timed-metrics/log-time-metric!
     #(metric-accumulator/record-metric! duration-metric {:slice "render"} %)
     (render-metric-sets sorted-metric-sets))))

(defn wrap-prometheus-metrics-ring-handler
  [handler]
  (fn [req]
    (if (re-matches #"^/metrics" (:uri req))
      {:status 200 :headers {"Content-Type" "text/plain"} :body (render-metrics!)}
      (handler req))))
