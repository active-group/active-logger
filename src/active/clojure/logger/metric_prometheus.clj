(ns active.clojure.logger.metric-prometheus
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.timed-metric :as timed-metrics]
            [active.clojure.logger.metric-prometheus-util :as util]
            [clojure.string :as string]))

(defn render-metric-sample
  [metric-sample]
  (str (util/cleanup-non-prometheus-label-characters (metric-samples/metric-sample-name metric-sample))
       (util/render-labels (metric-samples/metric-sample-labels metric-sample))
       " " (util/render-value (metric-samples/metric-sample-value metric-sample))))

(defn render-metric-type
  [metric-type]
  (case metric-type
    :gauge "gauge"
    :counter "counter"
    :histogram "histogram"))

(defn render-metric-set
  [metric-sample-set]
  (let [set-name (util/cleanup-non-prometheus-label-characters (metric-samples/metric-sample-set-name metric-sample-set))]
    (string/join "\n"
                 (concat
                  [(str "# HELP " set-name " "
                        (metric-samples/metric-sample-set-help metric-sample-set))
                   (str "# TYPE " set-name " "
                        (render-metric-type (metric-samples/metric-sample-set-type metric-sample-set)))]
                  (mapv render-metric-sample (metric-samples/metric-sample-set-samples metric-sample-set))))))

(defn render-metric-sets
  [ms]
  (string/join "\n"
               (mapv render-metric-set ms)))

(def ^:private number-of-calls
  (metric-types/make-counter-metric "active_clojure_logger_metric_prometheus_render_metrics_total"
                                    "Total number of calls to `render-metrics`."))

(def ^:private duration
  (metric-types/make-histogram-metric "active_clojure_logger_metric_render_metrics_duration_milliseconds"
                                      "Duration of rendering metrics." []))

(def ^:private number-of-sets
  (metric-types/make-gauge-metric "active_clojure_logger_metric_prometheus_metric_sets_total"
                                  "Total number stored metric sets."))

(def ^:private number-of-samples
  (metric-types/make-gauge-metric "active_clojure_logger_metric_prometheus_metric_samples_total"
                                  "Total number stored metric samples."))

(defn render-metrics!
  [& [metric-sets]]
  (metric-accumulator/record-metric! number-of-calls {} 1)
  (let [all-metric-sets (timed-metrics/log-time-metric!
                         #(metric-accumulator/record-metric! duration {:slice "get"} %)
                         (or metric-sets (metric-accumulator/get-all-metric-sample-sets!)))
        sorted-metric-sets (timed-metrics/log-time-metric!
                            #(metric-accumulator/record-metric! duration {:slice "sort"} %)
                            (sort-by metric-samples/metric-sample-set-name all-metric-sets))]
    (timed-metrics/log-time-metric!
     #(metric-accumulator/record-metric! duration {:slice "count"} %)
     (do
       (metric-accumulator/record-metric! number-of-sets {} (count sorted-metric-sets))
       (metric-accumulator/record-metric! number-of-samples {}
                                          (reduce + 0 (map #(count (metric-samples/metric-sample-set-samples %)) sorted-metric-sets)))))
    (timed-metrics/log-time-metric!
     #(metric-accumulator/record-metric! duration {:slice "render"} %)
     (render-metric-sets sorted-metric-sets))))

(defn wrap-prometheus-metrics-ring-handler
  [handler]
  (fn [req]
    (if (re-matches #"^/metrics" (:uri req))
      {:status 200 :headers {"Content-Type" "text/plain"} :body (render-metrics!)}
      (handler req))))
