(ns active.clojure.logger.metric-prometheus
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.logger.metric-types :as metric-types]
            [active.clojure.logger.timed-metric :as timed-metrics]
            [active.clojure.logger.metric-prometheus-util :as util]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [java.io
            PipedInputStream PipedOutputStream
            Writer]))

(defn- make-render-metric-sample
  [cleanup-non-prometheus-label-characters]
  (let [render-labels (util/make-render-labels cleanup-non-prometheus-label-characters)]
    (fn [metric-sample]
      (str (cleanup-non-prometheus-label-characters (metric-samples/metric-sample-name metric-sample))
           (render-labels (metric-samples/metric-sample-labels metric-sample))
           " " (util/render-value (metric-samples/metric-sample-value metric-sample))))))

(defn- render-metric-type
  [set-name metric-type]
  (str "# TYPE " set-name " "
       (case metric-type
         :gauge "gauge"
         :counter "counter"
         :histogram "histogram")))

(defn- render-metric-help [set-name help]
  (str "# HELP " set-name " " help))

(defn- make-render-metric-set
  [cleanup-non-prometheus-label-characters]
  (let [render-metric-sample (make-render-metric-sample cleanup-non-prometheus-label-characters)]
    (fn [metric-sample-set]
      (let [set-name (cleanup-non-prometheus-label-characters (metric-samples/metric-sample-set-name metric-sample-set))]
        (cons
         (render-metric-help set-name (metric-samples/metric-sample-set-help metric-sample-set))
         (cons
          (render-metric-type set-name (metric-samples/metric-sample-set-type metric-sample-set))
          (map render-metric-sample (metric-samples/metric-sample-set-samples metric-sample-set))))))))

(defn render-metric-sets
  "Returns a lazy sequence of lines."
  [ms]
  ;; Note: the 'make-fn..*' schenanigans is used to make/enable memoization for this run, without growing memory infinitely.
  (let [render-metric-set (make-render-metric-set (util/make-cleanup-non-prometheus-label-characters))]
    (mapcat render-metric-set ms)))

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
  "Returns a lazy sequence of lines."
  ([]
   (render-metrics! (timed-metrics/log-time-metric!
                     #(metric-accumulator/record-metric! duration {:slice "get"} %)
                     (metric-accumulator/get-all-metric-sample-sets!))))
  ([metric-sets]
   (metric-accumulator/record-metric! number-of-calls {} 1)
   (let [sorted-metric-sets (timed-metrics/log-time-metric!
                             #(metric-accumulator/record-metric! duration {:slice "sort"} %)
                             (sort-by metric-samples/metric-sample-set-name metric-sets))]
     (timed-metrics/log-time-metric!
      #(metric-accumulator/record-metric! duration {:slice "count"} %)
      (do
        (metric-accumulator/record-metric! number-of-sets {} (count sorted-metric-sets))
        (metric-accumulator/record-metric! number-of-samples {}
                                           (reduce + 0 (map #(count (metric-samples/metric-sample-set-samples %)) sorted-metric-sets)))))
     (timed-metrics/log-time-metric!
      #(metric-accumulator/record-metric! duration {:slice "render"} %)
      (render-metric-sets sorted-metric-sets)))))

(defn- piped-input-stream
  [f]
  (let [input  (PipedInputStream.)
        output (PipedOutputStream.)]
    (.connect input output)
    (future
      (try
        (f output)
        (finally (.close output))))
    input))

(defn- render-metrics-body! []
  (piped-input-stream (fn [ostream]
                        (with-open [^Writer w (io/writer ostream)]
                          (doseq [l (render-metrics!)]
                            (.write w l)
                            (.write w "\n"))))))

(defn current-metrics-ring-response
  "Returns a ring response with the current metric values."
  []
  {:status 200 :headers {"Content-Type" "text/plain"} :body (render-metrics-body!)})

(defn wrap-prometheus-metrics-ring-handler
  "Ring middleware that responds to a request for '/metrics' with [[current-metrics-ring-response]] "
  [handler]
  (fn [req]
    (if (re-matches #"^/metrics" (:uri req))
      (current-metrics-ring-response)
      (handler req))))
