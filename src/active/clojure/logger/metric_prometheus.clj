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
    (fn [metric-sample-set sample-counter]
      (let [set-name (cleanup-non-prometheus-label-characters (metric-samples/metric-sample-set-name metric-sample-set))]
        (cons
         (render-metric-help set-name (metric-samples/metric-sample-set-help metric-sample-set))
         (cons
          (render-metric-type set-name (metric-samples/metric-sample-set-type metric-sample-set))
          (map (fn [sample]
                 (vswap! sample-counter inc)
                 (render-metric-sample sample))
               (metric-samples/metric-sample-set-samples metric-sample-set))))))))

(defn- render-metric-sets-seq*
  "Returns a lazy sequence of lines."
  [ms set-counter sample-counter]
  ;; Note: the 'make-fn..*' schenanigans is used to make/enable memoization for this run, without growing memory infinitely.
  (let [render-metric-set (make-render-metric-set (util/make-cleanup-non-prometheus-label-characters))]
    (mapcat (fn [set]
              (vswap! set-counter inc)
              (render-metric-set set sample-counter))
            ms)))

(defn ^:no-doc render-metric-sets-seq
  "Returns a lazy sequence of lines."
  [ms]
  (render-metric-sets-seq* ms (volatile! 0) (volatile! 0)))

(defn render-metric-sets
  [ms]
  (string/join "\n" (render-metric-sets-seq ms)))

(defn- render-metrics-seq!*
  ([set-counter sample-counter]
   (render-metrics-seq!* (metric-accumulator/get-all-metric-sample-sets!)
                         set-counter sample-counter))
  ([metric-sets set-counter sample-counter]
   (render-metric-sets-seq* metric-sets set-counter sample-counter)))

(defn ^:no-doc render-metrics-seq!
  "Returns a lazy sequence of lines."
  ([]
   (render-metrics-seq!* (volatile! 0) (volatile! 0)))
  ([metric-sets]
   (render-metrics-seq!* metric-sets (volatile! 0) (volatile! 0))))

(defn render-metrics!
  ([]
   (string/join "\n" (render-metrics-seq!)))
  ([metric-sets]
   (string/join "\n" (render-metrics-seq! metric-sets))))

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

(defn- render-metrics-body! []
  (metric-accumulator/record-metric! number-of-calls {} 1)
  (piped-input-stream
   (fn [ostream]
     (with-open [^Writer w (io/writer ostream)]
       (let [set-counter (volatile! 0)
             sample-counter (volatile! 0)]
         (timed-metrics/log-time-metric!
          #(metric-accumulator/record-metric! duration {:slice "render"} %)
          (doseq [l (render-metrics-seq! set-counter sample-counter)]
            (.write w l)
            (.write w "\n")))
         (metric-accumulator/record-metric! number-of-sets {} @set-counter)
         (metric-accumulator/record-metric! number-of-samples {} @sample-counter))))))

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
