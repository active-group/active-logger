(ns active.clojure.logger.metric-prometheus
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [clojure.string :as string]))

(defn render-label
  [k v]
  (str (string/replace (name k) "-" "_") "=\"" v "\""))

(defn render-labels
  [labels]
  (if (empty? labels)
    ""
    (str "{"
         (string/join "," (mapv render-label (keys labels) (vals labels)))
         "}")))

(defn maybe-render-timestamp
  [maybe-timestamp]
  (when maybe-timestamp
    (format " %d" maybe-timestamp)))

(defn render-metric-sample
  [metric-sample]
  (str (metric-accumulator/metric-sample-name metric-sample)
       (render-labels (metric-accumulator/metric-sample-labels metric-sample))
       " " (metric-accumulator/metric-sample-value metric-sample)
       (maybe-render-timestamp (metric-accumulator/metric-sample-timestamp metric-sample))))

(defn render-metric-type
  [metric-type]
  (case metric-type
    :gauge "gauge"
    :counter "counter"
    :histogram "histogram"))

(defn render-metric-set
  [metric-sample-set]
  (string/join "\n"
               (concat
                 [(str "# HELP " (metric-accumulator/metric-sample-set-name metric-sample-set) " "
                       (metric-accumulator/metric-sample-set-help metric-sample-set))
                  (str "# TYPE " (metric-accumulator/metric-sample-set-name metric-sample-set) " "
                       (render-metric-type (metric-accumulator/metric-sample-set-type metric-sample-set)))]
                 (mapv render-metric-sample (metric-accumulator/metric-sample-set-samples metric-sample-set)))))

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
