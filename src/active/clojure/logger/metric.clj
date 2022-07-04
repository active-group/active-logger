(ns active.clojure.logger.metric
  "Facilities for logging metrics."
  (:require [active.clojure.logger.riemann :as riemann-config]
            [active.clojure.logger.internal :as internal]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.monad :as monad]
            [active.clojure.config :as config]
            [active.clojure.record :refer [define-record-type]]))


;;;; Configuration

(def log-metrics-command-config-setting
  (config/setting :log-metrics-command-config
                  "Monad command config for running metric log commands."
                  ;; TODO allow port/host settings
                  (config/one-of-range #{:riemann :events :no-push} :events)))


(def metrics-config-default :events)
(defonce metrics-config (atom metrics-config-default))

(defn configure-metrics-logging
  "Returns an object that can be fed to
  [[set-global-log-metrics-config!]]."
  [riemann-config desc]
  (case desc
    :events  :events
    :no-push :no-push
    :riemann (riemann-config/make-riemann-config riemann-config)))

(defn set-global-log-metrics-config!
  [scc]
  (reset! metrics-config scc))

(defn reset-global-log-metrics-config!
  "Reset to back to default, if the config equals `compare`."
  [compare]
  (swap! metrics-config #(if (= % compare) metrics-config-default %)))


;;;; Data definition and DSL

(define-record-type LogMetric
  (make-log-metric namespace label value map) log-metric?
  [namespace log-metric-namespace
   ^{:doc "String"} label log-metric-label
   ^{:doc "Scalar value"} value log-metric-value
   ^{:doc "Map with more data or `nil`. The context is a map that is merged
  with the log context that's already active, if present."}
   map log-metric-map])

(define-record-type ^{:doc "Get the system time in milliseconds"} GetMilliTime
  (make-get-milli-time) get-milli-time? [])

;;; Actions

(def get-milli-time (make-get-milli-time))

(defn emit-metric-to-events!
  [namespace label value mp]
  (internal/log-event!-internal "metric"
                                namespace
                                :info
                                (merge mp {:label label :metric value})
                                (delay
                                  [(str "Metric " label " = " value)])))

(defn emit-metric-to-riemann!
  [config label value mp]
  (riemann-config/send-event-to-riemann! config "metric" mp {:label label :metric value}))

(defn emit-metric-sample!-internal
  ([namespace metric-sample context-map]
   (emit-metric-sample!-internal @metrics-config namespace metric-sample context-map))
  ([scconf namespace metric-sample context-map]
   (when (not= :no-push scconf)
     (let [mp            (internal/sanitize-context context-map)
           metric-name   (metric-accumulator/metric-sample-name metric-sample)
           metric-labels (metric-accumulator/metric-sample-labels metric-sample)
           metric-value  (metric-accumulator/metric-sample-value metric-sample)]
       (case scconf
         :events (emit-metric-to-events! namespace metric-name metric-labels metric-value mp)
         (emit-metric-to-riemann! scconf metric-name metric-value mp))))))

(defn emit-metric-samples!-internal
  [namespace metric-samples context-map]
  (let [sanitized-context-map (internal/sanitize-context context-map)
        scconf @metrics-config]
  (doseq [metric-sample metric-samples]
    (emit-metric-samples!-internal namespace metric-sample sanitized-context-map))))

(defn get-milli-time!
  []
  (/ (double (System/nanoTime)) 1000000.0))

(defmacro log-metric!
  ([?label ?value]
   `(internal/log-metric!-internal ~(str *ns*) ~?label ~?value nil))
  ([?label ?value ?mp]
   `(internal/log-metric!-internal ~(str *ns*) ~?label ~?value ~?mp)))

(defmacro log-metric
  ([?label ?value]
   `(make-log-metric ~(str *ns*) ~?label ~?value nil))
  ([?label ?value ?mp]
   `(make-log-metric ~(str *ns*) ~?label ~?value ~?mp))
  ([?label ?value ?mp ?ns]
   `(make-log-metric ~?ns ~?label ~?value ~?mp)))

;;;; Interpreter

(defn run-emit-metric
  [run-any env mstate m]
  (cond
    (log-metric? m)
    (do
      (emit-metric-sample!-internal (log-metric-namespace m)
                                    (log-metric-label m)
                                    (log-metric-value m)
                                    (log-metric-map m))
      [nil mstate])

    (get-milli-time? m)
    [(get-milli-time!) mstate]

    :else
    monad/unknown-command))

(def log-metrics-command-config
  (monad/make-monad-command-config run-emit-metric {} {}))
