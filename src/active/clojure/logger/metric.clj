(ns active.clojure.logger.metric
  "Facilities for logging metrics."
  (:require [active.clojure.logger.config.riemann :as riemann-config]
            [active.clojure.logger.internal :as internal]
            [active.clojure.monad :as monad]
            [active.clojure.record :refer [define-record-type]]))


;;;; Configuration

(def metrics-config-default :events)
(defonce metrics-config (atom metrics-config-default))

(defn configure-metrics-logging
  "Returns an object that can be fed to
  [[set-global-log-metrics-config!]]."
  [riemann-config desc]
  (case desc
    :events  :events
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
   ^{:doc "Map with more data or `nil`, see [[log-context-keys]]."}
   map log-metric-map])

(define-record-type ^{:doc "Get the system time in milliseconds"} GetMilliTime
  (make-get-milli-time) get-milli-time? [])

;;; Actions

(def get-milli-time (make-get-milli-time))

(defn log-metric-to-events!
  [namespace label value mp]
  (internal/log-event!-internal "metric"
                                namespace
                                :info
                                (merge mp {:label label :metric value})
                                (delay
                                  [(str "Metric " label " = " value)])))

(defn log-metric-to-riemann!
  [config label value mp]
  (riemann-config/send-event-to-riemann! config "metric" mp {:label label :metric value}))

(defn log-metric!-internal
  [namespace label value mp]
  (let [mp (internal/sanitize-context mp)
        scconf @metrics-config]
    (case scconf
      :events (log-metric-to-events! namespace label value mp)
      (log-metric-to-riemann! scconf label value mp))))

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

(defn run-log-metric
  [run-any env mstate m]
  (cond
    (log-metric? m)
    (do
      (log-metric!-internal (log-metric-namespace m)
                            (log-metric-label m)
                            (log-metric-value m)
                            (log-metric-map m))
      [nil mstate])

    (get-milli-time? m)
    [(get-milli-time!) mstate]

    :else
    monad/unknown-command))

(def log-metrics-command-config
  (monad/make-monad-command-config run-log-metric {} {}))
