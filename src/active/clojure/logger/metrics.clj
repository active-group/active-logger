(ns active.clojure.logger.metrics
  (:require [active.clojure.record :refer [define-record-type]]))

(define-record-type LogMetric
  (make-log-metric namespace label value map)
  log-metric?
  [namespace log-metric-namespace
   ^{:doc "String"}
   label log-metric-label
   ^{:doc "Scalar value"}
   value log-metric-value
   ^{:doc "Map with more data or `nil`, see [[log-context-keys]]."}
   map log-metric-map])

(define-record-type
  ^{:doc "Get the system time in milliseconds"}
  GetMilliTime
  (make-get-milli-time)
  get-milli-time?
  [])

(def get-milli-time (make-get-milli-time))

(defmacro log-metric
  ([?label ?value]
   `(make-log-metric ~(str *ns*) ~?label ~?value nil))
  ([?label ?value ?mp]
   `(make-log-metric ~(str *ns*) ~?label ~?value ~?mp))
  ([?label ?value ?mp ?ns]
   `(make-log-metric ~?ns ~?label ~?value ~?mp)))

(defn log-metric-to-events!
  [namespace label value mp]
  (log-event!-internal "metric"
                       namespace
                       :info
                       (merge mp {:label label :metric value})
                       (delay
                        [(str "Metric " label " = " value)])))

(defn log-metric-to-riemann!
  [config label value mp]
  (send-event-to-riemann! config "metric" mp {:label label :metric value}))

(def metrics-config-default :events)
(defonce metrics-config (atom metrics-config-default))

(defn log-metric!-internal
  [namespace label value mp]
  (let [mp (sanitize-context mp)
        scconf @metrics-config]
    (case scconf
      :events (log-metric-to-events! namespace label value mp)
      (log-metric-to-riemann! scconf label value mp))))

(defn get-milli-time!
  []
  (/ (double (System/nanoTime)) 1000000.0))

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

