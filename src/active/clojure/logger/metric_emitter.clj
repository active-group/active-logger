(ns active.clojure.logger.metric-emitter
  "Facilities for emitting metrics."
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

(define-record-type EmitMetric
  (make-emit-metric namespace sample map) emit-metric?
  [namespace emit-metric-namespace
   ^{:doc "Metric sample"} sample emit-metric-sample
   ^{:doc "Map with more data or `nil`. The context is a map that is merged
  with the log context that's already active, if present."}
   map emit-metric-map])

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
     (let [metric-name   (metric-accumulator/metric-sample-name metric-sample)
           metric-labels (metric-accumulator/metric-sample-labels metric-sample)
           metric-value  (metric-accumulator/metric-sample-value metric-sample)]
       (case scconf
         :events (emit-metric-to-events! namespace metric-name metric-labels metric-value context-map)
         (emit-metric-to-riemann! scconf metric-name metric-value context-map))))))

(defn emit-metric-samples!-internal
  [namespace metric-samples context-map]
  (let [scconf @metrics-config]
  (doseq [metric-sample metric-samples]
    (emit-metric-samples!-internal namespace metric-sample context-map))))


;;;; Interpreter

(defn run-emit-metric
  [run-any env mstate m]
  (cond
    (emit-metric? m)
    (do
      (emit-metric-sample!-internal (emit-metric-namespace m)
                                    (emit-metric-sample m)
                                    (emit-metric-map m))
      [nil mstate])

    :else
    monad/unknown-command))

(def log-metrics-command-config
  (monad/make-monad-command-config run-emit-metric {} {}))

(defmacro emit-metric!
  ([?metric-sample]
   `(emit-metric-sample!-internal ~(str *ns*) ~?metric-sample nil))
  ([?metric-sample ?mp]
   `(emit-metric-sample!-internal ~(str *ns*) ~?metric-sample ~?mp)))

(defmacro emit-metric
  ([?metric-sample]
   `(make-emit-metric ~(str *ns*) ~?metric-sample nil))
  ([?metric-sample ?mp]
   `(make-emit-metric ~(str *ns*) ~?metric-sample ~?mp))
  ([?metric-sample ?mp ?ns]
   `(make-emit-metric ~?ns ~?metric-sample ~?mp)))
