(ns active.clojure.logger.core
  (:require [active.clojure.logger.internal :as internal]
            [active.clojure.logger.state-change :as state-change]
            [active.clojure.logger.metric :as metric]
            [active.clojure.logger.config.timbre :as timbre-config]
            [taoensso.timbre :as timbre]))

(defmacro log-state-change!
  "Log a state change asynchronously (imperative version).

  `?more`, if present, is a map with more properties."
  ([?context ?state]
   `(internal/log-state-change!-internal ~(str *ns*) ~?state nil ~?context))
  ([?context ?state ?ttl]
   `(internal/log-state-change!-internal ~(str *ns*) ~?state ~?ttl ~?context))
  ([?context ?state ?ttl ?mp]
   `(internal/log-state-change!-internal ~(str *ns*) ~?state ~?ttl (merge ~?context ~?mp))))

(defmacro log-metric!
  ([?label ?value]
   `(internal/log-metric!-internal ~(str *ns*) ~?label ~?value nil))
  ([?label ?value ?mp]
   `(internal/log-metric!-internal ~(str *ns*) ~?label ~?value ~?mp)))

(defn set-global-log-events-config!
  [ec]
  (timbre/set-config! ec))

(defonce timbre-default-config timbre/*config*) ;; actually not nil!

(defn destroy-timbre-config!
  "Cleans up resources that might be held in result of [[make-timbre-config]] resp. [[configure-events-logging]]"
  [timbre-config]
  ;; we use a non-standard field :cleanup-fn in the appenders spec (see https://github.com/ptaoussanis/timbre/issues/217)
  (doseq [[id appender] (:appenders timbre-config)]
    (when-let [f (:cleanup-fn appender)]
      (f appender))))

(defn reset-global-log-events-config!
  "Reset logging config to default, if it equals `compare`."
  [compare]
  (timbre/swap-config! #(if (= % compare) timbre-default-config %)))

(defmacro with-log-context!
  "Attach context to a log for the extent of the body."
  [?context & ?body]
  `(timbre/with-context (merge timbre/*context* (sanitize-context ~?context))
     ~@?body))

(defmacro log-event!
  "Log an event, described by a level and the arguments
  (imperative version; global config).

  Uses current namespace as origin."
  [?level ?msg]
  `(internal/log-event!-internal "event"
                                 ~(str *ns*)
                                 ~?level
                                 nil
                                 (delay [~?msg])))

(defn- -log-event! [level msg] ;; forward declared above
  (log-event! level msg))

(defmacro log-exception-event!
  "Log an exception event."
  [?level ?msg ?throwable]
  `(internal/log-exception-event!-internal "event"
                                           ~(str *ns*)
                                           ~?level
                                           nil
                                           (delay [~?msg (str ~?throwable)])
                                           ~?throwable))

(defmacro log-event-with-context!
  "Log an event, described by a level and the arguments
  (imperative version; global config).

  Uses current namespace as origin."
  [?context ?level ?msg]
  `(internal/log-event!-internal "event"
                                 ~(str *ns*)
                                 ~?level
                                 (sanitize-context ~?context)
                                 (delay [~?msg])))

(defmacro log-exception-event-with-context!
  "Log an exception event."
  [?context ?level ?msg ?throwable]
  `(internal/log-exception-event!-internal "event"
                                           ~(str *ns*)
                                           ~?level
                                           (internal/sanitize-context ~?context)
                                           (delay [~?msg (str ~?throwable)]) ~?throwable))

(defn current-log-context
  []
  timbre/*context*)

