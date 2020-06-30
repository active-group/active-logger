(ns active.clojure.logger.state-change
  "Logging state changes."
  (:require [active.clojure.logger.config.riemann :as riemann-config]
            [active.clojure.logger.internal :as internal]
            [active.clojure.monad :as monad]
            [active.clojure.record :refer [define-record-type]]
            [active.clojure.logger.config.riemann :as riemann-config]
            [riemann.client :as riemann]
            [taoensso.timbre :as timbre]))

(define-record-type LogStateChange
  (make-log-state-change namespace state ttl map)
  log-state-change?
  [^{:doc "String"}
   ;; in case this gets redirected to event log
   namespace log-state-change-namespace
   ^{:doc "Arbitrary string, should be from finite set"}
   state log-state-change-state
   ^{:doc "Floating-point number in seconds or `nil`."}
   ttl log-state-change-ttl
   ^{:doc "Map with more data or `nil`, see [[log-context-keys]]."}
    map log-state-change-map])

(defmacro log-state-change
  "Log a state change asynchronously (monadic version), constructed from the arguments.

  `?ttl`, if present, is the time-to-live in seconds."
  ([?state]
   `(make-log-state-change ~(str *ns*) ~?state nil nil))
  ([?state ?ttl]
   `(make-log-state-change ~(str *ns*) ~?state ~?ttl nil))
  ([?state ?ttl ?mp]
   `(make-log-state-change ~(str *ns*) ~?state ~?ttl ~?mp)))

(def state-changes-config-default :events)
(defonce state-changes-config (atom state-changes-config-default))

; helper to avoid having to construct a merged map
(defmacro log-context-access
  [?mp ?key]
  `(or (get ~?mp ~?key)
       (get timbre/*context* ~?key)))

; apply this before adding :metric, :ttl, :time etc.

(defn log-state-change-to-events!
  [namespace state mp]
  (internal/log-event!-internal "state"
                                namespace
                                :info
                                {:state state}
                                (delay
                                  [(str "Changed state to "
                                        state)])))

(defn log-state-change!-internal
  [namespace state ttl mp]
  (let [mp (internal/sanitize-context mp)
        scconf @state-changes-config]
    (case scconf
      :events (log-state-change-to-events! namespace state mp)
      (riemann-config/log-state-change-to-riemann! scconf state ttl mp))))

(defn run-log-state-change
  [run-any env mstate m]
  (cond
    (log-state-change? m)
    (do
      (log-state-change!-internal (log-state-change-namespace m)
                                  (log-state-change-state m)
                                  (log-state-change-ttl m)
                                  (log-state-change-map m))
      [nil mstate])
    :else
    monad/unknown-command))

(def log-state-changes-command-config
  (monad/make-monad-command-config run-log-state-change {} {}))

(defn set-global-log-state-changes-config!
  [scc]
  (reset! state-changes-config scc))

(defn reset-global-log-state-changes-config!
  "Reset to back to default, if the config equals `compare`."
  [compare]
  (swap! state-changes-config #(if (= % compare) state-changes-config-default %)))

(defn configure-state-changes-logging
  "Returns an object that can be fed to
  [[set-global-log-state-changes-config!]]."
  [access-config desc]
  (case desc
    :events :events
    :riemann (riemann-config/make-riemann-config access-config)))
