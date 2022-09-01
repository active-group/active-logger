(ns active.clojure.logger.test
  (:require [active.clojure.logger.timbre :as timbre-config]
            [active.clojure.logger.event :as event]
            [active.clojure.logger.state-change :as state-change]
            [active.clojure.logger.metric-emitter :as metric-emitter]
            [taoensso.timbre :as timbre]))

(defn- log-config-test-fixture [config f]
  (event/set-global-log-events-config! config)
  (state-change/set-global-log-state-changes-config! (state-change/configure-state-changes-logging nil :events))
  (metric-emitter/set-global-log-metrics-config! (metric-emitter/configure-metrics-logging :events))
  ;; Note: set-global-log-events-config! uses timbre/set-config!,
  ;; which in turn used alter-var-root, but apparently using
  ;; with-config, which uses `binding` has a better effect of logs
  ;; from spawned threads.
  (timbre/with-config config (f)))

(defn log-ignore-test-fixture
  "For use as clojure.test fixture."
  [f]
  (log-config-test-fixture (timbre-config/make-timbre-config {})
                           f))

(defn log-stdout-test-fixture
  "For use as clojure.test fixture."
  [f]
  (log-config-test-fixture (timbre-config/make-timbre-config {:appenders {:stdout (timbre/println-appender)}})
                           f))

