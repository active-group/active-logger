(ns active.clojure.logger.config.timbre-test
  (:require [active.clojure.config :as active-config]
            [active.clojure.logger.config.timbre :as timbre-config]
            [clojure.test :as t]))

(def timbre-config-schema (active-config/schema "Timbre-config schema" timbre-config/timbre-config-section))

(t/deftest configuration->timbre-config-test
  ;; See https://github.com/ptaoussanis/timbre/releases/tag/v5.0.0
  ;; "[Deprecated] :level config option is being renamed :min-level"
  (let [log-level        :error
        timbre-config #(timbre-config/configuration->timbre-config
                        (active-config/section-subconfig
                         (active-config/make-configuration timbre-config-schema [] %)
                         timbre-config/timbre-config-section))]
    (t/testing ":min-level is used if configured"
      (t/is (= log-level (:min-level (timbre-config {:timbre-config {:min-level log-level}})))))
    (t/testing ":min-level takes precedence over :level if both are configured"
      (t/is (= log-level (:min-level (timbre-config {:timbre-config {:min-level log-level
                                                                        :level     :warn}})))))
    (t/testing "active-logger :level configuration is correctly translated timbre :min-level"
      (t/is (= log-level (:min-level (timbre-config {:timbre-config {:level log-level}})))))
    (t/testing "if neither :level nor :min-level are configured, uses timbres default"
      (t/is (= timbre-config/timbre-default-min-level (:min-level (timbre-config {:timbre-config {}})))))))
