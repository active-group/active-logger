(ns active.clojure.logger.timbre-test
  (:require [active.clojure.config :as active-config]
            [active.clojure.logger.timbre :as timbre-config]
            [clojure.test :as t]))

(t/deftest configuration->timbre-config-test
  ;; See https://github.com/ptaoussanis/timbre/releases/tag/v5.0.0
  ;; "[Deprecated] :level config option is being renamed :min-level"
  (let [log-level        :error
        timbre-config #(timbre-config/configuration->timbre-config
                         (active-config/make-configuration timbre-config/timbre-config-schema [] %))]
    (t/testing ":min-level is used if configured"
      (t/is (= log-level (:min-level (timbre-config {:min-level log-level})))))
    (t/testing ":min-level takes precedence over :level if both are configured"
      (t/is (= log-level (:min-level (timbre-config {:min-level log-level
                                                     :level     :warn})))))
    (t/testing "active-logger :level configuration is correctly translated timbre :min-level"
      (t/is (= log-level (:min-level (timbre-config {:level log-level})))))
    (t/testing "if neither :level nor :min-level are configured, uses timbres default"
      (t/is (= timbre-config/timbre-default-min-level (:min-level (timbre-config {})))))))

(t/deftest config-test
  (t/is (= {:min-level nil,
            :level nil,
            :appenders {:default '(println)},
            :ns-filter {},
            :middleware [],
            :timestamp-opts
            {:pattern "yyyy-MM-dd HH:mm:ss.SSS",
             :locale :jvm-default,
             :timezone :jvm-default},
            :hostname "calculon.home.active-group.de",
            :application nil}
           (active-config/normalize&check-config-object timbre-config/timbre-config-schema [] {})))
  (t/is (= {:min-level [["*" :debug]],
            :level nil,
            :appenders {:default '(println)},
            :ns-filter {:allow #{"*"} :deny #{"taoensso.*"}},
            :middleware [],
            :timestamp-opts
            {:pattern "yyyy-MM-dd HH:mm:ss.SSS",
             :locale :jvm-default,
             :timezone :jvm-default},
            :hostname "calculon.home.active-group.de",
            :application nil}
           (active-config/normalize&check-config-object timbre-config/timbre-config-schema []
                                                        {:min-level [["*" :debug]]
                                                         :ns-filter {:allow #{"*"} :deny #{"taoensso.*"}}}))))
