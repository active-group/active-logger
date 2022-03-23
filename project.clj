(defproject de.active-group/active-logger "0.7.0-SNAPSHOT"
  :description "Active Logger: Utilities and DSL for logging on top of Timbre."
  :url "http://github.com/active-group/active-logger"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0" :scope "provided"]
                 [org.slf4j/slf4j-log4j12 "1.7.32"]
                 [com.taoensso/timbre "5.1.0"]
                 [de.active-group/active-clojure "0.40.0"]
                 [de.active-group/timbre-riemann "0.2.0"]
                 [cheshire "5.10.1"] ;; for logstash 3rd-party-appender of timbre
                 [riemann-clojure-client "0.5.1" :exclusions [org.slf4j/slf4j-api]]])
