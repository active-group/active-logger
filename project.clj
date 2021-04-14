(defproject de.active-group/active-logger "0.4.0"
  :description "Active Logger: Utilities and DSL for logging on top of Timbre."
  :url "http://github.com/active-group/active-logger"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [;[org.clojure/clojure "1.9.0"]
                 ;[org.slf4j/slf4j-log4j12 "1.7.9"] ;; breaks things when included in a project where log4j is already included from somewhere else?!
                 [com.taoensso/timbre "4.7.0"]
                 [de.active-group/active-clojure "0.35.0"]
                 [de.active-group/timbre-riemann "0.2.0"]
                 [cheshire "5.8.0"] ;; for logstash 3rd-party-appender of timbre
                 [riemann-clojure-client "0.5.1"]])
