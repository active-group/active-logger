(defproject de.active-group/active-logger "0.14.0"
  :description "Active Logger: Utilities and DSL for logging on top of Timbre."
  :url "http://github.com/active-group/active-logger"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.0" :scope "provided"]
                 [com.fzakaria/slf4j-timbre "0.3.21"]
                 [com.taoensso/timbre "6.6.1"]
                 [de.active-group/active-clojure "0.40.0"]
                 [de.active-group/active-quickcheck "0.8.0"]
                 [de.active-group/timbre-riemann "0.2.1"]
                 [cheshire "5.10.1"] ;; for logstash 3rd-party-appender of timbre
                 [riemann-clojure-client "0.5.4"]
                 [org.slf4j/log4j-over-slf4j "1.7.36"]
                 [org.slf4j/jul-to-slf4j     "1.7.36"]
                 [org.slf4j/jcl-over-slf4j   "1.7.36"]])
