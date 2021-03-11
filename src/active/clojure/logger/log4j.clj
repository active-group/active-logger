(ns active.clojure.logger.log4j
  (:require [active.clojure.logger.event :as event])
  (:import [org.apache.log4j Logger AppenderSkeleton Level]
           [org.apache.log4j.spi LoggingEvent]))

(defn- log4j-level->level
  "Convert a log4j level to a keyword suitable here."
  [level]
  (cond
    (= level Level/DEBUG) :debug
    (= level Level/ERROR) :error
    (= level Level/FATAL) :fatal
    (= level Level/INFO)  :info
    (= level Level/TRACE) :trace
    (= level Level/WARN)  :warn))

(defn- make-log4j-appender
  []
  (proxy [AppenderSkeleton] []
    (append [^LoggingEvent event]
      (event/log-event! (log4j-level->level (.getLevel event)) (.getMessage event)))
    (close [] nil)
    (requiresLayout [] false)))

(defn redirect-log4j!
  "Redirects log4j output to event logging.

  Removes all other log4j appenders."
  []
  (let [root (Logger/getRootLogger)]
    ;; zap all existing appenders
    (.resetConfiguration (.getLoggerRepository root))
    (.setLevel root Level/INFO)
    (.addAppender root (make-log4j-appender))))
