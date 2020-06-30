(ns active.clojure.logger.context
  "Facilities for switching log contexts."
  (:require [taoensso.timbre :as timbre]))

(defmacro with-log-context!
  "Attach context to a log for the extent of the body."
  [?context & ?body]
  `(timbre/with-context (merge timbre/*context* (sanitize-context ~?context))
     ~@?body))

(defn current-log-context
  []
  timbre/*context*)
