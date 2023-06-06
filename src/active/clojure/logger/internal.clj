(ns active.clojure.logger.internal
  (:require [taoensso.timbre :as timbre]
            [active.clojure.condition-hooks :as condition-hooks]))

(defn log-event!-internal
  [type origin level context vargs-delay]
  (timbre/with-context (assoc (merge timbre/*context* context) :type type)
    (timbre/-log! timbre/*config* level origin nil nil :p nil vargs-delay nil)))

(defn log-exception-event!-internal
  [type origin level context vargs-delay e]
  (timbre/with-context (assoc (merge timbre/*context* context) :type type)
    (timbre/-log! timbre/*config* level origin nil nil :p e vargs-delay nil)))

(defmacro -log-event! [?level ?msg]
  `(log-event!-internal "event"
                        ~(str *ns*)
                        ~?level
                        nil
                        (delay [~?msg])))

;; dummy: but it needs to exist for event logging to be a service
(defn pr-exception-stacktrace [err]
  ;; expecially prints conditions more pretty
  (condition-hooks/print-stack-trace-of err))

