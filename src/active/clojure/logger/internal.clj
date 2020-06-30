(ns active.clojure.logger.internal
  (:require [taoensso.timbre :as timbre]))

(defn log-event!-internal
  [type origin level context vargs-delay]
  (timbre/with-context (assoc (merge timbre/*context* context) :type type)
    (timbre/-log! timbre/*config* level origin nil nil :p nil vargs-delay nil)))

(defn log-exception-event!-internal
  [type origin level context vargs-delay e]
  (timbre/with-context (assoc (merge timbre/*context* context) :type type)
    (timbre/-log! timbre/*config* level origin nil nil :p e vargs-delay nil)))

(defn sanitize-context
  "Make sure the context only contains string values."
  [mp]
  (if (some (fn [entry]
              (not (string? (val entry))))
            mp)
    (into {}
          (filter (fn [entry]
                    (if (string? (val entry))
                      true
                      (binding [*out* *err*]
                        (println (str "WARNING: log context contains non-string value for key " (key entry) " (" (val entry) ")"))
                        false)))
                  mp))
    mp))
