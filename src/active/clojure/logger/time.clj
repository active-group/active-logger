(ns active.clojure.logger.time
  "Get current system time"
  (:require [active.clojure.monad :as monad]
            [active.clojure.record :refer [define-record-type]]))

(define-record-type ^{:doc "Get the system time in milliseconds"} GetMilliTime
  (make-get-milli-time) get-milli-time? [])

(def get-milli-time (make-get-milli-time))

(defn get-milli-time!
  []
  (System/currentTimeMillis))

(define-record-type ^{:doc "Get the elapsed time in milliseconds.  Only useful for precise measurments of elapsed time."} GetElapsedTime
  (make-get-elapsed-time) get-elapsed-time? [])

(def get-elapsed-time (make-get-elapsed-time))

(defn get-elapsed-time!
  []
  (/ (double (System/nanoTime)) 1000000.0))

(defn run-time
  [_run-any _env mstate m]
  (cond
    (get-milli-time? m)
    [(get-milli-time!) mstate]

    (get-elapsed-time? m)
    [(get-elapsed-time!) mstate]

    :else
    monad/unknown-command))

(def monad-command-config
  (monad/make-monad-command-config run-time {} {}))
