(ns active.clojure.logger.timed-metric
  "Common functionality to ease logging of timed metrics."
  (:require [active.clojure.logger.event :as event]
            [active.clojure.logger.metric :as metric]
            [active.clojure.logger.time :as time]
            [active.clojure.monad :as monad]
            [active.clojure.record :refer :all]))

;; Simple timing

(defn logging-timing*
  [origin label m]
  (monad/monadic
   [st time/get-milli-time
    r m
    e time/get-milli-time]
   ;; use label as metric's name and help string
   (metric/log-gauge-metric label nil label (- e st) nil origin)
   (monad/return r)))

(defmacro logging-timing
  "A monadic command that executes `m` and returns its result, and
  also makes a debug log messages about the time it took to execute
  it, where the message begins with the given `text`."
  [?label ?m]
  `(logging-timing* ~(str *ns*) ~?label ~?m))

;; More sophisticated API

(define-record-type TimerName
  (make-timer-name namespace metric map)
  timer-name?
  [^{:doc "String"}
   namespace timer-name-namespace
   ^{:doc "Name of the metric that will be logged."}
   metric timer-name-metric
   ^{:doc "Map with log context info, keywords to strings."}
   map timer-name-map])

(defn start-metric-timer-1
  "Starts a metric timer, identified by `ns`, `metric` and `more`."
  [ns metric more]
  (monad/monadic
   [time time/get-milli-time]
   (let [name (make-timer-name ns metric more)])
   (monad/update-state-component! ::timers
                                  (fn [timers]
                                    (assoc timers name time)))
   (monad/return name)))

(defn cancel-metric-timer-1
  "Cancels a metric timer, identified by `timer-name`."
  [timer-name]
  (monad/update-state-component! ::timers
                                 (fn [timers]
                                    (dissoc timers timer-name))))

(defn stop-metric-timer
  [timer-name]
  (monad/monadic
   [end-time time/get-milli-time]
   [timers (monad/get-state-component ::timers)]
   (let [start-time (get timers timer-name)])
   (if start-time
     (monad/monadic
      (monad/update-state-component! ::timers
                                     (fn [timers]
                                       (dissoc timers timer-name)))
      (monad/return (- end-time start-time)))
     (monad/monadic
      (event/log-event* :error (event/log-msg "metric timer has not been started"
                                            (timer-name-namespace timer-name) (timer-name-metric timer-name) (timer-name-map timer-name))
                        (timer-name-map timer-name))
      (monad/return nil)))))

(defn stop-and-log-metric-timer-1
  "Stops a metric timer and logs the time as a metric."
  [timer-name]
  (monad/monadic
   [timer (stop-metric-timer timer-name)]
   (if timer
     (monad/monadic
       ;; use label as metric's name and help string
      (metric/log-gauge-metric (timer-name-metric timer-name) (timer-name-map timer-name) (timer-name-metric timer-name) timer nil (timer-name-namespace timer-name))
      (monad/return timer))
     (monad/return nil))))

(defmacro start-metric-timer
  "Starts a metric timer, identified by `metric` and `more`.

  `more` is a map that is merged with the log context that's already active,
  if present.

  Returns a timer-name object that can be fed to
  [[cancel-metric-timer]] or [[stop-and-log-metric-timer]]."
  [?metric & [?more]]
  `(start-metric-timer-1 ~(str *ns*) ~?metric ~?more))

(defmacro cancel-metric-timer
  "Cancels a metric timer.  It can either be identified by a timer name
  returned by [[start-metric-timer]] or by current namespace, metric, and
  (optional) addtional context map."
  ([?timer-name-or-metric]
   `(let [thing# ~?timer-name-or-metric]
      (if (timer-name? thing#)
        (cancel-metric-timer-1 thing#)
        (cancel-metric-timer-1 (make-timer-name ~(str *ns*) thing# nil)))))
  ([?metric ?more]
   `(cancel-metric-timer-1 (make-timer-name ~(str *ns*) ~?metric ~?more))))

(defmacro stop-and-log-metric-timer
  "Stops a metric timer and logs the time.  The timer can either be identified by a timer name
  returned by [[start-metric-timer]] or by current namespace, metric, and
  (optional) addtional context map.

  Returns the elapsed time in milliseconds."
  ([?timer-name-or-metric]
   `(let [thing# ~?timer-name-or-metric]
      (if (timer-name? thing#)
        (stop-and-log-metric-timer-1 thing#)
        (stop-and-log-metric-timer-1 (make-timer-name ~(str *ns*) thing# nil)))))
  ([?metric ?more]
   `(stop-and-log-metric-timer-1 (make-timer-name ~(str *ns*) ~?metric ~?more))))
