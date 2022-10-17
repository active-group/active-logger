(ns active.clojure.logger.timed-metric
  "Common functionality to ease logging of timed metrics."
  (:require [active.clojure.logger.event :as event]
            [active.clojure.logger.metric :as metric]
            [active.clojure.logger.time :as time]
            [active.clojure.monad :as monad]
            [active.clojure.record :refer [define-record-type]]))

;; Simple timing

(define-record-type TimerName
  (make-timer-name namespace metric map)
  timer-name?
  [^{:doc "String"}
   namespace timer-name-namespace
   ^{:doc "Name of the metric that will be logged."}
   metric timer-name-metric
   ^{:doc "Map with log context info, keywords to strings."}
   map timer-name-map])

(define-record-type LogTimedMetric
  ^{:doc "Monadic command for logging a timed metric."}
  log-timed-metric
  log-timed-metric?
  [timer-name log-timed-metric-timer-name
   timer log-timed-metric-timer])

(defmacro log-time-metric!
  [?log-metric-fn! ?body]
  `(let [s# (time/get-milli-time!)
        r# ~?body
        e# (time/get-milli-time!)]
     (~?log-metric-fn! (- e# s#))
    r#))

(declare start-metric-timer-1)
(declare stop-and-log-metric-timer-1)

(defn logging-timing*
  ([origin metric m]
   (logging-timing* origin metric {} m))
  ([origin metric more m]
   (monad/monadic
    ;;                     TimerName: ns metric map
    [timer-name (start-metric-timer-1 origin metric more)
     r m
     _ (stop-and-log-metric-timer-1 timer-name)]
    (monad/return r))))

(defmacro logging-timing
  "A monadic command that executes `m` and returns its result, and
  also makes a debug log message about the time it took to execute
  it, where the message contains the given `metric` as name and
  help string and `more`."
  ([?metric ?m]
  `(logging-timing* ~(str *ns*) ~?metric ~?m))
  ([?metric ?more ?m]
  `(logging-timing* ~(str *ns*) ~?metric ~?more ~?m)))

;; More sophisticated API

(defn start-metric-timer-1
  "Starts a metric timer, identified by `ns`, `metric` and `more`.

  `more` is a map that is merged with the log context that's already active,
  if present."
  [ns metric more]
  (monad/monadic
   [time time/get-elapsed-time]
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
   [end-time time/get-elapsed-time]
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
      (log-timed-metric timer-name timer)
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

(defn run-timed-metrics-as-gauges
  [run-any env state m]
  (cond
    (log-timed-metric? m)
    (let [timer-name (log-timed-metric-timer-name m)
          timer (log-timed-metric-timer m)]
      (run-any env state
               ;; use label as metric's name and help string
               (metric/log-gauge-metric (timer-name-metric timer-name) (timer-name-map timer-name) (timer-name-metric timer-name) timer nil (timer-name-namespace timer-name))))

    :else
      monad/unknown-command))

(def timed-metrics-as-gauges-command-config
  (monad/combine-monad-command-configs
   (monad/make-monad-command-config
    run-timed-metrics-as-gauges
    {} {})
   time/monad-command-config
   metric/monad-command-config))

(defn run-timed-metrics-as-histograms
  [run-any env state m]
  (cond
    (log-timed-metric? m)
    (let [timer-name (log-timed-metric-timer-name m)
          timer (log-timed-metric-timer m)]
      (run-any env state
               ;; use label as metric's name and help string
               (metric/log-histogram-metric (timer-name-metric timer-name) [] (timer-name-map timer-name) (timer-name-metric timer-name) timer nil (timer-name-namespace timer-name))))

    :else
    monad/unknown-command))

(def timed-metrics-as-histograms-command-config
  (monad/combine-monad-command-configs
   (monad/make-monad-command-config
    run-timed-metrics-as-histograms
    {} {})
   time/monad-command-config
   metric/monad-command-config))
