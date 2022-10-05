(ns active.clojure.logger.timed-metric-test
  (:require [clojure.test :refer [deftest is testing]]
            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]
            [active.clojure.logger.event :as event]
            [active.clojure.logger.time :as time]
            [active.clojure.logger.timed-metric :as timed-metric]))

(defn float= [x y]
  (<= (Math/abs (double (- x y))) 0.00001))

(defn ignore-log-event
  [run-any env state m]
  (cond
    (event/with-log-context? m)
    (run-any env state (event/with-log-context-body m))

    (event/log-event? m)
    [nil state]

    :else monad/unknown-command))

(def ignore-log-event-command-config
  (monad/make-monad-command-config ignore-log-event {} {}))

(deftest t-start-stop
  (testing "Timing works"
    (let [result
          (mock-monad/mock-run-monad
           ignore-log-event-command-config
           [(mock-monad/mock-result time/get-elapsed-time 10)
            (mock-monad/mock-result time/get-elapsed-time 20)]
           (monad/monadic
            [name (timed-metric/start-metric-timer "test metric" :id-1)]
            (timed-metric/stop-metric-timer name)))]
      (is (float= result 10))))

  (testing "Stopping a timer that has not been started causes a log error."
    (let [result
          (mock-monad/mock-run-monad
           ignore-log-event-command-config
           [(mock-monad/mock-result time/get-elapsed-time nil)]
           (monad/monadic
            (timed-metric/stop-and-log-metric-timer "test metric")))]
      (is (nil? result))))

  (testing "Stopping a timer causes it to get removed"
    (let [result
          (mock-monad/mock-run-monad
           ignore-log-event-command-config
           [(mock-monad/mock-result time/get-elapsed-time 10)
            (mock-monad/mock-result time/get-elapsed-time 20)
            (mock-monad/mock-result time/get-elapsed-time nil)]
           (monad/monadic
            [name (timed-metric/start-metric-timer "test metric" :id-1)]
            (timed-metric/stop-metric-timer name)
            (timed-metric/stop-metric-timer name)))]
      (is (nil? result))))

  (testing "Cancelling a timer causes it to get removed"
    (let [result
          (mock-monad/mock-run-monad
           ignore-log-event-command-config
           [(mock-monad/mock-result time/get-elapsed-time 10)
            (mock-monad/mock-result time/get-elapsed-time nil)]
           (monad/monadic
            [name (timed-metric/start-metric-timer "test metric" :id-1)]
            (timed-metric/cancel-metric-timer "test metric" :id-1)
            (timed-metric/stop-and-log-metric-timer "test metric" :id-1)))]
      (is (nil? result))))

  (testing "Stopping and logging a timer causes it to get logged"
    (with-bindings {#'*ns* *ns*}
      (let [result
            (mock-monad/mock-run-monad
             ignore-log-event-command-config
             [(mock-monad/mock-result time/get-elapsed-time 10)
              (mock-monad/mock-result time/get-elapsed-time 20)
              (mock-monad/mock-result (timed-metric/log-timed-metric (timed-metric/make-timer-name "active.clojure.logger.timed-metric-test" "test metric" :id-1) 10) nil)]
             (monad/monadic
              [name (timed-metric/start-metric-timer "test metric" :id-1)]
              (timed-metric/stop-and-log-metric-timer name)))]
        (is (= 10 result))))))
