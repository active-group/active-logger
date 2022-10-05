(ns active.clojure.logger.timed-metric-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]
            [active.clojure.lens :as lens]
            [active.clojure.logger.event :as event]
            [active.clojure.logger.metric-emitter :as metric-emitter]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
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


;; Testing the simple logging

(defn reset-global-state-for-tests!
  [f]
  (metric-accumulator/reset-global-metric-store!)
  (metric-emitter/set-global-log-metrics-config! (metric-emitter/configure-metrics-logging :no-push))
  (f))

(use-fixtures :each reset-global-state-for-tests!)

(defmacro mock-run-monad
  [& ?args]
  `(reset-global-state-for-tests! (fn [] (mock-monad/mock-run-monad ~@?args))))

(def monad-command-config-gauges
  (monad/combine-monad-command-configs
   (monad/make-monad-command-config timed-metric/run-timed-metrics-as-gauges {} {})
   (monad/make-monad-command-config metric-accumulator/run-metrics {} {})
   metric-emitter/log-metrics-command-config))

(def monad-command-config-histograms
  (monad/combine-monad-command-configs
   (monad/make-monad-command-config timed-metric/run-timed-metrics-as-histograms {} {})
   (monad/make-monad-command-config metric-accumulator/run-metrics {} {})
   metric-emitter/log-metrics-command-config))

(deftest t-logging-timing
  (testing "Simple timing works: checking mocked results"
    (let [result (mock-monad/mock-run-monad
                   [(mock-monad/mock-result time/get-elapsed-time 10)
                    (mock-monad/mock-result time/get-elapsed-time 20)
                    (mock-monad/mock-result
                     (timed-metric/log-timed-metric
                      (timed-metric/make-timer-name "active.clojure.logger.timed-metric-test" "example-metric" {}) 10) nil)]
                   (timed-metric/logging-timing "example-metric" (monad/return nil)))]
      (is (nil? result))))

  (testing "Simple timing works with gauges: checking metric-store"
    (let [result (mock-run-monad
                  monad-command-config-gauges
                  [(mock-monad/mock-result time/get-elapsed-time 10)
                   (mock-monad/mock-result time/get-elapsed-time 20)
                   (mock-monad/mock-result time/get-milli-time 12345)]
                  (monad/monadic
                   (timed-metric/logging-timing "example-metric" (monad/return nil))
                   (metric-accumulator/get-all-metric-sample-sets)))]
      (is (= [(metric-accumulator/make-metric-sample-set "example-metric" :gauge "example-metric"
                                                         [(metric-accumulator/make-metric-sample "example-metric" {} 10 12345)])]
             result))))

  (testing "Simple timing works with histograms: checking metric-store"
    (let [result (mock-run-monad
                  monad-command-config-histograms
                  [(mock-monad/mock-result time/get-elapsed-time 10)
                   (mock-monad/mock-result time/get-elapsed-time 20)
                   (mock-monad/mock-result time/get-milli-time 12345)]
                  (monad/monadic
                   (timed-metric/logging-timing "example-metric" (monad/return nil))
                   (metric-accumulator/get-all-metric-sample-sets)))]
      (is (= [(metric-accumulator/make-metric-sample-set "example-metric" :histogram "example-metric"
                                                         [(metric-accumulator/make-metric-sample "example-metric_sum" {} 10 12345)
                                                          (metric-accumulator/make-metric-sample "example-metric_count" {} 1 12345)
                                                          (metric-accumulator/make-metric-sample "example-metric_bucket" {:le "+Inf"} 1 12345)])]
             result)))))
