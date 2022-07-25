(ns active.clojure.logger.metric-test
  (:require [active.clojure.logger.metric :as m]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-emitter :as metric-emitter]
            [clojure.test :as t]
            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]

            [clojure.spec.test.alpha :as stest]
            [active.clojure.logger.metric-emitter :as metric-emitter]
            [active.clojure.logger.metric-accumulator :as metric-accumulator])
  (:use [active.quickcheck]))

(defn reset-global-state-for-tests!
  [f]
  (metric-accumulator/reset-global-raw-metric-store!)
  (metric-emitter/set-global-log-metrics-config! (metric-emitter/configure-metrics-logging {} :no-push))
  (f))

(t/use-fixtures :each reset-global-state-for-tests!)

(defmacro mock-run-monad
  [& ?args]
  `(reset-global-state-for-tests! (fn [] (mock-monad/mock-run-monad ~@?args))))

(defn strip-timestamps
  [samples]
  (mapv #(metric-accumulator/metric-sample-timestamp % 0) samples))

(stest/instrument)

(t/deftest t-log-metric!-internal
  (m/log-metric!-internal (str *ns*) (metric-accumulator/make-gauge-metric "name" "help" {:label "a"}) 23 {:context "b"})
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-metric-internal
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-metric-internal (str *ns*) (metric-accumulator/make-gauge-metric "name" "help" {:label "a"}) 23 {:context "b"})
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-metric!-2
  (m/log-metric! (metric-accumulator/make-gauge-metric "name" "help" {:label "a"}) 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-metric!-3
  (m/log-metric! (metric-accumulator/make-gauge-metric "name" "help" {:label "a"}) 23 {:context "b"})
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-metric!-4
  (m/log-metric! (metric-accumulator/make-gauge-metric "name" "help" {:label "a"}) 23 {:context "b"} (str *ns*))
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))


(t/deftest t-log-metric-2
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-metric (metric-accumulator/make-gauge-metric "name" "help" {:label "a"}) 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-metric (metric-accumulator/make-gauge-metric "name" "help" {:label "a"}) 23 {:context "b"})
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-metric (metric-accumulator/make-gauge-metric "name" "help" {:label "a"}) 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-gauge-metric!-2
  (m/log-gauge-metric! "name" 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name" {} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-gauge-metric!-3
  (m/log-gauge-metric! "name" {:label "a"} 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-gauge-metric!-4
  (m/log-gauge-metric! "name" {:label "a"} "help" 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-gauge-metric!-5
  (m/log-gauge-metric! "name" {:label "a"} "help" 23 {:context "b"})
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-gauge-metric!-6
  (m/log-gauge-metric! "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-gauge-metric-2
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-gauge-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" {:label "a"} 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-gauge-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" {:label "a"} "help" 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-gauge-metric-5
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" {:label "a"} "help" 23 {:context "b"})
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-gauge-metric-6
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-counter-metric!-2
  (m/log-counter-metric! "name" 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name" {} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-counter-metric!-3
  (m/log-counter-metric! "name" {:label "a"} 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-counter-metric!-4
  (m/log-counter-metric! "name" {:label "a"} "help" 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-counter-metric!-5
  (m/log-counter-metric! "name" {:label "a"} "help" 23 {:context "b"})
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-counter-metric!-6
  (m/log-counter-metric! "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
  (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-counter-metric-2
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-counter-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" {:label "a"} 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-counter-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" {:label "a"} "help" 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-counter-metric-5
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" {:label "a"} "help" 23 {:context "b"})
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-counter-metric-6
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name" {:label "a"} 23 0)]
             (strip-timestamps result)))))

(t/deftest t-log-histogram-metric!-3
  (m/log-histogram-metric! "name" 20 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {} 23 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:le "+Inf"} 1 0)
            (metric-accumulator/make-metric-sample "name_count" {} 1 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:le "20"} 0 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-histogram-metric!-4
  (m/log-histogram-metric! "name" 20 {:label "a"} 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
            (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-histogram-metric!-5
  (m/log-histogram-metric! "name" 20 {:label "a"} "help" 23)
  (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
            (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-histogram-metric!-6
  (m/log-histogram-metric! "name" 20 {:label "a"} "help" 23 {:context "b"})
  (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
            (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-histogram-metric!-7
  (m/log-histogram-metric! "name" 20 {:label "a"} "help" 23 {:context "b"} (str *ns*))
  (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
            (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
            (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)]
           (strip-timestamps (metric-accumulator/get-metric-samples!)))))

(t/deftest t-log-histogram-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" 20 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {} 23 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:le "+Inf"} 1 0)
              (metric-accumulator/make-metric-sample "name_count" {} 1 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:le "20"} 0 0)]
             (strip-timestamps result)))))

(t/deftest t-log-histogram-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" 20 {:label "a"} 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
              (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)]
             (strip-timestamps result)))))

(t/deftest t-log-histogram-metric-5
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" 20 {:label "a"} "help" 23)
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
              (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)]
             (strip-timestamps result)))))

(t/deftest t-log-histogram-metric-6
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" 20 {:label "a"} "help" 23 {:context "b"})
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
              (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)]
             (strip-timestamps result)))))

(t/deftest t-log-histogram-metric-7
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" 20 {:label "a"} "help" 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-metric-samples)))]
    (t/is (= [(metric-accumulator/make-metric-sample "name_sum" {:label "a"} 23 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "+Inf"} 1 0)
              (metric-accumulator/make-metric-sample "name_count" {:label "a"} 1 0)
              (metric-accumulator/make-metric-sample "name_bucket" {:label "a" :le "20"} 0 0)]
             (strip-timestamps result)))))
