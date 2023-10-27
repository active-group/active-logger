(ns active.clojure.logger.metric-test
  (:require [active.clojure.logger.metric :as m]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-emitter :as metric-emitter]
            [clojure.test :as t]
            [active.clojure.monad :as monad]
            [active.clojure.mock-monad :as mock-monad]

            [clojure.spec.test.alpha :as stest]
            [active.clojure.logger.metric-emitter :as metric-emitter]
            [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.test-utils :as test-utils])
  (:use [active.quickcheck]))

(defn reset-global-state-for-tests!
  [f]
  (metric-accumulator/reset-global-metric-store!)
  (metric-emitter/set-global-log-metrics-config! (metric-emitter/configure-metrics-logging :no-push))
  (f))

(t/use-fixtures :each reset-global-state-for-tests!)

(defmacro mock-run-monad
  [& ?args]
  `(reset-global-state-for-tests! (fn [] (mock-monad/mock-run-monad ~@?args))))

(t/use-fixtures :once
  (fn [f]
    (stest/instrument)
    (f)
    (stest/unstrument)))

(t/deftest t-log-metric!-internal
  (m/log-metric!-internal (str *ns*) (metric-accumulator/make-gauge-metric "name" "help") {:label "a"} 23 {:context "b"})
    (test-utils/is-metric-set-stored? "name" :gauge "help")
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-metric-internal
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-metric-internal (str *ns*) (metric-accumulator/make-gauge-metric "name" "help") {:label "a"} 23 {:context "b"})
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-log-metric!-2
  (m/log-metric! (metric-accumulator/make-gauge-metric "name" "help") {:label "a"} 23)
  (test-utils/is-metric-set-stored? "name" :gauge "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-metric!-3
  (m/log-metric! (metric-accumulator/make-gauge-metric "name" "help") {:label "a"} 23 {:context "b"})
  (test-utils/is-metric-set-stored? "name" :gauge "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-metric!-4
  (m/log-metric! (metric-accumulator/make-gauge-metric "name" "help") {:label "a"} 23 {:context "b"} (str *ns*))
  (test-utils/is-metric-set-stored? "name" :gauge "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))


(t/deftest t-log-metric-2
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-metric (metric-accumulator/make-gauge-metric "name" "help") {:label "a"} 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0)))

(t/deftest t-log-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-metric (metric-accumulator/make-gauge-metric "name" "help") {:label "a"} 23 {:context "b"})
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0)))

(t/deftest t-log-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-metric (metric-accumulator/make-gauge-metric "name" "help") {:label "a"} 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0)))

(t/deftest t-log-gauge-metric!-2
  (m/log-gauge-metric! "name" 23)
  (test-utils/is-metric-set-stored? "name" :gauge "name")
  (test-utils/is-metric-stored? "name" {} 23.0))

(t/deftest t-log-gauge-metric!-3
  (m/log-gauge-metric! "name" {:label "a"} 23)
  (test-utils/is-metric-set-stored? "name" :gauge "name")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-gauge-metric!-4
  (m/log-gauge-metric! "name" {:label "a"} "help" 23)
  (test-utils/is-metric-set-stored? "name" :gauge "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-gauge-metric!-5
  (m/log-gauge-metric! "name" {:label "a"} "help" 23 {:context "b"})
  (test-utils/is-metric-set-stored? "name" :gauge "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-gauge-metric!-6
  (m/log-gauge-metric! "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
  (test-utils/is-metric-set-stored? "name" :gauge "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-gauge-metric-2
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "name" result)
    (test-utils/is-metric-stored? "name" {} 23.0 result)))

(t/deftest t-log-gauge-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" {:label "a"} 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "name" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-log-gauge-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" {:label "a"} "help" 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-log-gauge-metric-5
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" {:label "a"} "help" 23 {:context "b"})
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-log-gauge-metric-6
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-gauge-metric "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :gauge "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-log-counter-metric!-2
  (m/log-counter-metric! "name" 23)
  (test-utils/is-metric-set-stored? "name" :counter "name")
  (test-utils/is-metric-stored? "name" {} 23.0))

(t/deftest t-log-counter-metric!-3
  (m/log-counter-metric! "name" {:label "a"} 23)
  (test-utils/is-metric-set-stored? "name" :counter "name")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-counter-metric!-4
  (m/log-counter-metric! "name" {:label "a"} "help" 23)
  (test-utils/is-metric-set-stored? "name" :counter "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-counter-metric!-5
  (m/log-counter-metric! "name" {:label "a"} "help" 23 {:context "b"})
  (test-utils/is-metric-set-stored? "name" :counter "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-counter-metric!-6
  (m/log-counter-metric! "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
  (test-utils/is-metric-set-stored? "name" :counter "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 23.0))

(t/deftest t-log-counter-metric-2
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "name" result)
    (test-utils/is-metric-stored? "name" {} 23.0 result)))

(t/deftest t-log-counter-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" {:label "a"} 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "name" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-log-counter-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" {:label "a"} "help" 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-log-counter-metric-5
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" {:label "a"} "help" 23 {:context "b"})
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-log-counter-metric-6
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-counter-metric "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 23.0 result)))

(t/deftest t-set-counter-metric!-2
  (m/set-counter-metric! "name" 23)
  (m/set-counter-metric! "name" 42)
  (test-utils/is-metric-set-stored? "name" :counter "name")
  (test-utils/is-metric-stored? "name" {} 42.0))

(t/deftest t-set-counter-metric!-3
  (m/set-counter-metric! "name" {:label "a"} 23)
  (m/set-counter-metric! "name" {:label "a"} 42)
  (test-utils/is-metric-set-stored? "name" :counter "name")
  (test-utils/is-metric-stored? "name" {:label "a"} 42.0))

(t/deftest t-set-counter-metric!-4
  (m/set-counter-metric! "name" {:label "a"} "help" 23)
  (m/set-counter-metric! "name" {:label "a"} "help" 42)
  (test-utils/is-metric-set-stored? "name" :counter "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 42.0))

(t/deftest t-set-counter-metric!-5
  (m/set-counter-metric! "name" {:label "a"} "help" 23 {:context "b"})
  (m/set-counter-metric! "name" {:label "a"} "help" 42 {:context "b"})
  (test-utils/is-metric-set-stored? "name" :counter "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 42.0))

(t/deftest t-set-counter-metric!-6
  (m/set-counter-metric! "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
  (m/set-counter-metric! "name" {:label "a"} "help" 42 {:context "b"} (str *ns*))
  (test-utils/is-metric-set-stored? "name" :counter "help")
  (test-utils/is-metric-stored? "name" {:label "a"} 42.0))

(t/deftest t-set-counter-metric-2
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/set-counter-metric "name" 23)
                 (m/set-counter-metric "name" 42)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "name" result)
    (test-utils/is-metric-stored? "name" {} 42.0 result)))

(t/deftest t-set-counter-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/set-counter-metric "name" {:label "a"} 23)
                 (m/set-counter-metric "name" {:label "a"} 42)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "name" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 42.0 result)))

(t/deftest t-set-counter-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/set-counter-metric "name" {:label "a"} "help" 23)
                 (m/set-counter-metric "name" {:label "a"} "help" 42)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 42.0 result)))

(t/deftest t-set-counter-metric-5
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/set-counter-metric "name" {:label "a"} "help" 23 {:context "b"})
                 (m/set-counter-metric "name" {:label "a"} "help" 42 {:context "b"})
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 42.0 result)))

(t/deftest t-set-counter-metric-6
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/set-counter-metric "name" {:label "a"} "help" 23 {:context "b"} (str *ns*))
                 (m/set-counter-metric "name" {:label "a"} "help" 42 {:context "b"} (str *ns*))
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :counter "help" result)
    (test-utils/is-metric-stored? "name" {:label "a"} 42.0 result)))

(t/deftest t-log-histogram-metric!-2
  (m/log-histogram-metric! "name" 23)
  (test-utils/is-metric-set-stored? "name" :histogram "name")
  (test-utils/is-metric-stored? "name_sum" {} 23.0)
  (test-utils/is-metric-stored? "name_count" {} 1.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "+Inf"} 1.0))

(t/deftest t-log-histogram-metric!-3
  (m/log-histogram-metric! "name" [20] 23)
  (test-utils/is-metric-set-stored? "name" :histogram "name")
  (test-utils/is-metric-stored? "name_sum" {} 23.0)
  (test-utils/is-metric-stored? "name_count" {} 1.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "20"} 0.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "+Inf"} 1.0))

(t/deftest t-log-histogram-metric!-4
  (m/log-histogram-metric! "name" [20] {:label "a"} 23)
  (test-utils/is-metric-set-stored? "name" :histogram "name")
  (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0)
  (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "20" :label "a"} 0.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0))

(t/deftest t-log-histogram-metric!-5
  (m/log-histogram-metric! "name" [20] {:label "a"} "help" 23)
  (test-utils/is-metric-set-stored? "name" :histogram "help")
  (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0)
  (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "20" :label "a"} 0.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0))

(t/deftest t-log-histogram-metric!-6
  (m/log-histogram-metric! "name" [20] {:label "a"} "help" 23 {:context "b"})
  (test-utils/is-metric-set-stored? "name" :histogram "help")
  (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0)
  (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "20" :label "a"} 0.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0))

(t/deftest t-log-histogram-metric!-7
  (m/log-histogram-metric! "name" [20] {:label "a"} "help" 23 {:context "b"} (str *ns*))
  (test-utils/is-metric-set-stored? "name" :histogram "help")
  (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0)
  (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "20" :label "a"} 0.0)
  (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0))

(t/deftest t-log-histogram-metric-2
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" [] 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :histogram "name" result)
    (test-utils/is-metric-stored? "name_sum" {} 23.0 result)
    (test-utils/is-metric-stored? "name_count" {} 1.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "+Inf"} 1.0 result)))

(t/deftest t-log-histogram-metric-3
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" [20] 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :histogram "name" result)
    (test-utils/is-metric-stored? "name_sum" {} 23.0 result)
    (test-utils/is-metric-stored? "name_count" {} 1.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "20"} 0.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "+Inf"} 1.0 result)))

(t/deftest t-log-histogram-metric-4
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" [20] {:label "a"} 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :histogram "name" result)
    (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0 result)
    (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "20" :label "a"} 0.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0 result)))

(t/deftest t-log-histogram-metric-5
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" [20] {:label "a"} "help" 23)
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :histogram "help" result)
    (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0 result)
    (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "20" :label "a"} 0.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0 result)))

(t/deftest t-log-histogram-metric-6
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" [20] {:label "a"} "help" 23 {:context "b"})
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :histogram "help" result)
    (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0 result)
    (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "20" :label "a"} 0.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0 result)))

(t/deftest t-log-histogram-metric-7
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" [20] {:label "a"} "help" 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :histogram "help" result)
    (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0 result)
    (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "20" :label "a"} 0.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0 result)))

(t/deftest t-log-histogram-metric-no-buckets
  (let [result (mock-run-monad
                m/monad-command-config
                []
                (monad/monadic
                 (m/log-histogram-metric "name" [] {:label "a"} "help" 23 {:context "b"} (str *ns*))
                 (metric-accumulator/get-all-metric-sample-sets)))]
    (test-utils/is-metric-set-stored? "name" :histogram "help" result)
    (test-utils/is-metric-stored? "name_sum" {:label "a"} 23.0 result)
    (test-utils/is-metric-stored? "name_count" {:label "a"} 1.0 result)
    (test-utils/is-metric-stored? "name_bucket" {:le "+Inf" :label "a"} 1.0 result)))
