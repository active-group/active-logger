(ns active.clojure.logger.metric-prometheus-util-test
  (:require [active.clojure.logger.metric-prometheus-util :as sut]
            [clojure.test :as t]))

(def cleanup-non-prometheus-label-characters
  (sut/make-cleanup-non-prometheus-label-characters))

(def render-labels
  (sut/make-render-labels
   cleanup-non-prometheus-label-characters))

(t/deftest t-cleanup-non-prometheus-label-characters
  (t/is (= "valid_name" (cleanup-non-prometheus-label-characters "valid name")))
  (t/is (= "_1_valid_name_" (cleanup-non-prometheus-label-characters "1 valid-name:"))))

(t/deftest t-render-labels
  (t/is (= "{description_short=\"Station [Frontside] failed to inspect wafer... An error has occurred in \\\"load()\\\" with the following details\"}"
           (render-labels {"description-short" "Station [Frontside] failed to inspect wafer... An error has occurred in \"load()\" with the following details"})))
  (t/is (= "{description_short=\"\"}"
           (render-labels {"description-short" nil})))
  (t/is (#{"{description_short=\"23\",other=\"val\"}"
           "{other=\"val\",description_short=\"23\"}"}
         (render-labels {"description-short" 23 "other" "val"}))))
