(ns active.clojure.logger.metric-prometheus-util-test
  (:require [active.clojure.logger.metric-prometheus-util :as sut]
            [clojure.test :as t]))

(t/deftest t-cleanup-non-prometheus-label-characters
  (t/is (= "valid_name" (sut/cleanup-non-prometheus-label-characters "valid name")))
  (t/is (= "_1_valid_name_" (sut/cleanup-non-prometheus-label-characters "1 valid-name:"))))

(t/deftest t-render-label
  (t/is (= "description_short=\"Station [Frontside] failed to inspect wafer... An error has occurred in \\\"load()\\\" with the following details\""
           (sut/render-label "description-short" "Station [Frontside] failed to inspect wafer... An error has occurred in \"load()\" with the following details")))
  (t/is (= "description_short=\"\""
           (sut/render-label "description-short" nil)))
  (t/is (= "description_short=\"23\""
           (sut/render-label "description-short" 23))))
