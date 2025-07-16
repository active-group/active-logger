(ns active.clojure.logger.metric-prometheus-util-test
  (:require [active.clojure.logger.metric-prometheus-util :as sut]
            [clojure.test :as t]))

(t/deftest t-cleanup-non-prometheus-label-characters
  (t/is (= "valid_name" (sut/cleanup-non-prometheus-label-characters "valid name")))
  (t/is (= "_1_valid_name_" (sut/cleanup-non-prometheus-label-characters "1 valid-name:"))))
