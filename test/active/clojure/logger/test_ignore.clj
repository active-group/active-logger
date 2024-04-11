(ns active.clojure.logger.test-ignore
  (:require [active.clojure.logger.test :refer :all]
            [active.clojure.logger.event :as event]
            [clojure.test :refer :all]))

(use-fixtures :each log-ignore-test-fixture)

(deftest t-log-ignore-test-fixture
  (is (= "" (with-out-str (event/log-event! :info "This message should not show up")))))
