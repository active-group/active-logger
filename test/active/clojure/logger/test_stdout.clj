(ns active.clojure.logger.test-stdout
  (:require [active.clojure.logger.test :refer :all]
            [active.clojure.logger.event :as event]
            [clojure.test :refer :all]))

(use-fixtures :each log-stdout-test-fixture)

(deftest t-log-stdout-test-fixture
  (is (some? (re-find #"This message should show up"
                      (with-out-str (event/log-event! :info "This message should show up"))))))
