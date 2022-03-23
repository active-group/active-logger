(ns example.app
  (:require [active.clojure.logger.event :as event-logger]))

(defn -main [& _args]
  (event-logger/log-event! :info "We are live!"))
