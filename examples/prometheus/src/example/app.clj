(ns example.app
  (:require [active.clojure.logger.metric :as metrics]
            [active.clojure.logger.metric-prometheus :as metric-prometheus]
            [org.httpkit.server :as http]))

(defn record-http-requests-total-handler
  "Middleware to record the total number of HTTP requests.
  
   This function wraps a given handler and logs the count of HTTP requests
   received. It logs the metric 'http_requests_total' with additional tags
   including `:uri` of the request and `:status` of the response.

   ## Example

   ```
   (defn my-handler [req]
     {:status 200 :headers {} :body \"Hello, World!\"})

   (def my-app
     (record-http-requests-total-handler my-handler))

   (http/run-server my-app {:port 8080})
   ;; 
   ```
   "
  [handler]
  (fn [req]
    (let [res (handler req)]
      (metrics/log-counter-metric! "http_requests_total"
                                   (merge
                                    {:uri (:uri req) :server "example_server"}
                                    (when-let [status (:status res)]
                                      {:status status}))
                                   1)
      res)))

(def app-wrapped-in-prometheus-handler
  "A Prometheus-wrapped Ring handler.

  Handles HTTP requests:
  - Returns a 200 status with 'Hello, World!' for the `/hello` path.
  - Returns a 404 status with 'not found' for other paths.

  Middleware:
  - Tracks total HTTP requests and integrates Prometheus metrics."

  (record-http-requests-total-handler
   (metric-prometheus/wrap-prometheus-metrics-ring-handler
    (fn [req]
      (cond
        (re-matches #"^/hello" (:uri req))
          {:status 200 :headers {"Content-Type" "text/html"} :body "<h1>Hello, World!</h1>"}
        :else
          {:status 404 :headers {"Content-Type" "text/plain"} :body "not found"})))))

(defn start!
  "Start the metrics handler prometheus instance. 
   
   In addition to all the other paths (`localhost:<port>/hello` in this example), metrics can be viewed at `localhost:<port>/metrics`.
   
   ## Example
       > (example/start! {:port 8080})"
  [opts]
  (http/run-server app-wrapped-in-prometheus-handler opts))

(defn -main [& _args]
  (start! {:port 8080}))
