(ns ^:no-doc active.clojure.logger.metric-singular-value
  "Internal.

  Stores a single double value together with a last update time."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.logger.metric-types :as metric-types]
            
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

;; TODO: rename 'singularvalue'
(define-record-type ^{:doc "Metric value is a combination of the `value` itself
  and the `last-update-time-ms` of the value."}
  MetricValue
  ^:private really-make-metric-value
  metric-value?
  [value               metric-value-value
   last-update-time-ms metric-value-last-update-time-ms])


;; https://prometheus.io/docs/instrumenting/writing_exporters/
;; "You should not set timestamps on the metrics you expose, let Prometheus
;; take care of that."

(s/def ::metric-value-value-double (s/and double? #(not (Double/isNaN %))))

(s/def ::metric-value
  (s/spec
   (partial instance? MetricValue)
   :gen (fn []
          (sgen/fmap (fn [[metric-value-value-double metric-value-last-update-time-ms]]
                       (really-make-metric-value metric-value-value-double metric-value-last-update-time-ms))
                     (s/gen (s/tuple ::metric-value-value-double ::metric-types/metric-last-update-time-ms))))))

;; stores value as double
(s/fdef make-metric-value
  :args (s/cat :value          ::metric-types/metric-value
               :update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret  ::metric-value)
(defn make-metric-value
  [value update-time-ms]
  (really-make-metric-value (double value) update-time-ms))

(s/fdef inc-metric-value
  :args (s/cat :mv  ::metric-value
               :value    ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::metric-value)
(defn inc-metric-value
  "Increments an existing value, by a new singular value."
  [mv value last-update-time-ms]
  (make-metric-value (+ (metric-value-value mv) value)
                     last-update-time-ms))
