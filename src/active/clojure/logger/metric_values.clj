(ns ^:no-doc active.clojure.logger.metric-values
  "Internal.

  Functions to store and access data suitable for a metric, per labels."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]

            [active.clojure.logger.metric-singular-value :as singular]
            [active.clojure.logger.metric-histogram-value :as histogram]

            [active.clojure.logger.metric-types :as metric-types]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]))

;; -----------------------------------------------------------------

;; Maps from labels to value hold the current state of the metric

(define-record-type ^:private StoredValues
  (make-stored-values map) ;; a map from labels to a ref of singular or histogram values
  stored-values?
  [map stored-values-map])

(defn- make-empty-stored-values []
  (make-stored-values {}))

(s/def ::stored-values
  (s/spec stored-values?))

;; -----------------------------------------------------------------

(s/fdef stale?
  :args (s/cat :last-update-time-ms ::metric-types/metric-last-update-time-ms
               :time-ms ::metric-types/metric-last-update-time-ms)
  :ret boolean?)
(defn- stale?
  [last-update-time-ms time-ms]
  (< last-update-time-ms time-ms))


(defn- stale-values? [values time-ms]
  (cond
    (singular/metric-value? values)
    (stale? (singular/metric-value-last-update-time-ms values) time-ms)

    (histogram/histogram-metric-values? values)
    (stale? (histogram/histogram-metric-values-last-update-time-ms values) time-ms)

    :else
    (assert false values)))

;; -----------------------------------------------------------------

(defn- update-values [values-map labels f & args]
  (update values-map
          labels
          (fn [ref-v]
            (if ref-v
              (do (apply alter ref-v f args)
                  ref-v)
              (ref (apply f nil args))))))

(defn- set-values [values-map labels v]
  (update values-map
          labels
          (fn [ref-v]
            (if ref-v
              (do (ref-set ref-v v)
                  ref-v)
              (ref v)))))

(s/fdef inc-singular-values
  :args (s/cat :stored-values   ::stored-values
               :metric-labels   ::metric-types/metric-labels
               :metric-value    ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn- inc-singular-values
  "Increments the value for a counter metric. Must be called inside a transaction."
  [stored-values metric-labels metric-value last-update-time-ms]
  (lens/overhaul stored-values
                 stored-values-map
                 (fn [labels-values-map]
                   (update-values labels-values-map
                                  metric-labels
                                  (fn [v]
                                    (singular/inc-metric-value (if (singular/metric-value? v)
                                                                 v
                                                                 ;; new labels, or metric changed type? cannot happen via toplevel though.
                                                                 nil)
                                                               metric-value last-update-time-ms))))))

(s/fdef set-singular-values
  :args (s/cat :stored-values   ::stored-values
               :metric-labels   ::metric-types/metric-labels
               :metric-value    ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn- set-singular-values
  "Sets the value of a gauge of set-counter? counter metric. Must be called inside a transaction."
  [stored-values metric-labels metric-value last-update-time-ms]
  (lens/overhaul stored-values
                 stored-values-map
                 (fn [labels-values-map]
                   (set-values labels-values-map
                               metric-labels
                               (singular/make-metric-value metric-value last-update-time-ms)))))

(s/fdef update-histogram-values
  :args (s/cat :stored-values    ::stored-values
               :thresholds       ::metric-types/thresholds
               :metric-labels    ::metric-types/metric-labels
               :metric-value     ::metric-types/metric-value
               :time-ms          ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn- update-histogram-values
  "Updates the value of a histogram metric. Must be called inside a transaction."
  [stored-values thresholds metric-labels metric-value time-ms]
  (lens/overhaul stored-values
                 stored-values-map
                 (fn [labels-values-map]
                   (update-values labels-values-map metric-labels
                                  (fn [v]
                                    (histogram/update-histogram-metric-values
                                     (if (histogram/histogram-metric-values? v)
                                       v
                                       ;; new labels, or change of metric type
                                       nil)
                                     thresholds metric-value time-ms))))))

;; -----------------------------------------------------------------

(s/fdef update-stored-values
  :args (s/cat :stored-values ::stored-values
               :metric        ::metric-types/metric
               :labels ::metric-types/metric-labels
               :value  ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn- update-stored-values
  [stored-values metric labels value last-update-time-ms]
  (cond
    (metric-types/gauge-metric?     metric) (set-singular-values stored-values labels value last-update-time-ms)
    (metric-types/counter-metric?   metric) (if (metric-types/counter-metric-set-value? metric)
                                              (set-singular-values stored-values labels value last-update-time-ms)
                                              (inc-singular-values stored-values labels value last-update-time-ms))
    (metric-types/histogram-metric? metric) (update-histogram-values stored-values (metric-types/histogram-metric-thresholds metric) labels value last-update-time-ms)))

(s/fdef update-or-make-stored-values
  :args (s/cat :maybe-stored-values (s/nilable ::stored-values)
               :metric ::metric-types/metric
               :labels ::metric-types/metric-labels
               :value ::metric-types/metric-value
               :update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn update-or-make-stored-values
  "Updates or creates a value suitable for the given metric. Must be called inside a transaction."
  [maybe-stored-values metric labels value last-update-time-ms]
  ;; Note: updated thresholds in a histogram metric are ignored.
  (update-stored-values (or maybe-stored-values
                            (make-empty-stored-values))
                        metric labels value last-update-time-ms))

(s/fdef prune-stale-stored-values
  :args (s/cat :stored-values ::stored-values
               :time-ms       ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn prune-stale-stored-values
  "Removes values no updated in since the given time."
  [stored-values time-ms]
  (lens/overhaul stored-values
                 stored-values-map
                 (fn [m]
                   ;; OPT: transient?
                   (reduce-kv (fn [res labels values]
                                (if (stale-values? @values time-ms)
                                  res
                                  (assoc res labels values)))
                              {}
                              m))))

(s/fdef empty-stored-values?
  :args (s/cat :stored-values ::stored-values)
  :ret boolean?)
(defn empty-stored-values?
  [stored-values]
  (empty? (stored-values-map stored-values)))

(s/def ::snapshot (s/or :singular  ::singular/metric-value
                        :histogram ::histogram/histogram-metric-values))

(s/fdef get-stored-values-snapshot
  :args (s/cat :stored-values ::stored-values
               :labels ::metric-types/metric-labels)
  :ret (s/or :some ::snapshot :none nil?))
(defn get-stored-values-snapshot
  "Returns a singular or histogram value for the given labels, or nil."
  [stored-values labels]
  (when-let [a (get (stored-values-map stored-values) labels)]
    @a))

(s/fdef get-all-stored-values-snapshot
  :args (s/cat :stored-values ::stored-values)
  :ret (s/map-of ::metric-types/metric-labels ::snapshot))
(defn get-all-stored-values-snapshot
  "Returns a map from labels to snapshots of the values (singular or histogram)."
  [stored-values]
  (into {} (map (fn [[labels values]]
                  [labels @values])
                (stored-values-map stored-values))))
