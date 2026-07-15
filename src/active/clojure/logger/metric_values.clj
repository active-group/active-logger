(ns ^:no-doc active.clojure.logger.metric-values
  "Internal.

  Functions to store and access data suitable for a metric, per labels."
  (:require [active.clojure.record :refer [define-record-type]]
            [active.clojure.lens :as lens]

            [active.clojure.logger.metric-singular-value :as singular]
            [active.clojure.logger.metric-histogram-value :as histogram]

            [active.clojure.logger.metric-types :as metric-types]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen])
  (:refer-clojure :exclude [replace]))

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

(defn- last-update-time-ms [values]
  (cond
    (singular/metric-value? values)
    (singular/metric-value-last-update-time-ms values)

    (histogram/histogram-metric-values? values)
    (histogram/histogram-metric-values-last-update-time-ms values)

    :else
    (assert false values)))

(defn- stale-values? [values time-ms]
  (stale? (last-update-time-ms values) time-ms))

;; -----------------------------------------------------------------

(defn- make-values [metric value last-update-time-ms]
  (cond
    (metric-types/gauge-metric?     metric) (ref (singular/make-metric-value value last-update-time-ms))
    (metric-types/counter-metric?   metric) (ref (singular/make-metric-value value last-update-time-ms))
    (metric-types/histogram-metric? metric) (ref (histogram/fresh-histogram-metric-values (metric-types/histogram-metric-thresholds metric) value last-update-time-ms))))

(defn- replace [a_ b]
  b)

(defn- update-values! [metric values value last-update-time-ms]
  ;; Note: values must be of the corrent type for the given metric. Changing is is not supported.
  ;; Note: returns the new 'in transaction value', which could be used to optimize/change 'record-and-get'.
  (cond
    (metric-types/gauge-metric?     metric) (commute values replace (singular/make-metric-value value last-update-time-ms))
    (metric-types/counter-metric?   metric) (if (metric-types/counter-metric-set-value? metric)
                                              (commute values replace (singular/make-metric-value value last-update-time-ms))
                                              (commute values singular/inc-metric-value value last-update-time-ms))
    (metric-types/histogram-metric? metric) (commute values histogram/update-histogram-metric-values (metric-types/histogram-metric-thresholds metric) value last-update-time-ms)))

(s/fdef update-stored-values
  :args (s/cat :stored-values ::stored-values
               :metric        ::metric-types/metric
               :labels ::metric-types/metric-labels
               :value  ::metric-types/metric-value
               :last-update-time-ms ::metric-types/metric-last-update-time-ms)
  :ret ::stored-values)
(defn- update-stored-values
  [stored-values metric labels value last-update-time-ms]
  ;; Note: if we have 'values' it must be of the corrent type; changing the metric is not supported (and not possible toplevel)
  (let [labels-values-map (stored-values-map stored-values)]
    (if-let [values (get labels-values-map labels)]
      (do (update-values! metric values value last-update-time-ms)
          stored-values)
      (lens/shove stored-values
                  stored-values-map
                  (assoc labels-values-map labels
                         (make-values metric value last-update-time-ms))))))

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

(defn maybe-update-stored-values!
  "Update an existing value, or return falsy otherwise. Must be called inside a transaction."
  [maybe-stored-values metric labels value last-update-time-ms]
  (when maybe-stored-values
    (when-let [values (get (stored-values-map maybe-stored-values) labels)]
      (update-values! metric values value last-update-time-ms))))

(s/fdef maybe-remove-values
  :args (s/cat :stored-values ::stored-values
               :labels        ::metric-types/metric-labels
               :time-ms       ::metric-types/metric-last-update-time-ms)
  :ret (s/or :some ::stored-values :none nil?))
(defn maybe-remove-values
  "Removes the entry for 'labels' from the map, if and ensuring it's last-update-time
  equals the given one. Returns nil otherwise. Must be called inside a transaction."
  [stored-values labels time-ms]
  (let [m (stored-values-map stored-values)]
    (when-let [values (get m labels)]
      (when (and (= time-ms (last-update-time-ms @values))
                 (= time-ms (last-update-time-ms (ensure values))))
        (lens/shove stored-values stored-values-map
                    (dissoc m labels))))))

(s/fdef find-stale-labels
  :args (s/cat :stored-values ::stored-values
               :time-ms       ::metric-types/metric-last-update-time-ms)
  :ret (s/coll-of (s/tuple ::metric-types/metric-labels ::metric-types/metric-last-update-time-ms)))
(defn find-stale-labels
  "Returns a list of [labels last-update-time-ms] tuples, which were not updated since the given time-ms."
  [stored-values time-ms]
  (->> (stored-values-map stored-values)
       (filter #(stale-values? @(second %) time-ms))
       (map (fn [[labels values]]
              [labels (last-update-time-ms @values)]))))

(defn get-labels-count [stored-values]
  (count (stored-values-map stored-values)))

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
