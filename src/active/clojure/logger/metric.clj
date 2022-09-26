(ns active.clojure.logger.metric
  "Facilities for logging metrics."
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-emitter :as metric-emitter]
            [active.clojure.monad :as monad]))

(defn log-metric!-internal
  [namespace metric labels value & [mp]]
  (let [metric-samples (metric-accumulator/record-and-get! metric labels value)]
    (metric-emitter/emit-metrics! metric-samples mp namespace)))

(defn log-metric-internal
  [namespace metric labels value & [mp]]
  (monad/monadic
    [metric-samples (metric-accumulator/record-and-get metric labels value)]
    (metric-emitter/emit-metrics metric-samples mp namespace)))

(defmacro log-metric!
  ([?metric ?value]
  `(log-metric! ~?metric {} ~?value nil ~(str *ns*)))
  ([?metric ?labels ?value]
  `(log-metric! ~?metric ~?labels ~?value nil ~(str *ns*)))
  ([?metric ?labels ?value ?mp]
  `(log-metric! ~?metric ~?labels ~?value ~?mp ~(str *ns*)))
  ([?metric ?labels ?value ?mp ?ns]
  `(log-metric!-internal ~?ns ~?metric ~?labels ~?value ~?mp)))

(defmacro log-metric
  ([?metric ?value]
  `(log-metric ~?metric {} ~?value nil ~(str *ns*)))
  ([?metric ?labels ?value]
  `(log-metric ~?metric ~?labels ~?value nil ~(str *ns*)))
  ([?metric ?labels ?value ?mp]
  `(log-metric ~?metric ~?labels ~?value ~?mp ~(str *ns*)))
  ([?metric ?labels ?value ?mp ?ns]
  `(log-metric-internal ~?ns ~?metric ~?labels ~?value ~?mp)))

(defmacro log-gauge-metric!
  ([?name ?value]
   `(log-gauge-metric! ~?name {} ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?value]
   `(log-gauge-metric! ~?name ~?labels ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value]
   `(log-gauge-metric! ~?name ~?labels ~?help ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp]
   `(log-gauge-metric! ~?name ~?labels ~?help ~?value ~?mp ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp ?ns]
   `(log-metric! (metric-accumulator/make-gauge-metric ~?name ~?help) ~?labels ~?value ~?mp ~?ns)))

(defmacro log-gauge-metric
  ([?name ?value]
   `(log-gauge-metric ~?name {} ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?value]
   `(log-gauge-metric ~?name ~?labels ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value]
   `(log-gauge-metric ~?name ~?labels ~?help ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp]
   `(log-gauge-metric ~?name ~?labels ~?help ~?value ~?mp ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp ?ns]
   `(log-metric (metric-accumulator/make-gauge-metric ~?name ~?help) ~?labels ~?value ~?mp ~?ns)))

(defmacro log-counter-metric!
  ([?name ?value]
   `(log-counter-metric! ~?name {} ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?value]
   `(log-counter-metric! ~?name ~?labels ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value]
   `(log-counter-metric! ~?name ~?labels ~?help ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp]
   `(log-counter-metric! ~?name ~?labels ~?help ~?value ~?mp ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp ?ns]
   `(log-metric! (metric-accumulator/make-counter-metric ~?name ~?help) ~?labels ~?value ~?mp ~?ns)))

(defmacro log-counter-metric
  ([?name ?value]
   `(log-counter-metric ~?name {} ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?value]
   `(log-counter-metric ~?name ~?labels ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value]
   `(log-counter-metric ~?name ~?labels ~?help ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp]
   `(log-counter-metric ~?name ~?labels ~?help ~?value ~?mp ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp ?ns]
   `(log-metric (metric-accumulator/make-counter-metric ~?name ~?help) ~?labels ~?value ~?mp ~?ns)))

(defmacro set-counter-metric!
  ([?name ?value]
   `(set-counter-metric! ~?name {} ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?value]
   `(set-counter-metric! ~?name ~?labels ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value]
   `(set-counter-metric! ~?name ~?labels ~?help ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp]
   `(set-counter-metric! ~?name ~?labels ~?help ~?value ~?mp ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp ?ns]
   `(log-metric! (metric-accumulator/make-counter-metric ~?name ~?help true) ~?labels ~?value ~?mp ~?ns)))

(defmacro set-counter-metric
  ([?name ?value]
   `(set-counter-metric ~?name {} ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?value]
   `(set-counter-metric ~?name ~?labels ~?name ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value]
   `(set-counter-metric ~?name ~?labels ~?help ~?value nil ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp]
   `(set-counter-metric ~?name ~?labels ~?help ~?value ~?mp ~(str *ns*)))
  ([?name ?labels ?help ?value ?mp ?ns]
   `(log-metric (metric-accumulator/make-counter-metric ~?name ~?help true) ~?labels ~?value ~?mp ~?ns)))

(defmacro log-histogram-metric!
  ([?name ?thresholds ?value]
   `(log-histogram-metric! ~?name ~?thresholds {} ~?name ~?value nil ~(str *ns*)))
  ([?name ?thresholds ?labels ?value]
   `(log-histogram-metric! ~?name ~?thresholds ~?labels ~?name ~?value nil ~(str *ns*)))
  ([?name ?thresholds ?labels ?help ?value]
   `(log-histogram-metric! ~?name ~?thresholds ~?labels ~?help ~?value nil ~(str *ns*)))
  ([?name ?thresholds ?labels ?help ?value ?mp]
   `(log-histogram-metric! ~?name ~?thresholds ~?labels ~?help ~?value ~?mp ~(str *ns*)))
  ([?name ?thresholds ?labels ?help ?value ?mp ?ns]
   `(log-metric! (metric-accumulator/make-histogram-metric ~?name ~?help ~?thresholds) ~?labels ~?value ~?mp ~?ns)))

(defmacro log-histogram-metric
  ([?name ?thresholds ?value]
   `(log-histogram-metric ~?name ~?thresholds {} ~?name ~?value nil ~(str *ns*)))
  ([?name ?thresholds ?labels ?value]
   `(log-histogram-metric ~?name ~?thresholds ~?labels ~?name ~?value nil ~(str *ns*)))
  ([?name ?thresholds ?labels ?help ?value]
   `(log-histogram-metric ~?name ~?thresholds ~?labels ~?help ~?value nil ~(str *ns*)))
  ([?name ?thresholds ?labels ?help ?value ?mp]
   `(log-histogram-metric ~?name ~?thresholds ~?labels ~?help ~?value ~?mp ~(str *ns*)))
  ([?name ?thresholds ?labels ?help ?value ?mp ?ns]
   `(log-metric (metric-accumulator/make-histogram-metric ~?name ~?help ~?thresholds) ~?labels ~?value ~?mp ~?ns)))

(def monad-command-config
  (monad/combine-monad-command-configs metric-accumulator/monad-command-config metric-emitter/log-metrics-command-config))
