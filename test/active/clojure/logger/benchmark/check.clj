(ns active.clojure.logger.benchmark.check
  (:require [active.clojure.logger.metric-accumulator      :as m]
            [active.clojure.logger.metric-accumulator-test :as mt]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]))

(defn get-n-metric-value-values
  [num-elems]
  (gen/sample (s/gen ::m/metric-value-value) num-elems))

(def metric-number 100000)

(defn check-metric-values
  []
  (let [metric-values (get-n-metric-value-values metric-number)]

    (println metric-number)
    (doseq [value-pos (range (- metric-number 1))]
      (if (Double/isNaN (nth metric-values value-pos))
      (println "x")))
    (println "done")
    ))

;; (check-metric-values)
