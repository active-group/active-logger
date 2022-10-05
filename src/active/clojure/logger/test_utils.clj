(ns active.clojure.logger.test-utils
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.lens :as lens]
            [clojure.test :as t]))

(defn strip-timestamps-in-samples
  [samples]
  (mapv #(metric-accumulator/metric-sample-timestamp % 0) samples))

(defn strip-timestamps-in-sample-sets
  [sets]
  (mapv #(lens/overhaul % metric-accumulator/metric-sample-set-samples strip-timestamps-in-samples) sets))

(defn strip-sample-sets
  [sets]
  (map #(metric-accumulator/metric-sample-set-samples % []) sets))

(defn is-metric-set-stored?
  [name type help & [sample-sets]]
  (let [sample-sets (or sample-sets (metric-accumulator/get-all-metric-sample-sets!))
        striped-sample-sets (strip-sample-sets sample-sets)]
    (t/is (some #(= % (metric-accumulator/make-metric-sample-set name type help [])) striped-sample-sets))))

(defn is-metric-stored?
  [name labels value & [sample-sets]]
  (let [sample-sets (or sample-sets (metric-accumulator/get-all-metric-sample-sets!))
        striped-samples (strip-timestamps-in-samples (mapcat metric-accumulator/metric-sample-set-samples sample-sets))]
    (t/is (some #(= % (metric-accumulator/make-metric-sample name labels value 0)) striped-samples))))
