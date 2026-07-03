(ns active.clojure.logger.test-utils
  (:require [active.clojure.logger.metric-accumulator :as metric-accumulator]
            [active.clojure.logger.metric-samples :as metric-samples]
            [active.clojure.lens :as lens]
            [clojure.test :as t]))

(defn strip-timestamps-in-samples
  [samples]
  (mapv #(metric-samples/metric-sample-timestamp % 0) samples))

(defn strip-timestamps-in-sample-sets
  [sets]
  (mapv #(lens/overhaul % metric-samples/metric-sample-set-samples strip-timestamps-in-samples) sets))

(defn strip-sample-sets
  [sets]
  (map #(metric-samples/metric-sample-set-samples % []) sets))

(defn is-metric-set-stored?
  [name type help & [sample-sets]]
  (let [sample-sets (or sample-sets (metric-accumulator/get-all-metric-sample-sets!))
        striped-sample-sets (strip-sample-sets sample-sets)]
    (t/is (some #(= % (metric-samples/make-metric-sample-set name type help [])) striped-sample-sets))))

(defn is-metric-stored?
  [name labels value & [sample-sets]]
  (let [sample-sets (or sample-sets (metric-accumulator/get-all-metric-sample-sets!))
        striped-samples (strip-timestamps-in-samples (mapcat metric-samples/metric-sample-set-samples sample-sets))]
    ;; TODO test name, test labels, test values separately (gives better error messages)
    (t/is (some #(= % (metric-samples/make-metric-sample name labels value 0)) striped-samples))))
