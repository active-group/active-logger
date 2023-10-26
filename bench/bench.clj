(ns bench
  (:require [active.clojure.logger.metric-accumulator :as m]
            [active.clojure.logger.metric-accumulator-test :as mt]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [criterium.core :as crit])
  (:use [active.clojure.logger.metric-accumulator])
  (:gen-class))

(def metric-number 3000)
(def update-number 50000)

(defn get-n-distinct-metric-names
  [num-elems]
  (first (first (s/exercise (mt/gen-distinct-metric-names num-elems) 1))))

(defn get-n-metric-helps
  [num-elems]
  ;; TODO: very likely will be ["" "" "" "" ""]
  (first (first (s/exercise (mt/gen-metric-helps num-elems) 1))))

(defn get-n-distinct-metric-labels
  [num-elems]
  ;; TODO: many empty lists/maps/vectors and nil values
  (first (first (s/exercise (mt/gen-distinct-metric-labels num-elems) 1))))

(defn get-n-metric-values
  [num-elems]
  (first (first (s/exercise (mt/gen-metric-values num-elems) 1))))

(defn get-n-metric-value-values
  [num-elems]
  (gen/sample (s/gen ::m/metric-value-value) num-elems))

(defn get-n-metric-value-last-updates
  [num-elems]
  (gen/sample (s/gen ::m/metric-value-last-update-time-ms) num-elems))

(defn benchmark-record-and-get!
  []
  (println "Prepare benchmark")
  (println "Metric number:" metric-number)
  (println "Update number:" update-number)

  (let [metric-names  (get-n-distinct-metric-names  metric-number)
        metric-helps  (get-n-metric-helps           metric-number)
        metric-labels (get-n-distinct-metric-labels metric-number)

        value-values       (get-n-metric-value-values       update-number)
        value-last-updates (get-n-metric-value-last-updates update-number)

        metric-store (fresh-metric-store)
        metrics      (mapv (fn [mname, mhelp]
                             (make-counter-metric mname mhelp))
                           metric-names
                           metric-helps)]

    ;; initialize store
    (mapv (fn [m, l]
            (record-and-get! metric-store m l 0 0))
          metrics
          metric-labels)

    (println "Start benchmark")

    (let [record-and-get
          (fn []
            (doseq [update-pos (range (- update-number 1))]
              (let [update-metric (rand-int metric-number)]
                (record-and-get! metric-store
                                 (nth metrics            update-metric)
                                 (nth metric-labels      update-metric)
                                 (nth value-values       update-pos)
                                 (nth value-last-updates update-pos)))))]
      #_(time (record-and-get))
      (crit/bench (record-and-get)))

    (println "End benchmark")
    true))

(println (namespace (symbol (resolve 'fresh-metric-store))))

(defn -main
  [& _args]
  (benchmark-record-and-get!))
