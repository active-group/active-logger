(ns active.clojure.logger.metric-prometheus-util
  (:require [clojure.string :as string]))

(defn make-cleanup-non-prometheus-label-characters
  []
  (memoize
   (fn [s]
     (-> s
         (string/replace #"[^a-zA-Z0-9_]" "_")
         (string/replace #"(^[0-9])" "_\\1")))))

(defn- make-escape-prometheus-label-value
  []
  (memoize
   (fn [s]
     (string/escape (str s) {\" "\\\""}))))

(defn- make-render-label
  [cleanup-non-prometheus-label-characters]
  (let [escape-prometheus-label-value (make-escape-prometheus-label-value)]
    (memoize (fn [k v]
               (str (cleanup-non-prometheus-label-characters (name k)) "=\"" (escape-prometheus-label-value v) "\"")))))

(defn make-render-labels [cleanup-non-prometheus-label-characters]
  (let [render-label (make-render-label cleanup-non-prometheus-label-characters)]
    (memoize
     (fn [labels]
       (if (empty? labels)
         ""
         (str "{"
              (string/join "," (mapv render-label (keys labels) (vals labels)))
              "}"))))))

(defn render-value
  [v]
  (double v))
