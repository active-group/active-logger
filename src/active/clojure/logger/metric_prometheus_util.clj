(ns active.clojure.logger.metric-prometheus-util
  (:require [clojure.string :as string]))

(defn cleanup-non-prometheus-label-characters-1
  [s]
  (-> s
      (string/replace #"[^a-zA-Z0-9_]" "_")
      (string/replace #"(^[0-9])" "_\\1")))

(def cleanup-non-prometheus-label-characters
  (memoize cleanup-non-prometheus-label-characters-1))

(defn render-label-1
  [k v]
  (str (cleanup-non-prometheus-label-characters (name k)) "=\"" v "\""))

(def render-label
  (memoize render-label-1))

(defn render-labels-1
  [labels]
  (if (empty? labels)
    ""
    (str "{"
         (string/join "," (mapv render-label (keys labels) (vals labels)))
         "}")))

(def render-labels
  (memoize render-labels-1))

(defn render-value
  [v]
  (double v))

(defn maybe-render-timestamp
  [maybe-timestamp]
  (when maybe-timestamp
    (format " %d" maybe-timestamp)))
