(ns active.clojure.logger.metric-prometheus-util
  (:require [clojure.string :as string]))

(defn cleanup-non-prometheus-label-characters
  [s]
  (string/replace s #"[^a-zA-Z0-9_:]" "_"))

(defn render-label
  [k v]
  (str (cleanup-non-prometheus-label-characters (name k)) "=\"" v "\""))

(defn render-labels
  [labels]
  (if (empty? labels)
    ""
    (str "{"
         (string/join "," (mapv render-label (keys labels) (vals labels)))
         "}")))

(defn render-value
  [v]
  (double v))

(defn maybe-render-timestamp
  [maybe-timestamp]
  (when maybe-timestamp
    (format " %d" maybe-timestamp)))
