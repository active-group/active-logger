(ns active.clojure.logger.riemann
  "Configuration for Riemann."
  (:require [active.timbre-riemann :as timbre-riemann]
            [active.clojure.config :as config]
            [riemann.client :as riemann]
            [taoensso.timbre :as timbre]
            [active.clojure.logger.internal :as internal]))

(def riemann-host
  (config/setting :host
                  "TCP/IP host of Riemann server"
                  (config/default-string-range "127.0.0.1")))
(def riemann-port
  (config/setting :port
                  "TCP/IP port of Riemann server"
                  (config/integer-between-range 1 65535 5555)))
(def riemann-tls?
  (config/setting :tls?
                  "Use TLS for Riemann?"
                  (config/boolean-range false)))

(def riemann-hostname
  (config/setting :hostname
                  "String to set as the source ':host' of riemann events. Inherits from global :hostname setting."
                  (config/default-string-range (timbre/get-hostname))
                  :inherit? true))

(def riemann-application
  (config/setting :application
                  "String to set as the ':application' of riemann events. Inherits from global :application setting."
                  (config/default-string-range nil)
                  :inherit? true))

; https://github.com/aphyr/riemann-clojure-client:
; :key, :cert and :ca-cert could be any type of File, URI, URL,
; Socket, byte array, and String arguments. If the argument is a
; String, it tries to resolve it first as a URI, then as a local file
; name. URIs with a 'file' protocol are converted to local file names.

(def riemann-key
  (config/setting :key
                  "TLS key file for Riemann"
                  config/string-range))
(def riemann-cert
  (config/setting :cert
                  "TLS cert file for Riemann"
                  config/string-range))
(def riemann-ca-cert
  (config/setting :ca-cert
                  "TLS CA cert file for Riemann"
                  config/string-range))

(def riemann-config-section
  (config/section :riemann
                  (config/schema
                   "Riemann client parameters"
                   riemann-host
                   riemann-port
                   riemann-tls?
                   riemann-hostname
                   riemann-application
                   riemann-key riemann-cert riemann-ca-cert)))

(defn make-riemann-client
  "Log command config, to send state-changes or metrics to a Riemann server with the given riemann client."
  [riemann-config]
  (let [p {:host (config/access riemann-config riemann-host)
           :port (config/access riemann-config riemann-port)}
        p (if (config/access riemann-config riemann-tls?)
            (merge p
                   {:tls? true
                    :key (config/access riemann-config riemann-key)
                    :cert (config/access riemann-config riemann-cert)
                    :ca-cert (config/access riemann-config riemann-ca-cert)})
            p)]
    (riemann/tcp-client p)))

(defn make-riemann-config
  [riemann-config]
  {:client      (make-riemann-client riemann-config)
   ;; Note: this is the 'source hostname', used only as an event property.
   :hostname    (config/access riemann-config riemann-hostname)
   :application (config/access riemann-config riemann-application)})

(defn destroy-riemann-config! [config]
  ;; esp. closes the riemann-client, which otherwise leaves some non-daemon threads preventing process end.
  ;; (see https://github.com/riemann/riemann-java-client/issues/63)
  (riemann/close! (:client config)))

(defn current-time-for-riemann
  "Returns the current time, as [time time-ms], where time is the
  number of seconds since the epoch, and time-ms the corresponding
  sub-seconds milliseconds, converted to a string."
  []
  (let [tm (System/currentTimeMillis)]
    [(quot tm 1000)
     (str (mod tm 1000))]))

(defn fixed-properties-riemann [hostname application]
  ;; Additional riemann event properties, that should
  ;; always be set to a specific value; not matter who logs
  ;; something (including foreign libs)
  {:host hostname
   :application application})

;; TODO: maybe we need to take Riemann's special keys into consideration here:
;; From riemann sources (hardly any docs!):
;; :host         ; string
;; :service      ; string
;; :state        ; string
;; :description  ; string
;; :metric       ; float, long or double.
;; :tags         ; seq of strings
;; :time         ; long, seconds since UTC epoch
;; :ttl          ; float
;; anything else, must be keyword => string.
(defn sanitize-context
  "Make sure the context only contains string values, remove non-string values."
  [mp]
  (if (some (fn [entry] (not (string? (val entry)))) mp)
    (->> mp
         (filter (fn [entry]
                   (if (string? (val entry))
                     true
                     (binding [*out* *err*]
                       (println (str "WARNING: log context contains non-string value for key " (key entry) " (" (val entry) ")"))
                       false))))
         (into {}))
    mp))

(defn send-event-to-riemann!
  [config type context map]
  ;; config = (make-riemann-config ...)
  ;; TODO: async, and transform negative acks to conditions?
  ;; Note: new version never throws, but returns a promise, on which deref may throw.
  (riemann/send-events (:client config)
                       [(merge timbre/*context*
                               (sanitize-context context)
                               (let [[time time-ms] (current-time-for-riemann)]
                                 {:type type
                                  :time time
                                  :time-ms time-ms})
                               map
                               (fixed-properties-riemann (:hostname config)
                                                         (:application config)))]))

(defn log-state-change-to-riemann!
  [config state ttl mp]
  (send-event-to-riemann! config "state" mp {:state state :ttl (or ttl 60.0)}))

(defn timbre-event->riemann [data]
  ;; return zero or more riemann events for one timbre appender data structure.
  (let [{:keys [context instant hostname_ msg_ level]} data

        stacktrace     (when-let [?err (:?err data)]
                         (with-out-str (internal/pr-exception-stacktrace ?err)))
        [time time-ms] (current-time-for-riemann)]
    ;; Note: context already may contain some riemann specific props too, like
    ;; :service, :state, :metric, :ttl.
    [(-> (cond-> {:host (force hostname_)
                  ;; :state nil
                  ;; :description nil
                  ;; :metric nil
                  ;; :tags nil
                  :time time
                  :time-ms time-ms
                  ;; :ttl nil
                  }
           (some? stacktrace)          (assoc :stacktrace stacktrace)
           (= "event" (:type context)) (assoc :level (name level)
                                              :description (force msg_)))
         (merge context))]))

(defn riemann-appender [& [opts]]
  (let [riemann-opts (-> {:host "localhost" :port 5555}
                         (merge (select-keys opts [:host :port
                                                   :tls? :key
                                                   :cert :ca-cert
                                                   :cache-dns?])))
        ;; Note: creating the client never throws
        client       (riemann/tcp-client riemann-opts)]

    (let [at (str (:host riemann-opts) ":" (:port riemann-opts))]
      (if (not (riemann/connected? client))
        ;; this'll be visible on the console, before timbre is reconfigured:
        (internal/-log-event! :warn (str "Could not connect to Riemann at " at ". Some messages will be dropped until Riemann can be reached."))
        (internal/-log-event! :debug (str "Connected to Riemann at " at "."))))

    (-> (timbre-riemann/riemann-appender
         (merge {:client    client
                 :retry-fn  (fn [promise repeat-nr message-nr]
                             ;; we could (deref promise) here, resulting in a result message from the riemann server, or
                             ;; in an IOException("no channels available") when the connection drops/server stopped, or in
                             ;; an OverloadedException when the client produces more message than the server can handle.
                             ;; For now, we think it's more important that the platform keeps on running.
                              false ;; don't retry.
                              )
                 :events-fn timbre-event->riemann}
                opts))
        ;; cleanup-fn is our own extension to the appenders spec; see destroy-timbre-config! below
        (assoc :cleanup-fn (fn [this]
                             (riemann/close! client))))))
