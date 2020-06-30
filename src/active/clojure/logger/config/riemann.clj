(ns active.clojure.logger.config.riemann
  "Configuration for Riemann."
  (:require [active.clojure.config :as config]
            [riemann.client :as riemann]
            [taoensso.timbre :as timbre]))

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
  [access-config]
  (let [p {:host (access-config riemann-host)
           :port (access-config riemann-port)}
        p (if (access-config riemann-tls?)
            (merge p
                   {:tls? true
                    :key (access-config riemann-key)
                    :cert (access-config riemann-cert)
                    :ca-cert (access-config riemann-ca-cert)})
            p)]
    (riemann/tcp-client p)))

(def log-state-changes-command-config-setting
  (config/setting :log-state-changes-command-config
                  "Monad command config for running state-change log commands."
                  ;; TODO allow port/host settings
                  (config/one-of-range #{:riemann :events} :events)))

(def log-metrics-command-config-setting
  (config/setting :log-metrics-command-config
                  "Monad command config for running metric log commands."
                  ;; TODO allow port/host settings
                  (config/one-of-range #{:riemann :events} :events)))

(defn make-riemann-config [access-config]
  {:client (make-riemann-client access-config)
   ;; Note: this is the 'source hostname', used only as an event property.
   :hostname (access-config riemann-hostname)
   :application (access-config riemann-application)})

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

(defn send-event-to-riemann!
  [config type context map]
  ;; config = (make-riemann-config ...)
  ;; TODO: async, and transform negative acks to conditions?
  ;; Note: new version never throws, but returns a promise, on which deref may throw.
  (riemann/send-events (:client config)
                       [(merge timbre/*context*
                               context
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
