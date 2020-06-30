(ns active.clojure.logger.config.timbre
  (:require [active.clojure.condition :as c]
            [active.clojure.condition-hooks :as condition-hooks]
            [active.clojure.config :as config]
            [active.clojure.logger.config.riemann :as logger-riemann]
            [active.timbre-logstash :as timbre-logstash]
            [active.timbre-riemann :as timbre-riemann]
            [clojure.string :as string]
            [riemann.client :as riemann]
            [taoensso.encore :as encore]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as timbre-appenders]
            [taoensso.timbre.appenders.3rd-party.rotor :as timbre-rotor]))

;; Basic and initial timbre config, before the config is loaded and
;; applied. Note that appenders defined here, are completely replace
;; with the configured appenders.
(def basic-timbre-config ;; TODO: tune that a little
  (dissoc timbre/example-config :appenders)) ;; we don't want to keep the initial appenders (a println-appender)

(defn make-timbre-config
  [cmap]
  (encore/merge-deep basic-timbre-config cmap))


(def log-events-command-config-setting
  (config/setting :log-events-command-config
                  "Monad command config for running event log commands."
                  (config/one-of-range #{:timbre} :timbre)))

(def timbre-level-setting
  (config/setting :level
                  "Log level for Timbre"
                  (config/one-of-range #{:trace :debug :info :warn :error :fatal :report}
                                       :debug)))

(def timbre-appenders-setting
  (config/setting :appenders
                  "Appender map for Timbre, where appenders must be defined as a vector of a function creating an appender, and it's arguments. So instead of (appender x) define it as [appender x]."
                  (config/optional-default-range
                   (config/map-of-range
                    config/keyword-range   ;; arbitrary name
                    (config/any-value-range nil)) ;; resp [:spit-appender <any>]; see timbre-spec->appender
                   {:default '(println)})))

(def timbre-ns-whitelist-setting
  (config/setting :ns-whitelist
                  "Whitelist for Timbre"
                  (config/predicate-range "Timbre whitelist"
                                          vector?
                                          [])))

(def timbre-ns-blacklist-setting
  (config/setting :ns-blacklist
                  "Blacklist for Timbre"
                  (config/predicate-range "Timbre blacklist"
                                          vector?
                                          [])))

(def timbre-middleware-setting
  (config/setting :middleware
                  "Middleware for Timbre, where middleware must be defined as a vector of a function creating the middleware, and it's arguments. So instead of (middleware x) define it as [middleware x]."
                  (config/predicate-range "Timbre middleware"
                                          vector?
                                          [])))

(def timbre-hostname-setting
  (config/setting :hostname
                  "Hostname added to the context of all timbre events. Per default inherited from the toplevel :hostname setting."
                  (config/default-string-range (timbre/get-hostname))
                  :inherit? true))

(def timbre-application-setting
  (config/setting :application
                  "Application name added to the context of all timbre events. Per default inherited from the toplevel :application setting."
                  (config/default-string-range nil)
                  :inherit? true))

(def timbre-timestamp-opts-setting
  (config/setting :timestamp-opts
                  "Timestamp options for Timbre"
                  (config/schema-range
                   (config/schema
                    "Timestamp options for Timbre"
                    (config/setting
                     :pattern
                     "Pattern for Timbre's timestamp."
                     (config/any-range
                      (config/default-string-range "yyyy-MM-dd HH:mm:ss.SSS")
                      (config/one-of-range #{:iso8601 :rss2} :iso8601)))
                    (config/setting
                     :locale
                     "Locale language tag for Timbre's timestamp."
                     (config/any-range
                      (config/one-of-range #{:jvm-default} :jvm-default)
                      (config/default-string-range (.toLanguageTag ^java.util.Locale (java.util.Locale. "en")))))
                    (config/setting
                     :timezone
                     "Timezone id for Timbre's timestamp."
                     (config/any-range
                      (config/one-of-range #{:jvm-default :utc} :jvm-default)
                      (config/default-string-range (.getID ^java.util.TimeZone (java.util.TimeZone/getTimeZone "Europe/Amsterdam")))))))))

(def timbre-config-section
  (config/section :timbre-config
                  (config/schema
                   "Configuration for Timbre, merged into the default configuration."
                   ;; These could use fleshing out.  Or not.
                   timbre-level-setting
                   timbre-appenders-setting
                   timbre-ns-whitelist-setting
                   timbre-ns-blacklist-setting
                   timbre-middleware-setting
                   timbre-timestamp-opts-setting
                   timbre-hostname-setting
                   timbre-application-setting)))

;; dummy: but it needs to exist for event logging to be a service
(defn pr-exception-stacktrace [err]
  ;; expecially prints conditions more pretty
  (condition-hooks/print-stack-trace-of err))

(defn timbre-event->riemann [data]
  ;; return zero or more riemann events for one timbre appender data structure.
  (let [{:keys [context instant hostname_ msg_ level]} data

        stacktrace     (when-let [?err (and (:?err_ data) (force (:?err_ data)))]
                         ;; Note: ?err_ will be replaced by ?err in newer timbre versions.
                         (with-out-str (pr-exception-stacktrace ?err)))
        [time time-ms] (logger-riemann/current-time-for-riemann)]
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

(declare -log-event!)

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
        (-log-event! :warn (str "Could not connect to Riemann at " at ". Some messages will be dropped until Riemann can be reached."))
        (-log-event! :debug (str "Connected to Riemann at " at "."))))

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

(defn logstash-appender [host port & [opts]]
  (timbre-logstash/timbre-json-appender host port
                                        (merge {:pr-stacktrace pr-exception-stacktrace} opts)))

(defn rotor-appender [opts]
  (timbre-rotor/rotor-appender opts))

(defn spit-appender [opts]
  (timbre-appenders/spit-appender opts))

(defn println-appender [& [opts]]
  (timbre-appenders/println-appender opts))

(defn timbre-spec->appender
  "Convert an EDN Timbre appender spec to an actual appender.

  An appender spec is a list starting with one of `{spit, rotor, logstash, println}`,
  followed by keyword parameters corresponding to the respective appender."
  [v]
  (cond
    (and (list? v) (not (empty? v)) (symbol? (first v)))
    (case (first v)
      ;; example args: ({:fname "my.log"})
      spit (apply spit-appender (rest v))

      ;; example args: ({:path "my.log" :max-size 1048576 :backlog 5}), :max-size is given in bytes
      rotor (apply rotor-appender (rest v))

      ;; example args: ("localhost" 4660)
      logstash (apply logstash-appender (rest v))
      ;; a :stream arg would be possible
      println  (apply println-appender (rest v))

      riemann (apply riemann-appender (rest v))
      (c/error `timbre-spec->appender "invalid Timbre appender spec" v))

    :else (c/error `timbre-spec->appender "invalid Timbre appender spec" v)))

(defn output-fn
  "Timbre output function, adapted to our needs"
  ([data] (output-fn nil data))
  ([{:keys [no-stacktrace?] :as opts} data]
   (let [{:keys [level ?err_ vargs_ msg_ ?ns-str hostname_
                 timestamp_ ?line]} data
         origin-info
         (cond-> (or ?ns-str "?") ;; we never have line number, for which Timbre would print a ?
           ;; append a task or service name, and domain if we have one (e.g. log4j logs don't have one):
           (:component timbre/*context*)
           (str ", " (:component timbre/*context*)
                (if-let [domain (:domain timbre/*context*)]
                  (str " [" domain "]")
                  "")))]
     (str
       @timestamp_ " "
       @hostname_  " "
       (string/upper-case (name level))  " "
       "[" origin-info "] - "
       (force msg_)
       (when-not no-stacktrace?
         (when-let [err (force ?err_)]
           (str "\n" (with-out-str (pr-exception-stacktrace err)))))))))

(defn fixed-properties-timbre-middleware [hostname application]
  ;; A timbre middleware, that sets log event properties that should
  ;; always be set to a specific value; not matter who logs
  ;; something (including foreign libs)
  (let [hostname_ (delay hostname)]
    (fn [data]
      (-> data
          (assoc-in [:hostname_] hostname_)
          (assoc-in [:context :application] application)))))

(defn configure-events-logging
  "Returns an object that can be fed to
  [[set-global-log-events-config!]]."
  [timbre-config]
  (make-timbre-config
   {:level          (config/access timbre-config timbre-level-setting)
    :appenders      (into {}
                     (map (fn [[k v]]
                            [k (timbre-spec->appender v)])
                          (config/access timbre-config timbre-appenders-setting)))
    :ns-whitelist   (config/access timbre-config timbre-ns-whitelist-setting)
    :ns-blacklist   (config/access timbre-config timbre-ns-blacklist-setting)
    :middleware     (conj (config/access timbre-config timbre-middleware-setting)
                      (fixed-properties-timbre-middleware (config/access timbre-config timbre-hostname-setting)
                                                          (config/access timbre-config timbre-application-setting)))
    :output-fn      output-fn
    :timestamp-opts (let [tso (config/access timbre-config timbre-timestamp-opts-setting)]
                      {:pattern  (get tso :pattern)
                       :locale   (let [l (get tso :locale)]
                                 (if (string? l)
                                   (java.util.Locale/forLanguageTag l)
                                   l))
                       :timezone (let [t (get tso :timezone)]
                                   (if (string? t)
                                     (java.util.TimeZone/getTimeZone ^String t)
                                     t))})}))
