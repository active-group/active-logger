(ns active.clojure.logger.timbre
  (:require [clojure.string :as string]
            [taoensso.encore :as encore]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as timbre-appenders]
            [taoensso.timbre.appenders.3rd-party.rotor :as timbre-rotor]
            [taoensso.timbre.appenders.3rd-party.logstash :as timbre-logstash]

            [active.clojure.condition :as c]
            [active.clojure.config :as config]
            [active.clojure.logger.internal :as internal]
            [active.clojure.logger.riemann :as riemann]))

(defonce timbre-default-config timbre/*config*) ;; actually not nil!

;; Basic and initial timbre config, before the config is loaded and
;; applied. Note that appenders defined here, are completely replace
;; with the configured appenders.
(def basic-timbre-config ;; TODO: tune that a little timbre/example-config is
  ;; `example-config` is deprectated in timbre > 5.0.0; but since we need to use
  ;; timbre 4.7.2 in some environments because of dependency hell, especially
  ;; when Riemann is involved, we cannot move to `default-config` for now.
  timbre/example-config)

(defn destroy-timbre-config!
  "Cleans up resources that might be held in result of [[make-timbre-config]] resp. [[configure-events-logging]]"
  [timbre-config]
  ;; we use a non-standard field :cleanup-fn in the appenders spec (see https://github.com/ptaoussanis/timbre/issues/217)
  (doseq [[id appender] (:appenders timbre-config)]
    (when-let [f (:cleanup-fn appender)]
      (f appender))))

(defn reset-global-timbre-config!
  "Reset logging config to default, if it equals `compare`."
  [compare]
  (timbre/swap-config! #(if (= % compare) timbre-default-config %)))

(defmacro with-context
  [& args]
  `(timbre/with-context ~@args))

(def ^:dynamic *context* timbre/*context*)

(defn make-timbre-config
  [cmap]
  (if-let [appenders (:appenders cmap)]
    ;; we don't want to keep the initial appenders (a println-appender)
    (encore/merge-deep (dissoc basic-timbre-config :appenders) cmap)
    (encore/merge-deep basic-timbre-config cmap)))

(def log-events-command-config-setting
  (config/setting :log-events-command-config
                  "Monad command config for running event log commands."
                  (config/one-of-range #{:timbre} :timbre)))

;; Since inception of `active-logger`, timbre's api changed the key
;; from :level to :log-level.  We will support both options, with
;; :min-level taking precedence over :level if both are configured.
;; See https://github.com/ptaoussanis/timbre/releases/tag/v5.0.0
(def timbre-min-level-setting
  (config/setting :min-level
                  "Log level for Timbre"
                  (config/optional-range
                   (config/one-of-range #{:trace :debug :info :warn :error :fatal :report}
                                        :debug))))

(def timbre-level-setting
  (config/setting :level
                  "Log level for Timbre.  If both :min-level and :level are configured, :min-level takes precedence over :level."
                  (config/optional-range
                   (config/one-of-range #{:trace :debug :info :warn :error :fatal :report}
                                        :debug))))

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

(def timbre-config-schema
  (config/schema
   "Configuration for Timbre, merged into the default configuration."
                   ;; These could use fleshing out.  Or not.
   timbre-min-level-setting
   timbre-level-setting
   timbre-appenders-setting
   timbre-ns-whitelist-setting
   timbre-ns-blacklist-setting
   timbre-middleware-setting
   timbre-timestamp-opts-setting
   timbre-hostname-setting
   timbre-application-setting))

(defn riemann-appender [& [opts]]
  (riemann/riemann-appender opts))

(defn logstash-appender [host port & [opts]]
  (timbre-logstash/logstash-appender host port
                                     (merge {:pr-stacktrace internal/pr-exception-stacktrace} opts)))

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
           (str "\n" (with-out-str (internal/pr-exception-stacktrace err)))))))))

(defn fixed-properties-timbre-middleware [hostname application]
  ;; A timbre middleware, that sets log event properties that should
  ;; always be set to a specific value; not matter who logs
  ;; something (including foreign libs)
  (let [hostname_ (delay hostname)]
    (fn [data]
      (-> data
          (assoc-in [:hostname_] hostname_)
          (assoc-in [:context :application] application)))))

(def timbre-default-min-level :debug)

(defn configuration->timbre-config
  "Returns an object that can be fed to
  [[set-global-timbre-config!]]."
  [timbre-subconfig]
  (make-timbre-config
   {:min-level      (or (config/access timbre-subconfig timbre-min-level-setting)
                        (config/access timbre-subconfig timbre-level-setting)
                        ;; Since both configuration settings are
                        ;; optional, if none are configured, they will
                        ;; both be `nil`.  That's why we pass :debug
                        ;; as the default here (which matches timbre's
                        ;; default).
                        timbre-default-min-level)
    :appenders      (into {}
                     (map (fn [[k v]]
                            [k (timbre-spec->appender v)])
                          (config/access timbre-subconfig timbre-appenders-setting)))
    :ns-whitelist   (config/access timbre-subconfig timbre-ns-whitelist-setting)
    :ns-blacklist   (config/access timbre-subconfig timbre-ns-blacklist-setting)
    :middleware     (conj (config/access timbre-subconfig timbre-middleware-setting)
                      (fixed-properties-timbre-middleware (config/access timbre-subconfig timbre-hostname-setting)
                                                          (config/access timbre-subconfig timbre-application-setting)))
    :output-fn      output-fn
    :timestamp-opts (let [tso (config/access timbre-subconfig timbre-timestamp-opts-setting)]
                      {:pattern  (get tso :pattern)
                       :locale   (let [l (get tso :locale)]
                                 (if (string? l)
                                   (java.util.Locale/forLanguageTag l)
                                   l))
                       :timezone (let [t (get tso :timezone)]
                                   (if (string? t)
                                     (java.util.TimeZone/getTimeZone ^String t)
                                     t))})}))

(defn set-global-timbre-config!
  [cmap]
  (timbre/set-config! cmap))

(defn set-global-timbre-config-from-map!
  [cmap]
  (set-global-timbre-config! (make-timbre-config cmap)))
