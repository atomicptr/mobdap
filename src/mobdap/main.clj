(ns mobdap.main
  (:require
   [clojure.java.io :as io]
   [mobdap.handler :as handler]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders])
  (:gen-class))

(defn- cache-dir []
  (or
   (System/getenv "XDG_CACHE_HOME")
   (str (io/file (System/getProperty "user.home") ".cache"))))

(defn -main [& args]
  (log/set-config!
   {:level :info
    :appenders {:spit (appenders/spit-appender {:fname (str (io/file (cache-dir) "mobdap.log"))})}})
  (log/info "Started mobdap with arguments:" args)
  (handler/run))


