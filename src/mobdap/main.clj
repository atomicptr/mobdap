(ns mobdap.main
  (:require
   [clojure.java.io :as io]
   [clojure.tools.cli :refer [parse-opts]]
   [mobdap.handler :as handler]
   [taoensso.timbre :as log]
   [taoensso.timbre.appenders.core :as appenders])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn- cache-dir []
  (or
   (System/getenv "XDG_CACHE_HOME")
   (str (io/file (System/getProperty "user.home") ".cache"))))

(def cli-options
  [["-v" "--version" "Show the installed version of mobdap"]
   ["-h" "--help"    "Show the help text"]
   [nil  "--debug"   "Show debug logs"]])

(defn- print-usage [result]
  (println "Usage:")
  (println (str "\t" "mobdap [options]"))
  (println)
  (println (:summary result)))

(defn -main [& args]
  (let [res (parse-opts args cli-options)]
    (cond
      ; if we had errors
      (not-empty (:errors res)) (do (println "Error:")
                                    (doseq [err (:errors res)]
                                      (println (str "\t" err)))
                                    (println)
                                    (print-usage res)
                                    (System/exit 1))

      ; --help
      (get-in res [:options :help]) (print-usage res)

      ; --version
      (get-in res [:options :version]) (println (System/getProperty "mobdap.version"))

      ; run the app
      :else
      (do
        (log/set-config!
         {:level (if (get-in res [:options :debug])
                   :debug
                   :info)
          :appenders {:spit (appenders/spit-appender {:fname (str (io/file (cache-dir) "mobdap.log"))})}})
        (log/info "Started mobdap with arguments:" args)
        (log/debug "CLI Options" (dissoc res :summary))
        (handler/run)))))

