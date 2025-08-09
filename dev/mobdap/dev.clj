(ns mobdap.dev
  (:require [mobdap.main :as mobdap]
            [nrepl.server :as nrepl]))

(defonce nrepl-server (atom {}))

(defn -main [& _]
  (reset! nrepl-server (nrepl/start-server :port (or (System/getenv "NREPL_PORT") 45999)))
  (apply mobdap/-main ["--debug"]))
