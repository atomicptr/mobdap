(ns mobdap.dap.core
  (:require [clojure.core.async :refer [go <!]]
            [mobdap.dap.message :as message]
            [mobdap.dap.server :as server]
            [mobdap.debugger :as debugger]
            [taoensso.timbre :as log]))

(defn run []
  (let [{:keys [in out]} (server/start-server)]
    (go (loop []
          (let [message (<! in)]
            (try
              (message/handle message out)
              (catch Throwable t
                (log/error "Something went wrong" t)
                (debugger/stop-server!)
                (System/exit 1)))
            (recur))))))
