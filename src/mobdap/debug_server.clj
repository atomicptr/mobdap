(ns mobdap.debug-server
  (:require
   [clj-stacktrace.core :refer [parse-exception]]
   [clj-stacktrace.repl :refer [pst-str]]
   [clojure.core.async :refer [<!! >!! chan go-loop thread]]
   [clojure.string :as string]
   [mobdap.lua :as lua]
   [taoensso.timbre :as log])
  (:import
   [java.io BufferedReader InputStreamReader PrintWriter]
   [java.net ServerSocket]))

(def ^:private response-pattern-200 #"^200 OK\s+(.+)")
(def ^:private response-pattern-202 #"^202 Paused\s+(.+)\s+([0-9]+)\s*$")
(def ^:private response-pattern-203 #"^203 Paused\s+(.+)\s+([0-9]+)\s+([0-9]+)\s*$")
(def ^:private response-pattern-204 #"^204 Output (\w+) (\d+)$")
(def ^:private response-pattern-401 #"^401 Error in Execution (\d+)\s*$")

(defn send-line! [server line]
  (let [writer (get-in server [:client :writer])]
    (log/info "Server -> Client:" line)
    (doto writer
      (.write line)
      (.write "\n")
      (.flush))))

(defn read-response! [server]
  (let [reader (get-in server [:client :reader])]
    (when-let [message (.readLine reader)]
      (log/info "Client -> Server:" message)
      (string/trim message))))

(defn- send-command! [server command]
  (let [command (string/upper-case command)]
    (log/info "Send Command:" command)
    (send-line! server command)
    (when (read-response! server)
      (loop []
        (let [breakpoint (read-response! server)]
          (if-not breakpoint
            (do (log/info "Program finished")
                (.close (get-in server [:client :socket]))
                (System/exit 0))
            (let [[_ status] (re-find #"^(\d+)" breakpoint)
                  to-adapter (get-in server [:channels :to-adapter])]
              (case status
                "200" (recur)
                "202" (if-let [[_ file line] (re-find response-pattern-202 breakpoint)]
                        (do (log/info "Paused at file:" file "line:" line)
                            (>!! to-adapter {:cmd           :stopped
                                             :type          :breakpoint
                                             :breakpoint    {:file file
                                                             :line line}})
                            nil)
                        (recur))
                "203" (if-let [[_ file line watch-index] (re-find response-pattern-203 breakpoint)]
                        (do (log/info "Paused at file:" file "line:" line "( watch expression:" watch-index ")")
                            nil)
                        (recur))
                "204" (if-let [[_ text size] (re-find response-pattern-204 breakpoint)]
                        (let [size (parse-long size)
                              message (if (pos? size) (read-response! server) "")]
                          (log/info "OUT1:" text)
                          (log/info "OUT2:" message)
                          (recur))
                        (recur))
                "401" (if-let [[_ _size] (re-find response-pattern-401 breakpoint)]
                        (let [message (read-response! server)]
                          (log/info "Error in remote application:" message)
                          nil)
                        (recur))
                (do (log/error "Unknown Error:" breakpoint)
                    nil)))))))))

(defn send-command-setb! [server file line]
  (send-line! server (format "SETB %s %d" file line)))

(defn send-command-stack! [server]
  (send-line! server "STACK")
  (if-let [[_ stack-code] (re-find response-pattern-200 (read-response! server))]
    (lua/extract-table stack-code)
    nil))

; TODO: this doesnt actually work
(defn is-connected? [socket]
  (and (not (.isClosed socket))
       (.isConnected socket)
       (not (.isOutputShutdown socket))))

(defn run-server! [port to-adapter]
  (let [to-debug-server (chan)]
    (thread
      (with-open [server-socket (ServerSocket. port)]
        (try
          (let [client (.accept server-socket)
                _      (log/info "Client connected" client)
                writer (PrintWriter. (.getOutputStream client) true)
                reader (BufferedReader. (InputStreamReader. (.getInputStream client)))
                server-handle {:client   {:socket client
                                          :writer writer
                                          :reader reader}
                               :server   server-socket
                               :port     port
                               :channels {:to-adapter      to-adapter
                                          :to-debug-server to-debug-server}}]

            ; send step command
            (send-command! server-handle "step")
            (>!! to-adapter {:cmd :setup-done})

            (go-loop []
              (when-let [command (<!! to-debug-server)]
                (log/info "Command to debug server" command)
                (case (:cmd command)
                  :run             (send-command! server-handle "run")

                  ; TODO: delete all prior breakpoints by using "LISTB" -> "DELB"
                  :set-breakpoints (doseq [[filename breakpoints] (:breakpoints command)
                                           {:keys [line]} breakpoints]
                                     (send-command-setb! server-handle filename line))

                  :stacktrace      (let [stack (send-command-stack! server-handle)]
                                     (log/info "STACK TRACE GOT:" stack)
                                     (>!! to-adapter {:cmd :stacktrace :stack stack :seq (:seq command)}))

                  :exit            (do
                                     (send-command! server-handle "exit")
                                     (.close client)
                                     (System/exit 0))

                  (log/error "Unknown Command:" (:cmd command)))
                (recur)))

            (loop []
              ; TODO: this actually doesnt work but we should shut down mobdap when client is gone
              (when (not (is-connected? client))
                (log/info "Lost connection to client, exitting...")
                (System/exit 0))
              (recur)))

          (catch Throwable t
            (log/error "Debug Server: Something went wrong" (pst-str (parse-exception t)))
            (System/exit 1)))))

    to-debug-server))

