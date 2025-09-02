(ns mobdap.debug-server
  (:require
   [clj-stacktrace.core :refer [parse-exception]]
   [clj-stacktrace.repl :refer [pst-str]]
   [clojure.core.async :refer [<!! >!! chan go go-loop thread]]
   [clojure.string :as string]
   [mobdap.lua :as lua]
   [taoensso.timbre :as log])
  (:import
   [java.io BufferedReader InputStreamReader PrintWriter]
   [java.net ServerSocket Socket]))

(def ^:private response-pattern-200 #"^200 OK\s+(.+)")
(def ^:private response-pattern-202 #"^202 Paused\s+(.+)\s+([0-9]+)\s*$")
(def ^:private response-pattern-203 #"^203 Paused\s+(.+)\s+([0-9]+)\s+([0-9]+)\s*$")
(def ^:private response-pattern-204 #"^204 Output (\w+) (\d+)$")
(def ^:private response-pattern-401 #"^401 Error in (\w+) (\d+)\s*$")

(defn- send-line! [server ^String line]
  (let [^PrintWriter writer (get-in server [:client :writer])]
    (log/debug "Debug Server -> Debuggee" line)
    (doto writer
      (.write line)
      (.write "\n")
      (.flush))))

(defn- read-response! [server]
  (let [^BufferedReader reader (get-in server [:client :reader])]
    (when-let [message (.readLine reader)]
      (log/debug "Debuggee -> Debug Server:" message)
      (string/trim message))))

(defn- read-response-with-length! [server len]
  (let [^BufferedReader reader (get-in server [:client :reader])
        len                    (Integer/parseInt len)
        buffer                 (char-array len)]
    (.read reader buffer 0 len)
    (let [res (String/new buffer)]
      (log/debug (format "Debuggee -> Debug Server (%s): %s" len res))
      res)))

(defn- send-blocking-command! [server command]
  (let [^Socket socket (get-in server [:client :socket])
        command (string/upper-case command)]
    (send-line! server command)
    (when (read-response! server)
      (loop []
        (let [breakpoint (read-response! server)]
          (if-not breakpoint
            (do (log/info "Program finished")
                (.close socket)
                (System/exit 0))
            (let [[_ status] (re-find #"^(\d+)" breakpoint)
                  to-adapter (get-in server [:channels :to-adapter])]
              (case status
                "200" (recur)
                "202" (if-let [[_ file line] (re-find response-pattern-202 breakpoint)]
                        (do (log/info "Paused at file:" file "line:" line)
                            (>!! to-adapter {:cmd           :stopped
                                             :type          (cond
                                                              (= command "STEP") :step
                                                              (= command "OUT")  :out
                                                              (= command "OVER") :over
                                                              :else              :breakpoint)
                                             :breakpoint    {:file file
                                                             :line line}})
                            nil)
                        (recur))
                "203" (if-let [[_ file line watch-index] (re-find response-pattern-203 breakpoint)]
                        (do (log/info "Paused at file:" file "line:" line "( watch expression:" watch-index ")")
                            nil)
                        (recur))
                "204" (if-let [[_ text len] (re-find response-pattern-204 breakpoint)]
                        (let [size (parse-long len)
                              message (if (pos? size) (read-response-with-length! server size) "")]
                          (log/debug "OUT1:" text)
                          (log/debug "OUT2:" message)
                          (recur))
                        (recur))
                "401" (if-let [[_ error-type size] (re-find response-pattern-401 breakpoint)]
                        (let [message (read-response-with-length! server size)]
                          (log/error "Error in remote application:" error-type message)
                          nil)
                        (recur))
                (do (log/error "Unknown Error:" breakpoint)
                    nil)))))))))

(defn- send-command-delb! [server file line]
  (send-line! server (format "DELB %s %d" file line)))

(defn- send-command-setb! [server file line]
  (send-line! server (format "SETB %s %d" file line)))

(defn- send-command-stack! [server]
  (send-line! server "STACK")
  (if-let [[_ stack-code] (re-find response-pattern-200 (read-response! server))]
    (lua/extract-table stack-code)
    nil))

(defn- convert-to-safe-whitespace [s]
  (string/replace s #"\r?\n" "\012"))

(defn- send-eval-command! [server expression]
  (try
    (send-line!
     server
     (format
      (if (lua/expression? expression)
        "EXEC return %s"
        "EXEC %s")
      (convert-to-safe-whitespace expression)))
    (let [res        (read-response! server)
          [_ status] (re-find #"^(\d+)" res)]
      (case status
        "200" (when-let [[_ len] (re-find response-pattern-200 res)]
                (let [res (read-response-with-length! server len)]
                  (first (lua/extract-table res))))

        "204" (when-let [[_ text len] (re-find response-pattern-204 res)]
                (let [size (parse-long len)
                      message (if (pos? size) (read-response-with-length! server size) "")]
                  (log/debug "OUT1:" text)
                  (log/debug "OUT2:" message)
                  nil))

        "401" (when-let [[_ error-type len] (re-find response-pattern-401 res)]
                (let [res (read-response-with-length! server len)]
                  (log/error (format "Eval Error in %s: %s" error-type res))
                  res))

        (do (log/error "Unknown Error:" res)
            nil)))
    (catch Throwable t
      (log/error "Eval: Something went wrong:" (pr-str (parse-exception t))))))

(defn- is-connected? [^Socket socket]
  (and (not (.isClosed socket))
       (.isConnected socket)
       (not (.isOutputShutdown socket))))

(defn run-server! [port to-adapter]
  (let [to-debug-server (chan)]
    (thread
      (with-open [server-socket (ServerSocket. port)]
        (try
          (let [client (.accept server-socket)
                _      (log/info "Debuggee connected" client)
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
            (send-blocking-command! server-handle "step")
            (>!! to-adapter {:cmd :setup-done})

            (go-loop []
              (when-let [command (<!! to-debug-server)]
                (log/debug "Handler -> Debug Server:" command)
                (case (:cmd command)
                  :run             (go (send-blocking-command! server-handle "run"))

                  :set-breakpoints (do
                                     ; this command only gets called before executing a command so we
                                     ; can just delete and re-apply all breakpoints
                                     (send-command-delb! server-handle "*" 0)

                                     (doseq [[filename breakpoints] (:breakpoints command)
                                             {:keys [line]} breakpoints]
                                       (send-command-setb! server-handle filename line)))

                  :step-in         (go (send-blocking-command! server-handle "step"))

                  :step-out        (go (send-blocking-command! server-handle "out"))

                  :over            (go (send-blocking-command! server-handle "over"))

                  :stacktrace      (let [stack (send-command-stack! server-handle)]
                                     (>!! to-adapter {:cmd :stacktrace :stack stack :seq (:seq command)}))

                  :eval            (let [result (send-eval-command! server-handle (:expression command))]
                                     (>!! to-adapter {:cmd :eval :result result :seq (:seq command)}))

                  :exit            (do
                                     (send-blocking-command! server-handle "exit")
                                     (.close client)
                                     (System/exit 0))

                  (log/error "Unknown Command:" (:cmd command)))
                (recur)))

            (loop []
              (when (not (is-connected? client))
                (log/info "Lost connection to debuggee, exitting...")
                (System/exit 0))
              (recur)))

          (catch Throwable t
            (log/error "Debug Server: Something went wrong" (pst-str (parse-exception t)))
            (System/exit 1)))))

    to-debug-server))

