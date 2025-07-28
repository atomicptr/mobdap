(ns mobdap.debugger
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [taoensso.timbre :as log])
  (:import
   [java.io BufferedReader InputStreamReader PrintWriter]
   [java.net ServerSocket]))

(def ^:private state (atom {:server nil
                            :client nil
                            :breakpoints {}
                            :breakpoint-id 1
                            :root-dir nil}))

(defn- relative-path [from to]
  (if (= from to)
    from
    (.relativize (.toPath (io/file from)) (.toPath (io/file to)))))

(defn configure! [config]
  (log/info "Update Configuration" config)
  (swap! state assoc :root-dir (:rootdir config)))

(defn add-breakpoint! [file breakpoints]
  (let [root-dir (or (:root-dir @state) file)
        file     (relative-path root-dir file)]
    (log/info "Add Breakpoint for file:" file " " breakpoints)
    (swap! state assoc-in [:breakpoints file] (distinct breakpoints))))

(defn next-breakpoint-id! []
  (let [id (get @state :breakpoint-id)]
    (swap! state update :breakpoint-id inc)
    id))

(def ^:private response-pattern-202 #"^202 Paused\s+(.+)\s+([0-9]+)\s*$")
(def ^:private response-pattern-203 #"^203 Paused\s+(.+)\s+([0-9]+)\s+([0-9]+)\s*$")
(def ^:private response-pattern-204 #"^204 Output (\w+) (\d+)$")
(def ^:private response-pattern-401 #"^401 Error in Execution (\d+)\s*$")

(defn send-line! [writer line]
  (log/info "Server -> Client:" line)
  (doto writer
    (.write line)
    (.write "\n")
    (.flush)))

(defn read-response! [reader]
  (when-let [message (.readLine reader)]
    (log/info "Client -> Server:" message)
    (string/trim message)))

(defn- send-command! [command]
  (let [reader (get-in @state [:client :reader])
        writer (get-in @state [:client :writer])
        command (string/upper-case command)]
    (log/info "Send Control Command:" command)
    (send-line! writer command)
    (when (read-response! reader)
      (loop []
        (let [breakpoint (read-response! reader)]
          (if-not breakpoint
            (do (log/info "Program finished")
                false)
            (let [[_ status] (re-find #"^(\d+)" breakpoint)]
              (case status
                "200" (recur)
                "202" (if-let [[_ file line] (re-find response-pattern-202 breakpoint)]
                        (do (log/info "Paused at file:" file "line:" line) ; TODO: we hit breakpoint here, tell editor about it
                            true)
                        (recur))
                "203" (if-let [[_ file line watch-index] (re-find response-pattern-203 breakpoint)]
                        (do (log/info "Paused at file:" file "line:" line "( watch expression:" watch-index ")")
                            true)
                        (recur))
                "204" (if-let [[_ text size] (re-find response-pattern-204 breakpoint)]
                        (let [size (parse-long size)
                              message (if (pos? size) (read-response! reader) "")]
                          (log/info "OUT1:" text)
                          (log/info "OUT2:" message)
                          (recur))
                        (recur))
                "401" (if-let [[_ _size] (re-find response-pattern-401 breakpoint)]
                        (let [message (read-response! reader)]
                          (log/info "Error in remote application:" message)
                          false)
                        (recur))
                (do (log/error "Unknown Error:" breakpoint)
                    nil)))))))))

(def send-command-run! (partial send-command! "run"))

(def send-command-step! (partial send-command! "step"))

(def send-command-out! (partial send-command! "out"))

(def send-command-over! (partial send-command! "over"))

(def send-command-exit! (partial send-command! "exit"))

(defn send-command-setb! [file line]
  (let [reader (get-in @state [:client :reader])
        writer (get-in @state [:client :writer])]
    (send-line! writer (format "SETB %s %d" file line))
    (read-response! reader) ; TODO: actually handle response
    true))

(defn send-breakpoints! []
  (doseq [[filename breakpoints] (get @state :breakpoints)
          {:keys [line]} breakpoints]
    (send-command-setb! filename line)))

(defn start-server! [port]
  (let [server (ServerSocket. port)]
    (swap! state assoc :server server)

    (log/info "Started server at port:" port)
    (let [client (.accept server)
          writer (PrintWriter. (.getOutputStream client) true)
          reader (BufferedReader. (InputStreamReader. (.getInputStream client)))]
      (log/info "Client connected" client)
      (swap! state assoc :client {:socket client
                                  :writer writer
                                  :reader reader})
      (send-command-step!))))

(defn stop-server! []
  (let [client (get-in @state [:client :socket])
        server (get-in @state [:server])]
    (log/info "Stopping server...")
    (swap! state assoc :server nil)
    (swap! state assoc :client nil)
    (.close client)
    (.close server)))

