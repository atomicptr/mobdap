(ns mobdap.handler
  (:require
   [clj-stacktrace.core :refer [parse-exception]]
   [clj-stacktrace.repl :refer [pst-str]]
   [clojure.core.async :refer [<!! >! >!! alts!! chan go-loop]]
   [clojure.java.io :as io]
   [mobdap.adapter :as adapter]
   [mobdap.debug-server :as debug-server]
   [taoensso.timbre :as log]))

(def ^:private go-handler (atom nil))
(def ^:private breakpoint-id-counter (atom 1))

(defn- create-handler [adapter]
  {:adapter adapter
   :debug-server nil
   :root-dir nil
   :breakpoints {}
   :channels {:to-handler (chan)
              :to-debug-server nil}})

(defn- to-int [value]
  (if (int? value)
    value
    (Integer/parseInt value)))

(defn- relative-path [from to]
  (if (= from to)
    from
    (.relativize (.toPath (io/file from)) (.toPath (io/file to)))))

(defn- find-breakpoint-id [handler file line]
  (when-let [breakpoints (get-in handler [:breakpoints file])]
    (->> breakpoints
         (filter #(= (to-int line) (:line %)))
         first
         :id)))

(defn- wait-for-command [ch cmd]
  (go-loop []
    (let [[command _] (alts!! [[ch]])]
      (log/info "WAIT FOR COMMAND:" command "SAME AS" cmd)
      (if (= cmd (:cmd command))
        command
        (do
          ; put command back if it doesnt match
          (>! ch command)
          (recur))))))

(defn- response [success seq command body]
  {:type "response"
   :request_seq seq
   :success success
   :command command
   :body body})

(defn- success [seq command body]
  (response true seq command body))

(defn- error [seq command message]
  (response false seq command {:error message}))

(defn- event
  ([ev] (dissoc (event ev nil) :body))
  ([event body]
   {:type "event"
    :event event
    :body body}))

(defn handle-initialize [handler message]
  (let [response
        (success
         (:seq message)
         "initialize"
         {:supportsConfigurationDoneRequest      true
          :supportsFunctionBreakpoints           false
          :supportsConditionalBreakpoints        false
          :supportsHitConditionalBreakpoints     false
          :supportsEvaluateForHovers             false
          :exceptionBreakpointFilters            false
          :supportsStepBack                      false
          :supportsSetVariable                   false
          :supportsRestartFrame                  false
          :supportsGotoTargetsRequest            false
          :supportsStepInTargetsRequest          false
          :supportsCompletionsRequest            false
          :supportsModulesRequest                false
          :supportsRestartRequest                false
          :supportsExceptionOptions              false
          :supportsValueFormattingOptions        false
          :supportsExceptionInfoRequest          false
          :supportTerminateDebuggee              false
          :supportSuspendDebuggee                false
          :supportsDelayedStackTraceLoading      false
          :supportsLoadedSourcesRequest          false
          :supportsLogPoints                     false
          :supportsTerminateThreadsRequest       false
          :supportsSetExpression                 false
          :supportsTerminateRequest              false
          :supportsDataBreakpoints               false
          :supportsReadMemoryRequest             false
          :supportsWriteMemoryRequest            false
          :supportsDisassembleRequest            false
          :supportsCancelRequest                 false
          :supportsBreakpointLocationsRequest    false
          :supportsClipboardContext              false
          :supportsSteppingGranularity           false
          :supportsInstructionBreakpoints        false
          :supportsExceptionFilterOptions        false
          :supportsSingleThreadExecutionRequests false
          :supportsDataBreakpointBytes           false
          :supportsANSIStyling                   false})]
    (adapter/send-message! (:adapter handler) response)
    handler))

; TODO: this probably still kinda sucks
(defn- transform-stack-trace [stack-trace]
  (map-indexed
   (fn [idx [frame-info _ _]]
     {:id (inc idx)
      :name (first frame-info)
      :source {:name (frame-info 1)}
      :line (parse-long (frame-info 2))
      :column (parse-long (frame-info 3))})
   stack-trace))

(defn- handle-debug-server-command [handler command]
  (log/info "Received command from debug server:" command)
  (case (:cmd command)
    :stopped (case (:type command)
               :breakpoint (let [file (get-in command [:breakpoint :file])
                                 line (get-in command [:breakpoint :line])
                                 id   (find-breakpoint-id handler file line)]
                             (adapter/send-message!
                              (:adapter handler)
                              (event
                               "stopped"
                               {:reason "breakpoint"
                                :allThreadsStopped true
                                :threadId 1
                                :hitBreakpointIds [id]})))

               (log/error "Unknown :stopped command:" (:type command)))

    :stacktrace (let [seq   (:seq   command)
                      stack (:stack command)
                      stack (transform-stack-trace stack)]
                  (log/info "Stacktrace Result" stack)
                  (adapter/send-message! (:adapter handler) (success seq "stackTrace" {:stackFrames stack
                                                                                       :totalFrames (count stack)})))

    (log/error "Unknown server command:" (:cmd command))))

(defn handle-launch [handler message]
  (let [adapter   (:adapter handler)
        port      (or (get-in message [:arguments :port]) 18172)
        arguments (:arguments message)
        root-dir  (:rootdir arguments)
        to-handler (get-in handler [:channels :to-handler])]
    (log/info "Waiting for client on port" port)

    (let [server-channel (debug-server/run-server! (int port) to-handler)]
      (log/info "Waiting for server to finish setup...")

      (loop [] ; TODO: this should not be blocking...
        (let [response (<!! to-handler)]
          (if (= :setup-done (:cmd response))
            nil
            (recur))))

      (log/info "Finished server setup")

      (go-loop []
        (when-let [command (<!! to-handler)]
          (try
            (handle-debug-server-command @go-handler command)
            (catch Throwable t
              (log/error "Message from debug server handler" (pst-str (parse-exception t))))))
        (recur))

      (adapter/send-message! adapter (success (:seq message) "launch" nil))
      (adapter/send-message! adapter (event "initialized"))

      (-> handler
          (assoc    :root-dir root-dir)
          (assoc-in [:channels :to-debug-server] server-channel)))))

(defn handle-set-breakpoints [handler message]
  (let [adapter (:adapter handler)
        {:keys [source breakpoints]} (:arguments message)
        source-path (:path source)
        filename (relative-path (or (:root-dir handler) source-path) source-path)
        breakpoints (mapv #(assoc % :verified true :id (swap! breakpoint-id-counter inc)) breakpoints)]
    (adapter/send-message! adapter (success (:seq message) "setBreakpoints" {:breakpoints breakpoints}))
    (-> handler
        (assoc-in [:breakpoints (str filename)] breakpoints))))
;
(defn handle-configuration-done [handler message]
  (adapter/send-message! (:adapter handler) (success (:seq message) "configurationDone" nil))

  (let [to-debug-server (get-in handler [:channels :to-debug-server])]
    (>!! to-debug-server {:cmd :set-breakpoints :breakpoints (:breakpoints handler)})
    (>!! to-debug-server {:cmd :run}))

  handler)

(defn handle-threads [handler message]
  ; lua is single threaded
  (adapter/send-message! (:adapter handler) (success (:seq message) "threads" {:threads [{:id 1 :name "main"}]}))
  handler)

(defn handle-stacktrace [handler message]
  (let [to-debug-server (get-in handler [:channels :to-debug-server])
        to-handler      (get-in handler [:channels :to-handler])
        start-frame (get-in message [:arguments :startFrame])
        levels      (get-in message [:arguments :levels])]

    (>!! to-debug-server {:cmd :stacktrace :start start-frame :length levels :seq (:seq message)})

    handler))

(defn handle-terminate [handler message]
  (>!! (get-in handler [:channels :to-debug-server]) {:cmd :exit})
  (adapter/send-message! (:adapter handler) (success (:seq message) "terminate" nil))
  handler)

(defn handle-message [handler message]
  (log/info "Handle incoming message:" message)

  (reset! go-handler handler)

  (case (:command message)
    "initialize"        (handle-initialize handler message)
    "launch"            (handle-launch     handler message)
    "setBreakpoints"    (handle-set-breakpoints handler message)
    "configurationDone" (handle-configuration-done handler message)
    "threads"           (handle-threads handler message)
    "stackTrace"        (handle-stacktrace handler message)
    "disconnect"        (handle-terminate handler message)
    "terminate"         (handle-terminate handler message)

    (do (log/error "Unknown Command Received:" message)
        handler)))

(defn run []
  (let [adapter (adapter/create-stdio-adapter)]
    (try
      (loop [handler (create-handler adapter)]
        (recur (if-let [message (adapter/read-message! (:adapter handler))]
                 (handle-message handler message)
                 handler)))

      (catch Throwable t
        (log/error "Handler: Something went wrong" (pst-str (parse-exception t)))
        (System/exit 1)))))

