(ns mobdap.handler
  (:require
   [clj-stacktrace.core :refer [parse-exception]]
   [clj-stacktrace.repl :refer [pst-str]]
   [clojure.core.async :refer [<!! >!! chan]]
   [clojure.java.io :as io]
   [mobdap.adapter :as adapter]
   [mobdap.debug-server :as debug-server]
   [taoensso.timbre :as log]))

(def ^:private breakpoint-id-counter (atom 1))

(defn- create-handler [adapter]
  {:adapter adapter
   :debug-server nil
   :root-dir nil
   :breakpoints {}
   :channels {:to-handler (chan)
              :to-debug-server nil}})

(defn- relative-path [from to]
  (if (= from to)
    from
    (.relativize (.toPath (io/file from)) (.toPath (io/file to)))))

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
  ([seq ev] (dissoc (event seq ev nil) :body))
  ([seq event body]
   {:type "event"
    :seq seq
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

      (adapter/send-message! adapter (success (:seq message) "launch" nil))
      (adapter/send-message! adapter (event (:seq message) "initialized"))

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

(defn handle-terminate [handler message]
  (>!! (get-in handler [:channels :to-debug-server]) {:cmd :exit})
  (adapter/send-message! (:adapter handler) (success (:seq message) "terminate" nil))
  handler)

(defn handle-message [handler message]
  (log/info "Handle incoming message:" message)

  (case (:command message)
    "initialize"        (handle-initialize handler message)
    "launch"            (handle-launch     handler message)
    "setBreakpoints"    (handle-set-breakpoints handler message)
    "configurationDone" (handle-configuration-done handler message)
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

