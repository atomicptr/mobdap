(ns mobdap.dap.message
  (:require
   [mobdap.dap.server :as server]
   [mobdap.debugger :as debugger]
   [taoensso.timbre :as log]))

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

(defn handle-initialize [message out]
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
    (server/send-message! out response)
    (server/send-message! out (event (:seq message) "initialized"))))

(defn handle-launch [message out]
  (let [port (or (get-in message [:arguments :port]) 18172)]
    (log/info "Waiting for client on port" port)
    (debugger/configure! (:arguments message))
    (debugger/start-server! (int port))
    (server/send-message! out (success (:seq message) "launch" nil))))

(defn handle-set-breakpoints [message out]
  (let [{:keys [source breakpoints]} (:arguments message)
        source-path (:path source)
        breakpoints (mapv #(assoc % :verified true :id (debugger/next-breakpoint-id!)) breakpoints)]
    (debugger/add-breakpoint! source-path breakpoints)
    (server/send-message! out (success (:seq message) "setBreakpoints" {:breakpoints breakpoints}))))

(defn handle-configuration-done [message out]
  (server/send-message! out (success (:seq message) "configurationDone" nil))
  (debugger/send-breakpoints!)
  (debugger/send-command-run!))

(defn handle-disconnect [message out]
  (server/send-message! out (success (:seq message) "disconnect" nil))
  (debugger/send-command-exit!)
  (debugger/stop-server!))

(defn handle-terminate [message out]
  (server/send-message! out (success (:seq message) "terminate" nil))
  (debugger/send-command-exit!)
  (debugger/stop-server!))

(defn handle [message out]
  (log/info "Incoming message:" message)

  (case (:command message)
    "initialize"        (handle-initialize message out)
    "launch"            (handle-launch message out)
    "setBreakpoints"    (handle-set-breakpoints message out)
    "configurationDone" (handle-configuration-done message out)
    "disconnect"        (handle-disconnect message out)
    "terminate"         (handle-terminate message out)

    ; unknown command
    (do (log/error "Unknown command: " message)
        (server/send-message! out (error (:seq message) (:command message) "Unknown command")))))

