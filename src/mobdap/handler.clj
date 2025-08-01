(ns mobdap.handler
  (:require
   [clj-stacktrace.core :refer [parse-exception]]
   [clj-stacktrace.repl :refer [pst-str]]
   [clojure.core.async :refer [<!! >!! chan go-loop]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [mobdap.adapter :as adapter]
   [mobdap.debug-server :as debug-server]
   [taoensso.timbre :as log]))

(def ^:private go-handler (atom nil))
(def ^:private breakpoint-id-counter (atom 1))
(def ^:private stackframe-id-counter (atom 1))
(def ^:private vars-id-counter (atom 1))
(def ^:private var-index (atom {}))

(defn- create-handler [adapter]
  {:adapter adapter
   :debug-server nil
   :root-dir     nil
   :breakpoints  {}
   :channels     {:to-handler (chan)
                  :to-debug-server nil}
   :stackframes  (atom [])})

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

(defn- map-over-map [fun m]
  (reduce-kv (fn [m k v] (assoc m k (fun k v))) {} m))

(defn- parse-heap-value [ident]
  (when-let [[_ type addr] (re-find #"(.+):\s*(0x[A-Za-z0-9]+)" ident)]
    {:type (keyword type)
     :addr addr}))

(defn- parse-value [_ packed-value]
  (case (count packed-value)
    1 {:id   (swap! vars-id-counter inc)
       :type :constant
       :value nil}
    2 (let [[value ident] packed-value]
        (if-let [{type :type addr :addr} (parse-heap-value ident)]
          {:id   (swap! vars-id-counter inc)
           :type type
           :addr addr
           :value value}
          {:id   (swap! vars-id-counter inc)
           :type :constant
           :value value}))
    (do (log/error "Could not unpack" packed-value)
        nil)))

(defn- transform-stack-trace [handler stack-trace]
  (map-indexed
   (fn [_ [frame-info stack upvalues]]
     (let [stack    (map-over-map parse-value stack)
           upvalues (map-over-map parse-value upvalues)]

       ; register vars from stack
       (doseq [[k v] stack]
         (swap! var-index assoc k v))

       ; register vars from upvalues
       (doseq [[k v] upvalues]
         (swap! var-index assoc k v))

       (case (count frame-info)
         ; example of the frame header (6):
         ;   0: "modules/tbl.lua"   - path for the function
         ;   1: 6                   - line where function gets declared
         ;   2: 13                  - breakpoint? entry point?
         ;   3: "Lua"               - Lua (thats where it comes from ig)
         ;   4: "field"             - ??? this can also be "upvalue"
         ;   5: "modules/tbl.lua"   - file location again
         6 {:id (swap! stackframe-id-counter inc)
            :name ""
            :source {:path (str (io/file (or (:root-dir handler) ".") (frame-info 0)))}
            :line (parse-long (frame-info 2))
            :column 0
            :extras {:scope-start (parse-long (frame-info 1))
                     :stack       {:id     (swap! vars-id-counter inc)
                                   :values stack}
                     :upvalues    {:id     (swap! vars-id-counter inc)
                                   :values upvalues}
                     :type        (frame-info 4)}}

         ; example of the frame header:
         ;   0: "contains"          - name of the function
         ;   1: "modules/tbl.lua"   - path for the function
         ;   2: 6                   - line where function gets declared
         ;   3: 13                  - breakpoint? entry point?
         ;   4: "Lua"               - Lua (thats where it comes from ig)
         ;   5: "field"             - ??? this can also be "upvalue"
         ;   6: "modules/tbl.lua"   - file location again
         7 {:id (swap! stackframe-id-counter inc)
            :name (or (frame-info 0) "")
            :source {:path (str (io/file (or (:root-dir handler) ".") (frame-info 1)))}
            :line (parse-long (frame-info 3))
            :column 0
            :extras {:scope-start (parse-long (frame-info 2))
                     :stack       {:id     (swap! vars-id-counter inc)
                                   :values stack}
                     :upvalues    {:id     (swap! vars-id-counter inc)
                                   :values upvalues}
                     :type        (frame-info 5)}}

         (do (log/error "Could not transform stack trace" frame-info)
             nil))))
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
                      stack (transform-stack-trace handler stack)]
                  (log/info "Stacktrace Result" stack)
                  (reset! (:stackframes handler) stack)
                  (adapter/send-message! (:adapter handler) (success seq "stackTrace" {:stackFrames (map #(dissoc % :extras) stack)
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
        start-frame (get-in message [:arguments :startFrame])
        levels      (get-in message [:arguments :levels])]

    (>!! to-debug-server {:cmd :stacktrace :start start-frame :length levels :seq (:seq message)})

    handler))

(defn- create-scope [frame vars name hint]
  (let [id     (:id vars)
        values (:values vars)]
    (when (not-empty values)
      {:name name
       :presentationHint hint
       :variablesReference id
       :namedVariables (count values)
       :indexedVariables 0
       :expensive false
       :source (get-in frame [:source])
       :line (get-in frame [:extras :scope-start])})))

(defn handle-scopes [handler message]
  (let [frame-id     (get-in message [:arguments :frameId])
        frame        (first (filter #(= (:id %) frame-id) @(:stackframes handler)))
        stack-scope  (create-scope frame (get-in frame [:extras :stack]) "Stack" "locals")
        upvals-scope (create-scope frame (get-in frame [:extras :upvalues]) "Upvalues" "registers")]
    (adapter/send-message!
     (:adapter handler)
     (success (:seq message)
              "scopes"
              {:scopes (filter some? [stack-scope upvals-scope])}))
    handler))

(defn- find-vars-scope-by-id [data id]
  (some
   (fn [m]
     (or (when (= id (get-in m [:extras :stack :id]))
           (get-in m [:extras :stack]))
         (when (= id (get-in m [:extras :upvalues :id]))
           (get-in m [:extras :upvalues]))))
   data))

(defn- table-remove-types-not-to-be-shown [table]
  (filter #(not (#{:function} (second %))) table))

(defn- float-to-string [n]
  (.replaceAll (format "%.16f" n) "\\.?0*$" ""))

(defn- create-variables [vars]
  (map
   (fn [[k v]]
     (case (:type v)
       :constant {:name (name k)
                  :value (cond
                           (string?  (:value v)) (:value v)
                           (int?     (:value v)) (format "%i"     (:value v))
                           (float?   (:value v)) (float-to-string (:value v))
                           (number?  (:value v)) (float-to-string (:value v))
                           (boolean? (:value v)) (str (:value v))
                           :else     (str (:value v)))
                  :type (cond
                          (string?  (:value v)) "string"
                          (int?     (:value v)) "int"
                          (float?   (:value v)) "float"
                          (number?  (:value v)) "number"
                          (boolean? (:value v)) "boolean"
                          :else nil)
                  :evaluateName (name k)
                  :variablesReference (:id v)
                  :namedVariables 0
                  :indexedVariables 0
                  :presentationHint {:kind "data"
                                     :attributes ["constant"]}}
       :table    {:name (name k)
                  :value
                  (if (vector? (:value v))
                    (format
                     "[ %s ]"
                     (string/join ", " (:value v)))
                    (format
                     "{ %s }"
                     (string/join ", " (map (fn [[k v]] (str (name k) " = " v)) (table-remove-types-not-to-be-shown (:value v))))))
                  :type
                  (if (vector? (:value v)) "array" "object")
                  :evaluateName (name k)
                  :variablesReference (:id v)
                  :namedVariables   (if (map?    (:value v)) (count (table-remove-types-not-to-be-shown (:value v))) 0)
                  :indexedVariables (if (vector? (:value v)) (count (:value v))                0)
                  :presentationHint {:kind "data"
                                     :attributes (if (vector? (:value v)) "array" "rawObject")}}
       :function  nil
       (do (log/error "Unknown variable type:" (:type v) v)
           nil)))
   vars))

(defn handle-variables [handler message]
  (let [vars-id (get-in message [:arguments :variablesReference])
        vars    (or (:values (find-vars-scope-by-id @(:stackframes handler) vars-id))
                    (@var-index vars-id))]
    (adapter/send-message!
     (:adapter handler)
     (success (:seq message)
              "variables"
              {:variables (filter some? (create-variables vars))}))
    handler))

(defn handle-continue [handler message]
  (let [to-debug-server (get-in handler [:channels :to-debug-server])]
    (>!! to-debug-server {:cmd :set-breakpoints :breakpoints (:breakpoints handler)})
    (>!! to-debug-server {:cmd :run}))
  (adapter/send-message! (:adapter handler) (success (:seq message) "continue" nil))
  handler)

(defn handle-step-in [handler message]
  (let [to-debug-server (get-in handler [:channels :to-debug-server])]
    (>!! to-debug-server {:cmd :set-breakpoints :breakpoints (:breakpoints handler)})
    (>!! to-debug-server {:cmd :step-in})
    (adapter/send-message! (:adapter handler) (success (:seq message) "stepIn" nil))
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
    "scopes"            (handle-scopes handler message)
    "variables"         (handle-variables handler message)
    "continue"          (handle-continue handler message)
    "stepIn"            (handle-step-in handler message)
    "disconnect"        (handle-terminate handler message)
    "terminate"         (handle-terminate handler message)

    (do (log/error "Unknown Command Received:" message)
        handler)))

(defn run []
  (let [adapter (adapter/create-stdio-adapter)]
    (try
      (loop [handler (create-handler adapter)]
        (recur (if-let [message (adapter/read-message! (:adapter handler))]
                 (let [new-handler (handle-message handler message)]
                   (assert (some? new-handler)) ; TODO: check for some spec or something
                   new-handler)
                 handler)))

      (catch Throwable t
        (log/error "Handler: Something went wrong" (pst-str (parse-exception t)))
        (System/exit 1)))))

