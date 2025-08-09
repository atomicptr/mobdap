(ns mobdap.handler
  (:require
   [clj-stacktrace.core :refer [parse-exception]]
   [clj-stacktrace.repl :refer [pst-str]]
   [clojure.core.async :refer [<!! >!! chan go-loop]]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [mobdap.adapter :as adapter]
   [mobdap.debug-server :as debug-server]
   [mobdap.utils :refer [float-to-string map-over-map to-int]]
   [taoensso.timbre :as log]))

(def ^:private go-handler (atom nil))

(defn- create-handler [adapter]
  {:adapter adapter
   :debug-server nil
   :root-dir     nil
   :source-dirs  []
   :breakpoints  {}
   :channels     {:to-handler (chan)
                  :to-debug-server nil}
   :counter      {:breakpoint (atom 0)
                  :stackframe (atom 0)
                  :vars       (atom 0)}
   :stackframes  (atom [])
   :var-index    (atom {})})

(defn- find-source-dir [handler filepath]
  (let [file (io/file filepath)]
    (cond
      (and (:root-dir handler)
           (.isAbsolute file)
           (.exists file)
           (.startsWith (.getCanonicalPath file) (:root-dir handler)))
      (:root-dir handler)

      (and (:root-dir handler)
           (not (.isAbsolute file))
           (.exists (io/file (:root-dir handler) file)))
      (str (io/file (:root-dir handler)))

      :else
      (first (filter #(.startsWith (.getCanonicalPath file) (.getCanonicalPath (io/file %))) (:source-dirs handler))))))

(defn- find-source-file [handler filepath]
  (let [file (io/file filepath)]
    (cond
      (.isAbsolute file)
      (.getCanonicalPath file)

      (and (:root-dir handler)
           (.exists (io/file (:root-dir handler) file)))
      (.getCanonicalPath (io/file (:root-dir handler) file))

      :else
      (->> (:source-dirs handler)
           (map #(io/file % filepath))
           (filter #(.exists %))
           (map #(.getCanonicalPath %))
           (first)))))

(defn- find-source-relative-file [handler filepath]
  (let [source-dir (.getCanonicalPath (io/file (find-source-dir handler filepath)))
        filepath   (.getCanonicalPath (io/file filepath))]
    (assert (some? source-dir))
    (assert (some? filepath))
    (string/replace-first (subs filepath (count source-dir)) #"^\/+" "")))

(defn- find-breakpoint-id [handler file line]
  (let [breakpoints (get-in handler [:breakpoints file])]
    (when breakpoints
      (->> breakpoints
           (filter #(= (to-int line) (:line %)))
           first
           :id))))

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

(defn- send-command-to-debug-server
  ([handler command] (send-command-to-debug-server handler command false))
  ([handler command update-breakpoints?]
   (let [to-debug-server (get-in handler [:channels :to-debug-server])]
     (assert (some? to-debug-server))

     (when update-breakpoints?
       (>!! to-debug-server {:cmd :set-breakpoints :breakpoints (:breakpoints handler)}))

     (>!! to-debug-server command))))

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

(defn- parse-heap-value [ident]
  (when-let [[_ type addr] (re-find #"(.+):\s*(0x[A-Za-z0-9]+)" ident)]
    {:type (keyword type)
     :addr addr}))

(defn- parse-inner-value [handler name value]
  (cond
    (vector? value)
    {:id (swap! (get-in handler [:counter :vars]) inc)
     :name name
     :type :table
     :addr nil
     :value (vec (map (partial parse-inner-value handler nil) value))}

    (map?    value)
    {:id (swap! (get-in handler [:counter :vars]) inc)
     :name name
     :type :table
     :addr nil
     :value (map-over-map (fn [k v] (parse-inner-value handler k v)) value)}

    :else
    {:name name
     :type :constant
     :value value}))

(defn- parse-value [handler var-name packed-value]
  (case (count packed-value)
    1 {:name var-name
       :type :constant
       :value nil}
    2 (let [[value ident] packed-value]
        (if-let [{type :type addr :addr} (parse-heap-value ident)]
          {:id   (swap! (get-in handler [:counter :vars]) inc)
           :name var-name
           :type type
           :addr addr
           :value (case type
                    :table (if (vector? value)
                             (vec (map-indexed (partial parse-inner-value handler) value))
                             (map-over-map (fn [k v] (parse-inner-value handler k v)) value))
                    value)}
          {:name var-name
           :type :constant
           :value value}))
    (do (log/error "Could not unpack" packed-value)
        nil)))

(defn- register-vars [handler vars]
  (doseq [[_ v] (if (map? vars) vars (zipmap (range (count vars)) vars))]
    (when (:id v)
      (swap! (:var-index handler) assoc (:id v) v))
    (when (= :table (:type v))
      (register-vars handler (:value v)))))

(defn- transform-stack-trace [handler stack-trace]
  (map-indexed
   (fn [_ [frame-info stack upvalues]]
     (let [parse-value (partial parse-value handler)
           stack       (map-over-map parse-value stack)
           upvalues    (map-over-map parse-value upvalues)]

       (register-vars handler stack)
       (register-vars handler upvalues)

       (case (count frame-info)
         ; example of the frame header (6):
         ;   0: "modules/tbl.lua"   - path for the function
         ;   1: 6                   - line where function gets declared
         ;   2: 13                  - breakpoint? entry point?
         ;   3: "Lua"               - Lua (thats where it comes from ig)
         ;   4: "field"             - ??? this can also be "upvalue"
         ;   5: "modules/tbl.lua"   - file location again
         6 {:id (swap! (get-in handler [:counter :stackframe]) inc)
            :name ""
            :source {:path (find-source-file handler (frame-info 0))}
            :line (parse-long (frame-info 2))
            :column 0
            :extras {:scope-start (parse-long (frame-info 1))
                     :stack       {:id     (swap! (get-in handler [:counter :vars]) inc)
                                   :values stack}
                     :upvalues    {:id     (swap! (get-in handler [:counter :vars]) inc)
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
         7 {:id (swap! (get-in handler [:counter :stackframe]) inc)
            :name (or (frame-info 0) "")
            :source {:path (find-source-file handler (frame-info 1))}
            :line (parse-long (frame-info 3))
            :column 0
            :extras {:scope-start (parse-long (frame-info 2))
                     :stack       {:id     (swap! (get-in handler [:counter :vars]) inc)
                                   :values stack}
                     :upvalues    {:id     (swap! (get-in handler [:counter :vars]) inc)
                                   :values upvalues}
                     :type        (frame-info 5)}}

         (do (log/error "Could not transform stack trace" frame-info)
             nil))))
   stack-trace))

(defn- handle-debug-server-command [handler command]
  (log/info "Handler: Received command from debug server:" command)
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
                                :hitBreakpointIds (vec (filter some? [id]))})))

               (:step :out :over)
               (adapter/send-message!
                (:adapter handler)
                (event
                 "stopped"
                 {:reason "step"
                  :allThreadsStopped true
                  :threadId 1}))

               (log/error "Unknown :stopped command:" (:type command)))

    :stacktrace (let [seq   (:seq   command)
                      stack (:stack command)
                      stack (transform-stack-trace handler stack)]
                  (log/debug "Stacktrace:" stack)
                  (reset! (:stackframes handler) stack)
                  (adapter/send-message! (:adapter handler) (success seq "stackTrace" {:stackFrames (map #(dissoc % :extras) stack)
                                                                                       :totalFrames (count stack)})))

    (log/error "Unknown server command:" (:cmd command))))

(defn handle-launch [handler message]
  (let [adapter     (:adapter handler)
        port        (or (get-in message [:arguments :port]) 8172)
        arguments   (:arguments message)
        root-dir    (:rootdir arguments)
        source-dirs (or (:sourcedirs arguments) [])
        to-handler (get-in handler [:channels :to-handler])]
    (log/info "Handler: Waiting for client on port" port)

    (let [server-channel (debug-server/run-server! (int port) to-handler)]
      (log/info "Handler: Waiting for server to finish setup...")

      ; send a special event indicating to the editor that it could start the game now
      (adapter/send-message! adapter (event "mobdap_waiting_for_connection"))

      (go-loop [setup-done false]
        (cond
          (not setup-done)
          (let [response (<!! to-handler)]
            (if (= :setup-done (:cmd response))
              (do
                (adapter/send-message! adapter (success (:seq message) "launch" nil))
                (adapter/send-message! adapter (event "initialized"))
                (log/info "Handler: Finished server setup")
                (recur true))
              (recur false)))

          :else
          (do (when-let [command (<!! to-handler)]
                (try
                  (handle-debug-server-command @go-handler command)
                  (catch Throwable t
                    (log/error "Message from debug server handler" (pst-str (parse-exception t))))))
              (recur true))))

      (-> handler
          (assoc    :root-dir    root-dir)
          (assoc    :source-dirs source-dirs)
          (assoc-in [:channels :to-debug-server] server-channel)))))

(defn handle-set-breakpoints [handler message]
  (let [adapter (:adapter handler)
        {:keys [source breakpoints]} (:arguments message)
        source-path (:path source)
        filename (find-source-relative-file handler source-path)
        breakpoints (mapv #(assoc % :verified true :id (swap! (get-in handler [:counter :breakpoint]) inc)) breakpoints)]
    (adapter/send-message! adapter (success (:seq message) "setBreakpoints" {:breakpoints breakpoints}))
    (-> handler
        (assoc-in [:breakpoints (str filename)] breakpoints))))
;
(defn handle-configuration-done [handler message]
  (adapter/send-message! (:adapter handler) (success (:seq message) "configurationDone" nil))
  (send-command-to-debug-server handler {:cmd :run} true)
  handler)

(defn handle-threads [handler message]
  ; lua is single threaded
  (adapter/send-message! (:adapter handler) (success (:seq message) "threads" {:threads [{:id 1 :name "main"}]}))
  handler)

(defn handle-stacktrace [handler message]
  (let [start-frame (get-in message [:arguments :startFrame])
        levels      (get-in message [:arguments :levels])]
    (send-command-to-debug-server handler {:cmd :stacktrace :start start-frame :length levels :seq (:seq message)})
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
              {:scopes (vec (filter some? [stack-scope upvals-scope]))}))
    handler))

(defn- find-vars-scope-by-id [data id]
  (some
   (fn [m]
     (or (when (= id (get-in m [:extras :stack :id]))
           (get-in m [:extras :stack]))
         (when (= id (get-in m [:extras :upvalues :id]))
           (get-in m [:extras :upvalues]))))
   data))

(defn- var-name [n]
  (cond
    (keyword? n) (name n)
    (string?  n) n
    (number?  n) (str n)
    :else (str n)))

(defn- var-value [v]
  (cond
    (string?  v) (str v)
    (int?     v) (format "%i"     v)
    (float?   v) (float-to-string v)
    (number?  v) (float-to-string v)
    (boolean? v) (str v)
    (vector?  v) (format "[%s]" (string/join ", " (map #(var-value (:value %)) v)))
    (map?     v) (format "{%s}" (string/join ", " (map (fn [[k v]] (str (var-name k) " = " (var-value (:value v)))) v)))
    :else        (str v)))

(defn- var-type [v]
  (cond
    (string?  v) "string"
    (int?     v) "int"
    (float?   v) "float"
    (number?  v) "number"
    (boolean? v) "boolean"
    (nil?     v) "null"
    :else nil))

(defn- var-value-table [v]
  (cond
    (vector? v)
    (format
     "[%s]"
     (string/join ", " (map #(var-value (:value %)) v)))

    (map? v)
    (format
     "{%s}"
     (string/join ", " (map (fn [[k v]] (str (var-name k) " = " (var-value (:value v)))) v)))

    (empty? v)
    "{}"

    :else (throw (ex-info (str "expected vector or map, got: " (pr-str v)) {:value v}))))

(defn- create-scope-variables [vars]
  (map
   (fn [[k v]]
     (case (:type v)
       :constant {:name (var-name k)
                  :value (var-value (:value v))
                  :type (var-type (:value v))
                  :evaluateName (var-name k)
                  :variablesReference 0
                  :namedVariables 0
                  :indexedVariables 0
                  :presentationHint {:kind "data"
                                     :attributes ["constant"]}}

       :table    {:name (var-name k)
                  :value (var-value-table (:value v))
                  :type (if (vector? (:value v)) "array" "object")
                  :evaluateName (var-name k)
                  :variablesReference (:id v)
                  :namedVariables   (if (map?    (:value v)) (count (:value v)) 0)
                  :indexedVariables (if (vector? (:value v)) (count (:value v)) 0)
                  :presentationHint {:kind "data"
                                     :attributes (if (vector? (:value v)) "array" "rawObject")}}

       :function  {:name (var-name k)
                   :value (format "function: %s" (:addr v))
                   :type "function"
                   :variablesReference 0
                   :presentationHint {:kind "method"}}

       (do (log/warn "Unknown variable type found:" k v)
           {:name  (var-name k)
            :value (format "%s: %s" (var-name (:type v)) (:addr v))
            :variablesReference 0
            :type "unknown"})))
   vars))

(defn- make-var [pname indexed? n value]
  (let [fstr (str (var-name pname) (if indexed? "[%s]" ".%s"))
        v    (:value value)]
    (cond
      (vector? v)
      {:name (var-name n)
       :value (var-value-table v)
       :type "array"
       :evaluateName (format fstr n)
       :variablesReference (:id value)
       :presentationHint {:kind "data"
                          :attributes ["array"]}}

      (map? v)
      {:name (var-name n)
       :value (var-value-table v)
       :type "object"
       :evaluateName (format fstr n)
       :variablesReference (:id value)
       :presentationHint {:kind "data"
                          :attributes ["rawObject"]}}

      :else
      {:name (var-name n)
       :value (var-value v)
       :type (var-type v)
       :evaluateName (format fstr n)
       :variablesReference 0
       :presentationHint {:kind "data"}})))

(defn- create-variables [vars]
  (case (:type vars)
    :table (map-indexed
            (if (vector? (:value vars))
              (fn [idx v]   (make-var (:name vars) true (str idx) v))
              (fn [_ [k v]] (make-var (:name vars) false (var-name k) v)))
            (:value vars))
    [nil]))

(defn- find-var-by-id [handler var-id]
  (let [scope (:values (find-vars-scope-by-id @(:stackframes handler) var-id))
        vars  (@(:var-index handler) var-id)]
    (cond
      scope [scope :scope]
      vars  [vars  :vars]
      :else [nil nil])))

(defn handle-variables [handler message]
  (let [vars-id     (get-in message [:arguments :variablesReference])
        [vars type] (find-var-by-id handler vars-id)]
    (assert (and (some? vars) (some? type)))

    (let [variables
          (vec (filter some? (case type
                               :scope (create-scope-variables vars)
                               :vars  (create-variables vars)
                               :else  [])))]

      (adapter/send-message!
       (:adapter handler)
       (success (:seq message)
                "variables"
                {:variables variables})))
    handler))

(defn handle-continue [handler message]
  (send-command-to-debug-server handler {:cmd :run} true)
  (adapter/send-message! (:adapter handler) (success (:seq message) "continue" nil))
  handler)

(defn handle-step-in [handler message]
  (send-command-to-debug-server handler {:cmd :step-in} true)
  (adapter/send-message! (:adapter handler) (success (:seq message) "stepIn" nil))
  handler)

(defn handle-step-out [handler message]
  (send-command-to-debug-server handler {:cmd :step-out} true)
  (adapter/send-message! (:adapter handler) (success (:seq message) "stepOut" nil))
  handler)

(defn handle-next [handler message]
  (send-command-to-debug-server handler {:cmd :over} true)
  (adapter/send-message! (:adapter handler) (success (:seq message) "next" nil))
  handler)

(defn handle-terminate [handler message]
  (send-command-to-debug-server handler {:cmd :exit})
  (adapter/send-message! (:adapter handler) (success (:seq message) "terminate" nil))
  handler)

(defn handle-message [handler message]
  (log/info "Handler: Incoming message:" message)

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
    "stepOut"           (handle-step-out handler message)
    "next"              (handle-next handler message)
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

