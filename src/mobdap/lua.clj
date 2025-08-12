(ns mobdap.lua
  (:require
   [clj-stacktrace.core :refer [parse-exception]]
   [clj-stacktrace.repl :refer [pst-str]]
   [taoensso.timbre :as log])
  (:import
   [org.luaj.vm2 Globals LuaTable LuaValue]
   [org.luaj.vm2.lib.jse JsePlatform]))

(declare lua->clojure)

(defn- is-array? [^LuaTable table]
  (every? #(.isint ^LuaValue %) (.keys table)))

(defn- table->array [^LuaTable table]
  (let [keys (.keys table)]
    (vec (map #(lua->clojure (.get table ^LuaValue %)) keys))))

(defn- table->map [^LuaTable table]
  (let [keys (.keys table)]
    (into {}
          (for [k keys]
            [(keyword (str k))
             (lua->clojure (.get table ^LuaValue k))]))))

(defn- lua->clojure [^LuaValue value]
  (cond
    (nil? value) nil

    (.isstring value) (.tojstring value)

    (.isnumber value) (.todouble value)
    (.isboolean value) (.toboolean value)

    (.istable value)
    (if (is-array? value)
      (table->array value)
      (table->map value))

    (instance? org.luaj.vm2.LuaFunction value)
    :function))

(defn extract-table [^String lua-code]
  (try
    (let [^Globals globals (JsePlatform/standardGlobals)
          chunk            (.load globals lua-code)]
      (-> (.call chunk)
          (lua->clojure)))
    (catch Throwable t
      (log/error "Lua Bridge:" (pst-str (parse-exception t)))
      [])))

