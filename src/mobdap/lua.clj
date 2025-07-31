(ns mobdap.lua
  (:require
   [clj-stacktrace.core :refer [parse-exception]]
   [clj-stacktrace.repl :refer [pst-str]]
   [clojure.string :as string]
   [taoensso.timbre :as log])
  (:import
   [org.luaj.vm2 LuaTable]
   [org.luaj.vm2.lib.jse JsePlatform]))

(declare lua->clojure)

(defn- is-array? [^LuaTable table]
  (every? #(.isint %) (.keys table)))

(defn- reference? [value]
  (when (.isstring value)
    (re-matches #"(table|function|Script): 0x[a-z0-9]+" (str (.tostring value)))))

(defn- table->array [^LuaTable table]
  (let [keys (.keys table)]
    (if (and (= (.length table) 2) (reference? (.get table (last keys))))
      (lua->clojure (.get table (first keys)))
      (vec (map #(lua->clojure (.get table %)) keys)))))

(defn- table->map [^LuaTable table]
  (let [keys (.keys table)]
    (into {}
          (for [k keys]
            [(keyword (str k))
             (lua->clojure (.get table k))]))))

(defn- lua->clojure [value]
  (cond
    (nil? value) nil

    (.isstring value)
    (let [s (.tojstring value)]
      (if-let [r (reference? value)]
        (keyword (string/lower-case (last r)))
        (try
          (Integer/parseInt s)
          (catch NumberFormatException _ s))))

    (.isnumber value) (.todouble value)
    (.isboolean value) (.toboolean value)

    (.istable value)
    (if (is-array? value)
      (table->array value)
      (table->map value))

    (instance? org.luaj.vm2.LuaFunction value)
    :function))

(defn extract-table [lua-code]
  (try
    (let [globals (JsePlatform/standardGlobals)
          chunk (.load globals lua-code)]
      (-> (.call chunk)
          (lua->clojure)))
    (catch Throwable t
      (log/error "Lua Bridge:" (pst-str (parse-exception t)))
      [])))

