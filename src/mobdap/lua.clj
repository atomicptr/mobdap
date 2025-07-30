(ns mobdap.lua
  (:import [org.luaj.vm2 LuaValue]
           [org.luaj.vm2.lib.jse JsePlatform]))

(defn- lua-to-clojure [lua-value]
  (cond
    (.istable lua-value)
    (let [keys (.keys lua-value)
          numeric? (every? #(.isnumber %) keys)]
      (if numeric?
        (mapv (fn [i] (lua-to-clojure (.get lua-value (LuaValue/valueOf i))))
              (sort (map #(.toint %) keys)))
        (into {}
              (for [k keys
                    :let [v (.get lua-value k)]
                    :when (not (or (.isfunction v) (.isclosure v) (.isuserdata v)
                                   (and (.isstring v) (re-matches #"^(table|function|Script): 0x[0-9a-f]+" (.tojstring v)))))]
                [(lua-to-clojure k) (lua-to-clojure v)]))))
    (.isstring lua-value) (.tojstring lua-value)
    (.isnumber lua-value) (.todouble lua-value)
    (.isboolean lua-value) (.toboolean lua-value)
    (.isnil lua-value) nil
    :else nil))

(defn extract-table [lua-code]
  (let [globals (JsePlatform/standardGlobals)
        chunk (.load globals lua-code)]
    (-> (.call chunk)
        (lua-to-clojure))))
