#!/usr/bin/env bb

(require '[cheshire.core :as json])

(def classes
  ["java.io.BufferedReader"
   "java.io.BufferedWriter"
   "java.io.InputStreamReader"
   "java.io.PrintWriter"
   "java.net.ServerSocket"
   "java.net.Socket"
   "org.luaj.vm2.Globals"
   "org.luaj.vm2.LuaBoolean"
   "org.luaj.vm2.LuaClosure"
   "org.luaj.vm2.LuaDouble"
   "org.luaj.vm2.LuaFunction"
   "org.luaj.vm2.LuaInteger"
   "org.luaj.vm2.LuaNil"
   "org.luaj.vm2.LuaNumber"
   "org.luaj.vm2.LuaString"
   "org.luaj.vm2.LuaTable"
   "org.luaj.vm2.LuaValue"
   "org.luaj.vm2.lib.Bit32Lib$Bit32Lib2"
   "org.luaj.vm2.lib.Bit32Lib$Bit32LibV"
   "org.luaj.vm2.lib.IoLib$IoLibV"
   "org.luaj.vm2.lib.jse.LuajavaLib"])

(defn generate-entry [item]
  {:name item
   :allDeclaredConstructors true
   :allPublicConstructors true
   :allDeclaredMethods true
   :allPublicMethods true})

(let [classes (map generate-entry classes)
      classes (json/generate-string classes {:pretty true})]
  (spit "resources/reflection.json" classes))
