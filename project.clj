(defproject mobdap "0.1.4"
  :description "Debug adapter implementation for Lua / MobDap"
  :url "https://github.com/atomicptr/mobdap"
  :license {:name "GPL-3.0-or-later" :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[cheshire/cheshire                 "6.0.0"]
                 [org.clojure/tools.cli             "1.1.230"]
                 [clj-stacktrace                    "0.2.8"]
                 [com.taoensso/timbre               "6.7.1"]
                 [org.clojure/clojure               "1.12.1"]
                 [org.clojure/core.async            "1.8.741"]
                 [org.luaj/luaj-jse                 "3.0.1"]]
  :plugins      [[io.taylorwood/lein-native-image   "0.3.1"]
                 [lein-set-version/lein-set-version "0.4.1"]]
  :main ^:skip-aot mobdap.main
  :omit-source true
  :source-paths ["src"]
  :target-path "target/%s"
  :global-vars  {*warn-on-reflection* true}
  :profiles {:default
             {:main ^:skip-aot mobdap.main
              :set-version {:updates [{:path "src/mobdap/main.clj"
                                       :search-regex  #"mobdap-version \"\d+\.\d+\.\d+(-\w+)?\""
                                       :replace-regex #"\d+\.\d+\.\d+(-\w+)?"}]}}

             :dev
             {:main ^:skip-aot mobdap.dev
              :source-paths ["src" "dev"]
              :dependencies [[nrepl/nrepl "1.3.1"]]}

             :test
             {:dependencies [[nubank/matcher-combinators "3.9.1"]]}

             :uberjar
             {:main mobdap.main
              :aot :all
              :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
              :global-vars {*warn-on-reflection* false}}

             :native-image
             {:main mobdap.main
              :aot :all
              :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
              :global-vars {*warn-on-reflection* false}
              :native-image {:opts ["-H:ReflectionConfigurationFiles=resources/reflection.json"
                                    "--verbose"
                                    "--no-fallback"
                                    "--parallelism=32"
                                    "--initialize-at-build-time"]}}})
