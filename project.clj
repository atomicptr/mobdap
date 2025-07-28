(defproject mobdap "0.1.0"
  :description "Debug Adapter for MobDap"
  :url "https://github.com/atomicptr/mobdap"
  :license {:name "GPL-3.0-or-later" :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[cheshire/cheshire               "6.0.0"]
                 [com.taoensso/timbre             "6.7.1"]
                 [org.clojure/clojure             "1.12.1"]
                 [org.clojure/core.async          "1.8.741"]]
  :plugins      [[io.taylorwood/lein-native-image "0.3.1"]]
  :main ^:skip-aot mobdap.main
  :omit-source true
  :target-path "target/%s"
  :native-image {:opts ["--verbose"
                        "--report-unsupported-elements-at-runtime"
                        "--initialize-at-build-time"]}
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :native-image {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
