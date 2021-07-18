(defproject run-length-encoding-notebook "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojars.benfb/gorilla-repl "0.7.0"  :exclusions [ring/ring-codec]]]
  :main ^:skip-aot run-length-encoding-notebook.core
  :plugins [[org.clojars.benfb/lein-gorilla "0.7.0"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
