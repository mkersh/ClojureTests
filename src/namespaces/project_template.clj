(defproject clojure-notebook "0.0.1"
  :description "My clozure tests"
  :url "https://github.com/MkershMambu"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0" 
  :uberjar-name "mk-mini-proj-standalone.jar"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "1.3.610"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/data.json "1.0.0"]
                 [http-kit "2.4.0"]
                 [clj-wamp "1.0.0-rc1"]
                 [clj-http "3.10.1"]
                 [clj-pdf "2.5.7"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [dk.ative/docjure "1.14.0"]
                 [org.clojars.bpsm/big-ivan "0.1.0"]
                 [org.clojure/test.check "1.1.0"]
                 [org.clojure/tools.namespace "1.2.0"]]
  :profiles {:replxx {:plugins [[cider/cider-nrepl "0.27.2"]]}
             :dev {:resource-paths ["resources-dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.3"]]
                   :jvm-opts ["-Xmx1g" "-server"
                              "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]}
             :production {:resource-paths ["resources-prod"]}}
  :test-paths ["test" "src"]
  ; Setting this to ClojureNotebook breaks the REPL load
  :xaot :all
  :main repl_start)
