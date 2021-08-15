(defproject clojure-notebook "0.0.1"
  :description "My clozure tests"
  :url "https://github.com/mkersh/JupyterNotebooks/tree/master/ClojureTests"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :uberjar-name "mk-clojure-tests-standalone.jar"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [log4j "1.2.17" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.apache.cxf/cxf-xjc-plugin "3.2.0"]
                 [ring-server "0.2.8"]
                 [lib-noir "0.5.5"]
                 [compojure "1.1.5"]
                 [clabango "0.5"]
                 [org.clojure/data.json "1.0.0"]
                 [http-kit "2.4.0"]
                 [clj-wamp "1.0.0-rc1"]
                 [clj-http "3.10.1"]
                 [dk.ative/docjure "1.14.0"]]
  :profiles {:dev {:resource-paths ["resources-dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.3"]]
                   :jvm-opts ["-Xmx1g" "-server"
                              "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]}
             :dev2 {:resource-paths ["resources-dev"]
                    :dependencies [[org.clojure/tools.namespace "0.2.3"]
                                   [org.clojure/tools.nrepl "0.2.13"]
                                   [nrepl/nrepl "0.8.2"]]
                    :jvm-opts ["-Xmx1g" "-server"
                               "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8030"]}
             :production {:resource-paths ["resources-prod"]}}
  :test-paths ["test" "src"]
  ; Setting this to ClojureNotebook breaks the REPL load
  :xaot :all
  :main tools.repl-navigate
  )
