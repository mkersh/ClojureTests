(ns debug_tools.print_classpath
  (:require
            [clojure.string :as str]
            [clojure.pprint :as pp]))

;; Print the classpath
(defn printClassPath []
  (println "JAVA CLASSPATH - Has the following configuration:")
  (println "===============")
  (pp/pprint (sort (str/split
                    (System/getProperty "java.class.path")
                    #":"))))
;; To test
(comment
(printClassPath)
)










;; OLD way of doing it that stopped working it newer versions of Java
;; https://stackoverflow.com/questions/46694600/java-9-compatability-issue-with-classloader-getsystemclassloader
;; https://blog.codefx.org/java/java-11-migration-guide/#Casting-To-URL-Class-Loader
(defn XXprintClassPath []
  (println (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))))

