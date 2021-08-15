;;; Namespace names need to match file name
;;; Required namespaces need to be on the classpath
;;;     - This is a bit messy for my tests
(ns namespaces.ns2
   (:require [namespaces.ns1 :as req1]
             [clojure.string :as str]
             [clojure.pprint :as pp])
  )
;; Here's another way of requiring another namespace
;; but you are better off using :require within (ns ...)
;(require '[namespaces.ns1 :as req1])

;; OLD way of doing it that stopped working it newer versions of Java
;; https://stackoverflow.com/questions/46694600/java-9-compatability-issue-with-classloader-getsystemclassloader
;; https://blog.codefx.org/java/java-11-migration-guide/#Casting-To-URL-Class-Loader
(defn XXprintClassPath []
  (println (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))))

;(require '[clojure.string :as str])
;; Print the classpath
(defn printClassPath []
  (println "JAVA CLASSPATH - Has the following configuration:")
  (println "===============")
  (pp/pprint (sort (str/split
   (System/getProperty "java.class.path")
   #":")))
)

;(printClassPath)

(defn ns2SayHello []
  (println "MKNameSpace2 Says Hello from me AND!!!")
  (req1/ns1SayHello)
  )


(ns2SayHello)
 ;; => Syntax error compiling at (namespaces/ns2.clj:1:8244).
 ;;    Unable to resolve symbol: ns2SayHello in this context

*ns*