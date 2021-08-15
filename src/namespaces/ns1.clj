;;; Namespace tests
(ns namespaces.ns1)

(defn ns1SayHello []
  (println "MKNameSpace1 Says Hello World!!!"))


(ns1SayHello)

;;; Common namespace expressions:
;;;     (require '[clojure.string :as str])
;;;     (in-ns 'ClojureNotebook)
;;;     (ns-name *ns*)
;;;     (ns-interns *ns*)
;;;     (all-ns)

*ns*