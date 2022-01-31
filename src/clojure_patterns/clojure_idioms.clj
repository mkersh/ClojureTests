;;; Plan to use this file to record less than obvious (for the beginner) tips and tricks when using closure
;;; see also my gotcha.clj
;;;
;;; 31 Jan 2022: This file has never really taken off as my single place for 
;;;      recording less-than-obvious clojure-idioms, which was the original plan.

(ns clojure-patterns.clojure-idioms)


;;; AND and OR can do more than you think in clojure

;; Using OR to provide a default if a function returns nil or false
(defn some-func [x]
(prn "some-func called with: " x) ;; For debug to see what functions are called
(condp = x
  1 "Value 1"
  2 nil
  3 false
  )
)

;; Idiom in clojure is to wrap a function call inside an or
;; If the function returns nil or false an alternative will be called
(or (some-func 1) "Default")

;; This next call will return the default
(or (some-func 2) "Default")

;; As will this one
(or (some-func 3) "Default")

;; The or can have more than one alternatve to try

(or (some-func 2) (some-func 3) "Default")
(or (some-func 2) (some-func 3) (some-func 1) "Default")

;; or is a special form in clojure
;; its arguments are only evaluated if needed

;; Look at the output for the following
;; only (some-func 1) is called
(or (some-func 1) (some-func 2) (some-func 3)  "Default")


;; Here's another example of the above idiom being used with maps

(def some-map {:field1 1 :field2 2 :field3 nil :field4 false})

(or (:field1 some-map) "default" )
(or (:field3 some-map) "default")
(or (:fiel4 some-map) "default")


;; AND will only execute its parameters if the previous parameter
;; was not false or nil

;; So in below only (some-func 2) is executed
(and (some-func 2) (some-func 3))

;; (some-func 1) will be called twice below and also (some-func 3)
(and (some-func 1) (some-func 1) (some-func 3))

;; We could use and to achieve the same as dorun or doall

;; dorun or doall are needed in the below
(defn this-does-not-realise-the-map []
  (prn "start")
  (map prn [1 2 3 4])
  (prn "end"))

(this-does-not-realise-the-map)

;; Here's how to make it work with doall
(defn this-does-realise-the-map1 []
  (prn "start")
  (doall (map prn [1 2 3 4]))
  (prn "end"))

(this-does-realise-the-map1)

;; dorun is an alernative way to get it working
(defn this-does-realise-the-map2 []
  (prn "start")
  (dorun (map prn [1 2 3 4]))
  (prn "end"))

(this-does-realise-the-map2)

;; THOUGHT the below might work but it doesn't:
;;
;; You could however also use an and to force the lazy seq
;; to be evaluated
;; UPDATE: No you can't because and is a macro and you
;; can't do the following

(defn this-does-realise-the-map3 []
  (prn "start")
  ;;(apply and (map prn [1 2 3 4])) ;; This would generate a compil error
  (prn "end"))

;;(this-does-realise-the-map3)

