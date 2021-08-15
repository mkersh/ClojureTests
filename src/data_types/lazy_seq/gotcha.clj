;;; Many sequences in Clojure are Lazy - http://clojure-doc.org/articles/language/laziness.html
;;; This has many advantages BUT you do have to be careful!!!
;;; I was caught out by the issue I will highlight below Aug 2020, whilst learning Clojure
;;; Summary of issue:
;;; When using map (and other sequence functions) they use lazy evaluation and you
;;; have to force evaluation using something like doall or dorun
(ns data-types.lazy-seq.gotcha
;;   (:require
;;    [clojure.pprint :as pp]
;;    )
  )


;; The next func appears to work
;; When you execute from a REPL 1,2,3,4 will be printed
;; 
;; It is only working though because the REPL is realising the result return which is the 
;; lazy seq returned by map. See this-does-not-realise-the-map below to understand more.
(defn this-appears-to-work []
  (map prn [1 2 3 4])
  )

(this-appears-to-work)


;; This is what caused me all my problems!!!
;; The map in the middle is not realised and nothing is printed out
;; This caused me hours of pain because it had been working because orignally the function 
;; had resembled this-appears-to-work function. I then changed it though so that the map
;; was not the last form and it then resembled this-does-not-realise-the-map
(defn this-does-not-realise-the-map []
  (prn "start")
  (map prn [1 2 3 4])
  (prn "end"))

(this-does-not-realise-the-map)


;; This is what you need to do to force lazy seqs to be evaluated
(defn this-does-realise-the-map []
  (prn "start")
  (doall (map prn [1 2 3 4]))
  ;; An alternative to doall is dorun. Difference is doall returns the realised results
  ;; whereas dorun throws results away. In general you probably want dorun because you are only 
  ;; forcing realisation for te side effects.
  (dorun (map prn ["a" "b" "c"]))
  (prn "end"))

(this-does-realise-the-map)
