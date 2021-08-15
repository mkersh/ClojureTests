(ns type-specification.-spec-other 
(:require
 [orchestra.core :refer [defn-spec]] ;; orchestra is an optional extra but adds some nice features
 [orchestra.spec.test :as st]
 [clojure.spec.alpha :as s]
 [clojure.test.check.generators])
)

;; Just setting up a test to see if when you call (st/instrument) does it
;; work across all namespaces.
;; And the answer is yes it does.
(s/fdef variadic-fn
  :args (s/cat :nm string? :args (s/* (s/and integer? pos?)) )
  :ret string?)
(defn variadic-fn [nm & args]
  (str nm " " (apply + args))
  )

(comment
(st/instrument)
(st/unstrument)
(variadic-fn "hello" 1 2 3)
(variadic-fn "hello")
(variadic-fn "hello" -1)
(variadic-fn 1 "hello")
;
)
