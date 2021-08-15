;;; Clojure is a dynamically typed language and does not require you to specify
;;; the types of function parameters and types that a datastructure is made up of
;;; This approach is the opposite to statically typed languages like java, C++, haskell, ...
;;;
;;; clojure.spec is a library that allows you to add type information to functions and data.
;;; This file contains my tests/experiments in using it.
;;; 
;;; See Also
;;; https://clojure.org/guides/spec 
;;; https://blog.jeaye.com/2017/05/31/clojure-spec/
;;;
;;; TESTS
;;; [A] - Life before core.spec
;;; [B] - Using core.spec to specify a functions parameters and return value
;;; [C] - Using orchestra.core higher level defn-spec
;;; [D] - Using core.spec to specify maps
;;; [E] - How do you spec anonymous functions?
;;; [F] - Specifying maps (part2)
;;; [G] - Specifying Collections
;;; [H] - Printing out and testing specs
;;;
;;;
;;; TIPS
;;; (1) To turn on automatic checking of function specs call (st/instrument)
;;;     This will then automatically check functions that have a corresponding s/fdef (see [B] below for first examples)
;;;     Turn off instrumentation using (st/unstrument)

(ns type-specification.using-spec
  (:require
   [orchestra.core :refer [defn-spec]] ;; orchestra is an optional extra but adds some nice features
   [orchestra.spec.test :as st]
   [clojure.spec.alpha :as s]
   [clojure.test.check.generators]
   [clojure.walk]
   ))

;;; --------------------------------------------------------------
;;; [A] - Life before core.spec 

;; standard functions in clojure are not typed
;; This doesn't mean that the types passed to parameters are not important though
;; It just means that if the paramers passed are wrong the errors will be detected
;; dynamically by the function failing and throwing an exception

(defn inc-by-two [n]
  (+ n 2))

(comment

;; The function will work for a whole range of number types
  (inc-by-two 3)
  (inc-by-two 3.1)
  (inc-by-two 3N)

;; If you pass in a non-number though you will get an exception
  (inc-by-two "hhh")
;; If you execute the above you will get a runtime error like:
; Execution error (ClassCastException) at type-specification.using-spec/inc-by-two (using_spec.clj:16).
; java.lang.String incompatible with java.lang.Number
  )

;; Seeing if primitive type hints allow us to restrict argument types
;; See also: https://github.com/clojure-cookbook/clojure-cookbook/blob/master/08_deployment-and-distribution/8-05_type-hinting.asciidoc 
(defn inc-by-two2 [^Integer n]
  (+ n 2))

(comment
  (inc-by-two2 3)
  (inc-by-two2 3.1) ;; In my REPL I get a clj-kondo warning but the code can still be executed. At least in the REPL and will work
  (inc-by-two2 (/ 3 5))
  (inc-by-two2 3N)

;; These next 2 are the same as passing the 3.1 above I get a lint warning but in the REPL I can still execute and it will get dynamic error
  (inc-by-two2 {})
  (inc-by-two2 "sgsgs"))

;;; --------------------------------------------------------------
;;; [B] - Using core.spec to specify a defn parameters and return  

;; So how do you use clojure's core.spec to specify the valid types
;; that can be passed as arguments to the function
(defn inc-by-two3 [n]
  (+ n 2))

;; Here's a spec for the function above BUT it will not be checked automatically
;; One way to check automatically is to us the orchestra library - https://github.com/jeaye/orchestra#usage
;; I have enabled this orchestration (see requires above). To enable you need to execute (st/instrument) below
(s/fdef inc-by-two3
  :args (s/cat :x number?)
  :ret number?)
;; Interestingly the s/fdef for the function the function can be before or after the actual function definition

(comment
;; Once you've started orchestrate, functions with specs will be checked automatically
  (st/instrument)
;; Turn the instrumentation off
;; Re-compiling the namespace will also remove the instrumentation
  (st/unstrument)

  (inc-by-two3 3)
  (inc-by-two3 3.1) ;; In my REPL I get a clj-kondo warning but the code can still be executed. At least in the REPL and will work
  (inc-by-two3 (/ 3 5))
  (inc-by-two3 3N)

;; These 2 will fail
  (inc-by-two3 {})
  (inc-by-two3 "sgsgs"))

;; Showing that the spec can be before the definition
(s/fdef inc-by-two4
  :args (s/cat :x number?)
  :ret number?)
(defn inc-by-two4 [n]
  (+ n 2))

(comment
  (st/instrument)
  (inc-by-two4 3)
  (inc-by-two4 "hhh"))

;; How do you specify a Variadic function in spec
;; https://blog.taylorwood.io/2017/10/15/fspec.html

(s/fdef variadic-fn
  :args (s/cat :nm string? :args (s/* (s/and integer? pos?)) )
  :ret string?)
(defn variadic-fn [nm & args]
  (str nm " " (apply + args))
  )

(comment
(st/instrument)
(variadic-fn "hello" 1 2 3)
(variadic-fn "hello")
(variadic-fn "hello" -1)
(variadic-fn 1 "hello")

;; First time I have used the next function to generative test the functions
(s/exercise-fn `variadic-fn)
)

;; Test whether (st/instrument) affects all namespace or just current
;; Goto -spec-other.clj after setting (st/instrument) here

(comment

;; Test whether call instrucment (and unstrument affects only this namespace or all)
(st/instrument)
(st/unstrument)
;; Goto _spec_other.clj and see if 

;
)

(s/def ::name-or-id (s/or :name string?
                          :id   int?))
(s/fdef simple-fn
  :args (s/* ::name-or-id) ;; You can define the args as a s/coll-of or s/*
  :ret (s/coll-of any?))
(defn simple-fn [& args]
  args)

(comment
(st/instrument)
(simple-fn "hello" "hhh" 3)
)

;; Specifiying a function that can take an arbitrary number of keywords + integer pairs
(s/def ::keyword-and-integer (s/cat :k keyword? :i integer?))
(s/def ::keyword-and-optional-integer (s/cat :k keyword? :i (s/? integer?)))
(s/def ::seq-of-keywords-and-integers (s/* (s/cat :k keyword? :i integer?)))
(s/def ::seq-of-keywords-and-integers2 (s/* ::keyword-and-integer))
(s/def ::seq-of-keywords-and-opt-integers (s/* ::keyword-and-optional-integer))
(s/fdef simple-fn2
  :args (s/* ::keyword-and-integer) 
  :ret (s/coll-of any?))
(defn simple-fn2 [& args]
  args)
(comment
(st/instrument)
(s/conform ::seq-of-keywords-and-integers [:k1 2 :k2 3 5])
(s/conform ::seq-of-keywords-and-integers2 [:k1 2 :k2 3])
(s/conform ::seq-of-keywords-and-opt-integers [:k1 2 :k2 3 :k3 :k4])
(simple-fn2 :k1 1 :k2 3 :k3 4)
;
)


;;; --------------------------------------------------------------
;;; [C] - Using orchestra.core higher level defn-spec 

;; orchestra.core also provides a higher level abstraction defn-spec
;; Let's take a look at this
(defn-spec inc-by-two5 number? [n number?]
  (+ n 2))

(comment
  (st/instrument) ;; You do still need to start orchestration before the checking against the specs will happen
  (inc-by-two5 3)
  (inc-by-two5 "hhhh"))

;; Problems with defn-spec
;; 1) It is non-standard which means will make code more complex to read
;; 2) It is already above playing havoc with my clj-kondo lint errors. kondo is getting very confused
;; 3) There are multiple versions of defn-spec
;;    There is the orchestra version that I am using above and https://github.com/danielcompton/defn-spec


;; Test to see whether the spec predicate can be a function
;; For complex maps I think this may be best. Will test this shortly

;; Changing the definition of parameter-check will not cause inc-by-two6 to change imediately
;; You have to reload this function definition as well
(defn parameter-check [x] (number? x))
(defn-spec inc-by-two6 number? [n parameter-check]
  (+ n 2))

(comment
  (st/instrument) ;; You do still need to start orchestration before the checking against the specs will happen
  (inc-by-two6 3)
  (inc-by-two6 3.1)
  (inc-by-two6 "hhhh"))


;;; --------------------------------------------------------------
;;; [D] - Using core.spec to specify maps 

;; Now let's start to look at something more interessting - How to specify the format of more complex EDN datastructures
;; This is the real reason I am interested in core.spec.

(defn-spec func-with-map-arg string? [context map?]
  (:required-arg1 context))

(comment
  (st/instrument)
  (func-with-map-arg 1)
  (func-with-map-arg "hhh")
  ;; Next one passes a map but the result is not a string? 
  (func-with-map-arg {})
  ;
  )

;; To be useful though we need to go into more details regarding the structure of the map

;; Here's our first simple definition of a map
;; Spec requires keywords to be namespace qualifield
;; If you want to avoid this use :: and :req-un and opt-un in s/keys
(s/def ::map1
  (s/keys :req-un [::required-arg1]))

(defn-spec func-with-map-arg2 string? [context ::map1]
  (str (:required-arg1 context)))


(comment
  (st/instrument)
  (func-with-map-arg2 1)
  (func-with-map-arg2 "hhh")
  ;; Next one passes a map but the result is not a string? 
  (func-with-map-arg2 {})
  (func-with-map-arg2 {:required-arg1 "hello world"}) ;; I can only use unqualified :required-arg1 here because I used :req-un in (s/keys :req-un [::required-arg1])
  (func-with-map-arg2 {:required-arg1 1})
  ;
  )

;; Quickly showing the difference if you use :req in s.keys
(s/def ::map2
  (s/keys :req [::required-arg1]))

(defn-spec func-with-map-arg3 string? [context ::map2]
  (::required-arg1 context))

(comment
  (st/instrument)
  (func-with-map-arg3 1)
  (func-with-map-arg3 "hhh")
  ;; Next one passes a map but the result is not a string? 
  (func-with-map-arg3 {})
  (func-with-map-arg3 {:required-arg1 "hello world"}) ;; I can only use unqualified :required-arg1 here because I used :req-un in (s/keys :req-un [::required-arg1])(func-with-map-arg3 {::required-arg1 "hello world"})
  (func-with-map-arg3 {::required-arg1 "hello world"})
;
  )

;; Restricting allowed keys in a map is considered an antipattern by the clojure community
(defn strict-check-map-items [m]
  (let [keys-passed (keys m)
        allowed-keys #{:required-arg1 :optional-arg1}
        valid-keys (remove nil? (map allowed-keys keys-passed))]
    (= (count keys-passed) (count valid-keys))))

(comment
  (remove nil? (map #{} [:required-arg1 :some-other-arg]))
  (strict-check-map-items {:required-arg1 "hello world" :some-other-arg 23})
;
  )

(s/def ::map3
  (s/and (s/keys :req-un [::required-arg1]
                 :opt-un [::optional-arg1])
         strict-check-map-items))

(defn-spec func-with-map-arg3 string? [context ::map3]
  (:required-arg1 context))

(comment
  (st/instrument)
  (func-with-map-arg3 1)
  ;; Pass a valid optional arg at it is OK
  (func-with-map-arg3 {:required-arg1 "hello world" :optional-arg1 23})

  ;; try to pass an invalidate arg and spec will complain because the strict-check-map-items test will fail
  ;; NOTE: Many people in the clojure community think of this as an anti-patttern and they would not recommend 
  ;; trying to prevent extra arguments being passed on a map. I tend to agree with this argument
  (func-with-map-arg3 {:required-arg1 "hello world" :some-other-arg 23})

  ;
  )


;; s/keys does not currently support non-keyword keys
;; https://stackoverflow.com/questions/43453776/clojure-spec-for-a-map-with-non-keyword-keys

;; If you want to spec a map that uses non-keywords then you need
;; to convert the keys to keywords first
;; If the map has spaces in the keys in gets a bit weird 

(comment 

(def newmap1 (clojure.walk/keywordize-keys {"k1" 1 "k2" 2 "big long keyword" 4}))
(count (keys newmap1))

;; To define a map manually with spaces in keywords
(def newmap2 {(keyword "big long keyword") 1 :k2 3})
newmap2
(keyword "test" "big long keyword")

;
)

;;; --------------------------------------------------------------
;;; [E] - How do you spec anonymous functions?
;;; How do you spec anonymous functions?
;;; https://stackoverflow.com/questions/51807511/validating-anonymous-functions-passed-in-to-my-function-at-runtime-with-clojure
;;;  s/fspec

(comment 

(s/def ::func-spec (s/fspec :args (s/cat :x number?)))
;; There is something more complicated with fspec
;; The below doesn't work I get the following errors:
; 
; Execution error (FileNotFoundException) at type-specification.using-spec/eval10179 (form-init17868726951467277749.clj:249).
; Could not locate clojure/test/check/generators__init.class, clojure/test/check/generators.clj or clojure/test/check/generators.cljc on classpath.
;
;; To resolve see https://stackoverflow.com/questions/64251022/could-not-locate-clojure-test-check-init-class-clojure-test-check-clj-or-cloju
;; Need to add [org.clojure/test.check "1.1.0"] as a dependency and require [clojure.test.check.generators]

(s/explain ::func-spec (fn [x] (+ x 3))) ;; This is a successful match
(s/explain ::func-spec (fn [x y] (+ x y))) ;; This one fails because too many args are passed

;; Something I don't understand yet is to how to atach an fspec to a function and check it when the function is called
;; Take a look at: https://blog.taylorwood.io/2017/10/15/fspec.html
;; This looks like it will explain things
(defn test-fn [x] (prn x) true)
(s/def ::func-spec2 (s/fspec :args (s/cat :x number?) :fn test-fn ))
(s/explain ::func-spec (fn [x] (+ x 3)))
;
)

;;; --------------------------------------------------------------
;;; [F] - Specifying maps (part2)

  ;; The map examples I have used so far have only checked for :keywords being present
  ;; They haven't checked the values of the :keywords
  ;; This next example shows how to check both the presence of a :keyword and check its value
(s/def :test1/required-arg1 string?)
(s/def ::map4
  (s/keys :req-un [:test1/required-arg1]))

(defn-spec func-with-map-arg4 string? [context ::map4]
  (str (:required-arg1 context)))


(comment
  (st/instrument)
  (func-with-map-arg4 1)
  (func-with-map-arg4 {:required-arg1 "hello world"})
  (func-with-map-arg4 {:required-arg1 1})
  ;
  )

;;; --------------------------------------------------------------
;;; [G] - Specifying Collections
;;;
;;; Let's look at a whole range of ways that you can specify collections on core.spec

;; [G.1 - s/cat]
;; We have already seen s/cat but let's take another look
;; s/cat allows you to define a spec to match against a known number of expected values in a collection
(comment
  (s/def ::collection-with-fixed-number-of-values (s/cat :x number? :y string? :z number?))
  (s/conform ::collection-with-fixed-number-of-values [1 "hello" 2])
  (s/valid? ::collection-with-fixed-number-of-values [1 "hello" 2])
  (s/conform ::collection-with-fixed-number-of-values ["1" "hello" 2])
  (s/explain ::collection-with-fixed-number-of-values ["1" "hello" 2])
;; Show that collection as well as vectors can be used
  (s/valid? ::collection-with-fixed-number-of-values '(1 "hello" 2)))

;; [G.2 - s/coll-of]
(comment

;; Spec a collection to be keywords1
  (s/def ::collection1 (s/coll-of keyword?))
  (s/conform ::collection1 [1 "hello" 2])
  (s/conform ::collection1 [:this :that :here :there])


;; Going into more details
  (s/def ::collection2 (s/coll-of keyword? :kind vector? :count 3 :distinct true :into #{}))
  (s/explain ::collection2 [:this :that :here :there]) ;; fail too many 
  (s/explain ::collection2 [:this :that :that]) ;; Not unique
  (s/explain ::collection2 [:this :that :that2]) ;; This will pass
  (s/conform ::collection2 [:this :that :that2]) ;; returns as a set
  (s/explain ::collection2 '(:this :that :that2)) ;; This fails because it is not a vector
;
  )

;; [G.3 - s/tuple]
;; Very similar to s/cat. I can't really see the difference other than s/cat allows you to name parameters
(comment

  (s/def :geom/point (s/tuple double? double? double?))
  (s/conform :geom/point [1.5 2.5 -0.5])

;
  )


;; [G.4 - Describing sequences with a range of RegEx operators]
;; s/cat is included as part of these, but we will skip as we have already looked a tha

(comment

  (s/def :ex/seq-of-keywords (s/* keyword?))
  (s/conform :ex/seq-of-keywords [:a :b :c])
  (s/conform :ex/seq-of-keywords [:a :b :c 2])


  (s/def :ex/odds-then-maybe-even (s/cat :odds (s/+ odd?)
                                         :even (s/? even?)))
  (s/conform :ex/odds-then-maybe-even [1 3 5 100])
;
  )


;; [G.5] - s/alt
;; For describingg alternatives

(comment

  (s/def :ex/config (s/*
                     (s/cat :prop string?
                            :val  (s/alt :s string? :b boolean?))))
  (s/conform :ex/config ["-server" "foo" "-verbose" true "-user" "joe"])
  (s/explain :ex/config ["-server" "foo" "-verbose" true "-user" "joe" "-other" 2])

;
  )


;;; --------------------------------------------------------------
;;; [H] - Printing out and testing specs
;;; We have already used most of these functions already above
;;; This is just a recap on most of them

(comment 

(s/describe :ex/config)
(s/valid? :ex/config ["-server" "foo" "-verbose" true "-user" "joe" "-other" 2])
(s/conform :ex/config ["-server" "foo" "-verbose" true "-user" "joe" "-other" 2])
(s/explain :ex/config ["-server" "foo" "-verbose" true "-user" "joe" "-other" 2])

; 
)
