;;; Basics about Clojure functions:
;;; [1] Standard function definitions
;;; [1a] Private function - only visible in namespace
;;; [1b] Variadic Functions in Clojure
;;; [2] Anonymous function definitions
;;; [3] Recursive functions
;;; [3b] Recursive functions using loop...recur
;;; [4] Multi-Arity functions
;;; [4b] Argument deconstruction (in general)
;;; [5] Polymorphic Functions
;;; [6] Higher-order function 
;;; 
;;; References:
;;; http://clojure-doc.org/articles/language/functions.html
(ns functions.func-basics)


;;; [1] Standard function definitions
;; Defining a simple function which takes no arguments
(defn test-func []
  (prn "test-func"))

;; To call it pass as the 1st argument in a list data struct
(test-func)

;; test-func is a var that is bound to the function
test-func


(defn test-func2 [x y]
  (prn "test-func2" x  y))


(test-func2 4 5)


(defn test-func3
  "Same as the previous test-func2 but contains this meta doc string"
  [x y]
  (prn "test-func3" x  y))

(test-func3 4 5)

(defmacro docstring [symbol]
  `(:doc (meta (var ~symbol))))

(docstring test-func3)

;;; [1b] Variadic Functions in Clojure
;;; i.e. functions with a variable number of arguments
(defn test-func4
  "function with 1 mandatory arg and any number of optional"
  [x & args]
  (prn "test-func4" x  args))

(test-func4 1 2 3 4 5 6)

(defn my-add
  "function with 1 mandatory arg and any number of optional"
  [& args]
  (apply + args)) ;; this is how you can apply a list of args to a function

(my-add 1 2 3 4 5 6 7 8 9 10)

(def ARGS-LIST [1 2 3 4 5])

;; you can pass multiple individual values to apply as well
(apply +  10 20 ARGS-LIST)

;; This next would not work. Only the last parameter can be a list/collection
;;(apply +  10 [2 3 4] ARGS-LIST)


;;; [1a] Private function - only visible in namespace
;; Making a function definition private to the name space
(defn- private-func []
  (prn "I am a very private funtion - only available within my own namespace"))

(private-func)


;;; [2] Anonymous function definitions
;; following creates an anonymous function
(fn [x]
  (prn "Function called with" x))

;; This creates an anonymous function and then calls it
((fn [x]
   (prn "Function called with" x)) 5)


;; following binds an anonymous function to a variable
;; which is the equivalent of doing (defn)
(def func6 (fn [x]
             (prn "Function called with" x)))

(func6 1)

(def func7 #(prn "another form of anonymous function" %))

(func7 7)

;; Abbreviated anon func with 2 args
(def func8 #(prn "another form of anonymous function" %2 %1))

(func8 7 8)


;;; [3] Recursive functions
;; Simple recursive function that counts down
(defn countdown1 [n]
  (prn n)
  (if (> n 0) (countdown1 (dec n)) nil))

(countdown1 5)

;;; [3b] Recursive functions using loop...recur
;; Alternative recursive function that uses recur
;; Recur is more efficient than caling the function using standard recursion

;; This first example just uses recur (i.e. there is no loop)
;; the recur happens on the existing function
(defn countdown2 [n]
  (prn n)
  (if (> n 0) (recur (dec n)) nil))

(countdown2 5)

;; This next one uses the full form of loop..recur
;; the loop is like an anonymous function that is then recursed on
;; when you call recur
(defn countdown3 [n]
  (prn "This will only get called once")
  (loop [m n]
    (prn m)
    (if (> m 0) (recur (dec m)) nil)))

(countdown3 5)


;;; [4] Multi-Arity functions
;;; You can define functions in clojure that accept different numbers of arguments and do
;;; something different depending on these arguments

(defn multi-arity-test
  ;; First definition is no args are passed
  ([] (prn "called with 0 args"))
  ([x] (prn "called with 1 arg" x))
  ([x & args] (prn "called with many args" x args)))

(multi-arity-test 1 2 3 4)


;; Extra Arguments (aka Named Parameters)

(defn job-info
  [& {:keys [name job income] :or {job "unemployed" income "$0.00"}}]
  (if name
    [name job income]
    (println "No name specified")))

(job-info :name "Robert" :job "Engineer" :income "$100K")
(job-info :job "Engineer")


;; [4b] Argument deconstruction (in general)

;; List/vector deconstruction
(defn decon-test1 [[x y & restArgs]]
  (prn "function takes a vector with 2 elements " x y restArgs))

(decon-test1 [1 2 3 4])
(decon-test1 '(1 2 3 4))

;; You can also similar deconstruction in a let block
(defn decon-test2 [arg1]
  (let
   [[x y & restArgs] arg1]
    (prn "function takes a vector with 2 elements " x y restArgs)))

(decon-test2 [1 2])

;; Map deconstruction
(defn decon-test3 [{:keys [x y restArgs]}]
  (prn "function takes a vector with 2 elements " x y restArgs))

(decon-test3 {:x 33 :y 500 :restArgs [3 4 5 6]})

;; If you want to define explicit defaults if parameters are missing then
;; use the :or keyword in the deconstruction expression
(defn decon-test4 [{:keys [x y restArgs] :or {x -1 y -1 restArgs "EMPTY"}}]
  (prn "function takes a vector with 2 elements " x y restArgs))

(decon-test4 {:x 33 :y 500 :restArgs [3 4 5 6]})



;;; [5] Polymorphic Functions
;;; See [5.1], [5.2] and [5.3] below
;;; [5.1] and [5.2] are the textbook ways. 
;;; [5.3] is a simpler alternative to just pass functions on maps


;; [5.1] defmulti +  defmethod
;; There are two ways to implement polymorphic functions in cloure
;; The first and most flexible method is using defmulti +  defmethod

;; defmulti defines the polymorphic function.
;; It takes a dispatch function and uses one of more of the arguments passed 
;; to the dispatch function to generate a dispatch-value
;; The dispatch-value is used in defmethod to identify which actual method to call
;; 
;; Gotchas:
;; (1) Once a defmulti is defined you can't just change it
;; https://stackoverflow.com/questions/44807442/how-to-reload-a-multimethod-in-clojure-repl
;; (ns-unmap *ns* 'say-hello) ;; will allow say-hello defmulti below to be redefined
;; (2) You can override a defmethod (for a specific dispatch-value) without any warnings
(defmulti say-hello (fn [x] (:type x)))


(comment
;; To redefine the dispatch you need to unmap i.e Gotcha 1 above
  )

(defmethod say-hello :dog [_] "Woof!")
(defmethod say-hello :cat [_] "Miaow!")
(defmethod say-hello :cow [_] "Moo!")
;; Override the previous :cat
;; You can do this without warning, so be careful!
(defmethod say-hello :cat [_] "Miaow222!")

(say-hello {:type :cat})

;; Here's a simpler dispatch function
;; but it is limited because it assumes that the method will only
;; be called with a single parameter and that will be used as the dispatch-value
(defmulti say-hello2 identity)
(defmethod say-hello2 :dog [_] "Woof!")
(defmethod say-hello2 :cat [_] "Miaow!")
(defmethod say-hello2 :cow [_] "Moo!")

(say-hello2 :dog)

;; [5.2] defprotocol approach
;; This is the 2nd way to do Polymorphism in Clojure
(defprotocol P
           (foo [this])
           (bar-me [this] [this y]))

(deftype Foo [a b c]
  P
  (foo [this] a)
  (bar-me [this] b)
  (bar-me [this y] (+ c y)))

(deftype Bar [a]
  P
  (foo [this] a)
  (bar-me [this] (* a 2))
  (bar-me [this y] (+ a 10)))

(bar-me (Foo. 1 2 3) 42)
(bar-me (Bar. 3) 42)
(bar-me (Bar. 3))

;; Alternative to (Bar. 3)
(->Bar 3) ;; returns (Bar. 3)
(-> (->Bar 3) foo)
(-> (->Bar 3) (bar-me))
(-> (->Bar 3) (bar-me 42))

;; Let's redo the Pet say-hello example using a protocol
(defprotocol IPetSayHello
           (say-hello3 [this]))

(deftype Dog []
  IPetSayHello
  (say-hello3 [_] "Woof!!"))

(deftype Cat []
  IPetSayHello
  (say-hello3 [_] "Miaow!!"))

(deftype Cow []
  IPetSayHello
  (say-hello3 [_] "Moo!!"))

(comment
;; See if a Type/Record or Java class extends a protocol
(extends? IPetSayHello Cat)
(extends? IPetSayHello StringBuffer)

(say-hello3 (->Dog))
(say-hello3 (->Cat))
(say-hello3 (->Cow))
)

;;; deftype v defrecord
;;; https://stackoverflow.com/questions/13150568/deftype-vs-defrecord
;;; https://clojure.org/reference/datatypes


;; [5.3] 3rd way of doing Polymorphism in Clojure
;; If the defmulti or defprotocol approaches above are not to your liking
;; You can always just implement polymorphism by attaching functions to maps
;;(def dog-map {:name "Fido" :say-hello #("Woof!!")})
(def dog-map {:name "Fido" :say-hello (fn [] "Woof!!")})
(:say-hello dog-map) ;; this returns the function
((:say-hello dog-map)) ;; if you want to call then you need double brackets

(let [fn-call (:say-hello dog-map)] ;; different way to call
  (fn-call))

;;; [6] Higher-order function 

;;; functional closures in clojure
;; A closure is a function that has access to some 
;; named value/variable outside its own scope, 
;; so from a higher scope surrounding the function 
;; when it was created (this excludes function arguments
;; and local named values created within the function)


(defn inc-by-n [n]
  ;; we return a function that is a closure. It has access to the
  ;; argument n that was passed into inc-by-n   
  (fn [x] (+ x n)))

(def inc11 (inc-by-n 11))

(inc11 3)

;; The standard function partial can be used to create some closures
;; https://clojuredocs.org/clojure.core/partial

(def inc11-v2 (partial + 11))
(inc11-v2 3)

;; Higher-order functions also refers to functions that take other
;; functions as arguments and apply those functions to datastructures (normally sequences) passed in
;; This includes the function filter, map, reduce, ...

;;; If you want more powerful argument deconstruction take a look at:
;;; https://github.com/clojusc/defun