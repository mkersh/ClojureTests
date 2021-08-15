;;; ************************************************************
;;;
;;; Clojure Tests and examples
;;;
;;;  GITHUB: https://github.com/mkersh/JupyterNotebooks/blob/master/ClojureTests/src/sandbox/ClojureNotebook.clj 
;;;
;;; ************************************************************
(ns sandbox.ClojureNotebook
  (:refer-clojure))
; (in-ns 'ClojureNotebook)
; (ns-name *ns*)
; (ns-interns *ns*)
; (all-ns)

;;; *********************************  List datastructures
(def MyExampleList '(1 2 3 4 5 6))
MyExampleList


; adding to the beginning of a list
(cons 0 MyExampleList)

; add multiple items to the beginning of a list
(conj MyExampleList 7 8 9 10)

;; append 2 lists together
(reduce conj MyExampleList '(7 8 9 10))

;; Functions that you can apply to list
(nth MyExampleList 3)


;; This is an example of calling a java function .indexOf on a list
;(.indexOf MyExampleList 4)

;;; Map function example


(map + [1 2 3 4 5 6 7 8 9 10] (cycle [1 3]))

(map + (cycle [1 3]) [1 2 3 4 5 6 7 8 9 10])

(map #(+ % 50) [1 2 3])

(count [1 2 3 4 5 6 7 8 9 10])


; If you try and execute the next line it will hang the REPL
; CTRL-OPT-C CTRL-OPT-D to break
;;(count (cycle [1 3]))



;;; ********************************* Vectors
;;; Very similar to lists but it general use vectors to store data.
;;; One advantage is that you do not have to quote a vector when it just has data in it,
;;; whereas with a list you do.
;;; With a list (unless quoted) the first argument is always expected to be a function to apply the following arguments.

[1 2 3 4]

(vector 1 4 5 6)

(vector 1 2 "hh")

(comment
  ; Commenting next line out cos it causes an error
  (vector-of :int 1 2 "hh"))

(vector-of :int 1 2 3 4 5)

(vector-of :char \h \k)

(def myVector ["this" "that" "here" "there"])

(count myVector)

;; List comprehension
(for [x myVector]
  (str "1" x))

;; Modify a vector - Replace an element with a new one
(assoc myVector 1 "Wow")


;;; *************************************** Maps
(def myMap {:key1 "This is key1" :key2 "this is key 2"
            :key3 "This is key3"
            4 "this is keyed by 4"
            "SomeKey" "Value of somekey"})

myMap

;; There are a number of ways to get stuff out of a map
(println "Using get without a default: " (get myMap 4))
(println "Using get with a default: " (get myMap 5 "(optional) default if key missing"))
(println "Map as function: " (myMap "SomeKey"))
(println "Map as function (with default): " (myMap "SomeKey4" "default"))
(println "keyword as function " (:key30 myMap))

(def myMap2 (sorted-map :key55 ["55 val1"] :key56 ["55 val2"]))
myMap2

(assoc myMap2 :key55 11)

;; Keys in maps are unique. If you want to add multiple values for a key you need to do something like:
(defn addItem
  "this is a docString associated with the function"
  [key val]
  (let
   [curVal (myMap2 key)
    newVal (conj curVal val)]
   (def myMap2 (assoc myMap2 key newVal))))  ; Not a good idea I know!

(addItem :key55 92)

myMap2

(ns-name *ns*)

(def vector7 [1 2 3 4])
(try (vector7 10)
     (catch Exception e (str "caught exception: " e)))

;;; *********************************  Functions
;;;

;; Standard public function
(defn publicFunction
  "optional doc string"
  {:optional-meta-data 1}
  [arg1 arg2]
  (println (str "this is" arg1 arg2)))

(publicFunction 1 2)

;; defn- creates a private function that is only visible within  the current namespace
(defn- privateFunction
  "optional doc string"
  {:optional-meta-data 1}
  [arg1 arg2]
  (println (str "this is" arg1 arg2)))

(privateFunction 1 2)

(defn funcWithOverloadedArgs
  ([]
   (println "No Args passed"))
  ([_] ; _ allows you to ignore arg and avoid lint errors
   (println "1 Args passed"))
  ([arg1 & rest]
   (println "More than 1 Args passed " rest)))

(funcWithOverloadedArgs 1 2 3)

(defn funcWithListParam
  ;([[]]
   ;(println "Null list"))
  ([[arg1 & rest]]
   (println (str "Args passed list " arg1 rest))))

(funcWithListParam [])

(def my-vector [1 2 3])

(let [[x & z] my-vector]
  (println x z))

;; Anonymous functions

(def fn1
  (fn [x] (println x)))

(fn1 "this is it")

;; Shorter version of anonymous functions
(#(println (str "anon func called with " %)) 1)

(def fn2 #(println (str "anon func2 called with " %)))

(fn2 "Let see this work")

;; Recursive functions

(defn fac [x]
  (if (= x 1)
    1
    (* x (fac (- x 1)))))

(fac 100N)

(defn fac2 [x]
  (loop [n x
         result 1]
    (if (= n 1)
      result
      (recur (- n 1) (* result n)))))


(fac2 100N)

;; Calculating the number of combinations of Crypto Seeds for BIP-39 Word List with dictionary of 2048 words and seeds of 24 words

(let [num-words 24
      res (/ (fac2 2048N) (fac2 (- 2048N num-words)))]
  (println "Result: " res)
  (str res))
;; ->> Macro

(->> 1 (+ 2) (fn2))

(count "115.792.089.237.316.195.423.570.985.008.687.907.853.269.984.665.640.564.039.457.584.007.913.129.639.936")

(require '[clojure.string :as str])
(count (str/split "115.792.089.237.316.195.423.570.985.008.687.907.853.269.984.665.640.564.039.457.584.007.913.129.639.936" #"\."))
(* 26 3)

(count "25892008055647378700916274834106651525738683598033725572049016676308484096000000")

(defn exp [x n]
  (reduce * (repeat n x)))

(count (str (exp 2048N 24N)))


;;; ******************** Concurrent programming
;;; Futures, Delays, Promises
;;; Atoms, Refs

;; Future - Basically creates a separate JVM thread
(defn testFuture []
  (future (Thread/sleep 4000)
          (println "I'll print after 4 seconds"))
  (println "I'll print immediately"))

(testFuture)

(let [result (future (Thread/sleep 3000)
                     (+ 1 1))]
  (println "The result is: " @result)
  (println "It will be at least 3 seconds before I print"))

(def fut1 (future (Thread/sleep 3000)
                  (+ 1 1)))

; See if a future is realised
(realized? fut1)
@fut1


;; Atoms allow you to change state in a controlled way
(def fred (atom {:cuddle-hunger-level 0
                 :percent-deteriorated 0}))
@fred
(swap! fred
       (fn [current-state]
         (merge-with + current-state {:cuddle-hunger-level 1})))

;; Delays
(def jackson-5-delay
  (delay (let [message "Just call my name and I'll be there"]
           (println "First deref:" message)
           message)))

jackson-5-delay

; Dereference or force for evaluate
(force jackson-5-delay)
@jackson-5-delay


;; Promises
(def my-promise (promise))
(deliver my-promise (+ 1 2))
@my-promise
