;;; https://clojure.org/guides/threading_macros
;;; Macros for making code more readable
;;; Threading results into the following expressions

(-> 1 inc inc )

(-> 1 inc inc)

(defn test [x] (* x 3))
(-> 1 inc test)


;; You have to place anonymous function in brackers
(-> 1 inc test ((fn [x] (* x 3))))

(fn [x] (* x 3))

(-> 1 inc test  (#(* % 3))   )

(range 10)

(range)

(->> (range 10)
     (filter odd? ,,,)) ;; ,,, is optional for readability

(defn func2Args [arg1 arg2]
  (+ arg1 arg2))

;; maps can take multiple collections
(map func2Args [1 1 1] [2 2 2])

(map func2Args [1 1 1] [2 2])

(map func2Args [1 1] [2 2 2])

(map func2Args [1 1 1] (range))

(map #(+ %1 %2) [1 1 1] (range))

;; From the manuals
(as-> [:foo :bar] v
  (map name v)
  (first v)
  (.substring v 1))

;; Easier to understand
(as-> [:foo :bar] v
  (str "Hello " v)
  (str "Hi " v)
  (str "Adios " v))

