(ns SICP.newton-raphson
  (:refer-clojure)
  )
; (in-ns 'ClojureNotebook.newton-raphson)
; (ns-name *ns*)
; (ns-interns *ns*)
; (all-ns)

(defn exp [x n]
  (reduce * (repeat n x)))

(defn square [x]
  (exp x 2))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) 0.00000000001))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(sqrt 81)



