(ns functions.pow
  (:require [clojure.math.numeric-tower :as math]))

(defn pow [num power]
  (loop
   [res 1N
    n power]
    (if (= n 0)
      res
      (recur (* res num) (- n 1)))))

(time (pow 2 2000))

(defn java-pow1 [num power]
  (.pow num power))
(time (java-pow1 2M 2000))

(defn java-pow2 [num power]
  (math/expt num power))

(time (java-pow2 2 2000))

(java-pow2 2 -1)

(defn abs [n] (max n (- n)))

;; Modified version that can handle -ve powers
(defn pow2 [num power]
  (loop
   [res 1N
    n (abs power)]
    (if (= n 0)
      (if (neg? power) (/ 1 res) res)
      (recur (* res num) (- n 1)))))

(time (pow2 2 -2000))
(time (java-pow2 2 -2000))

(java-pow2 2 2.5)

(math/sqrt 2)

;; https://www.rosettacode.org/wiki/Nth_root#Clojure 
(defn calc-delta
  " nth root algorithm delta calculation "
  [A x n]
  (/ (- (/ A (pow2 x (- n 1))) x) n))

(defn nth-root
  " nth root of algorithm: A = numer, n = root"
  ([A n]
   (nth-root A n 0.5 1.0))  ; Takes only two arguments A, n and calls version which takes A, n, guess-prev, guess-current
  ([A n guess-prev guess-current] ; version take takes in four arguments (A, n, guess-prev, guess-current)
   (if (< (abs (- guess-prev guess-current)) 1e-6)
     guess-current
     (recur A n guess-current (+ guess-current (calc-delta A guess-current n)))))) ; iterate answer using tail recursion

(nth-root 2 3)
