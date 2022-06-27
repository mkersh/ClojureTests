;;; https://projecteuler.net/problem=6
;;; The sum of the squares of the first ten natural numbers is:
;;; 1^2 + 2^2 + ... 10^2 = 385
;;;
;;; The square of the sum of the first ten natural numbers is:
;;; (1 + 2 + ... 10)^2 = 55^2 = 3025
;;;
;;; Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is
;;; 3025 - 385 = 2640 
;;;
;;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
;;;

(ns project-euler.euler6)

(defn sum-of-n [n]
  (* (/ n 2) (+ n 1)))

;; Magic formulae from: https://www.khanacademy.org/math/algebra-home/alg-series-and-induction/alg-sum-of-n-squares/v/sum-of-n-squares-1 
;; formulae = ⅙n(n+1)(2n+1)
(defn sum-of-n-squared [n]
  (* (* (/ 1 6) n) (+ n 1) (+ (* 2 n) 1)))

(defn sqr-sum-of-n [n]
  (let [sum (sum-of-n n)]
    (* sum sum)))

(defn diff-question [n]
  (let [sum1 (sqr-sum-of-n n)
        sum2 (sum-of-n-squared n)
        diff (- sum1 sum2)]
    (prn "ANSWER:" diff)
    diff))



(comment
(diff-question 10)
(diff-question 100)

;;
)