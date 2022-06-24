
;;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
;;; The sum of these multiples is 23.
;;; Find the sum of all the multiples of 3 or 5 below 1000.

(ns project-euler.problem1)

(defn sum-multiples-3-or-5 [n]
  (let [num-list (range n)
        multiples-list (filter #(or (= (mod % 3) 0) (= (mod % 5) 0)) num-list)]
    (reduce + multiples-list)))

(defn sum-mult [mult n ceil total]
  (let [res (* mult n)]
    (if (> res ceil)
      total
      (recur mult (+ n 1) ceil (+ total res)))))

;; Here another way of doing it that will be more efficient
(defn sum-multiples-3-or-5-v2 [n]
  (let [ceil (- n 1)
        sum-of-3s (sum-mult 3 1 ceil 0)
        sum-of-5s (sum-mult 5 1 ceil 0)
      ;; need to take 15s away because they will have already been counted
        sum-of-15s (sum-mult 15 1 ceil 0)]
    (- (+ sum-of-3s sum-of-5s) sum-of-15s)))

(comment
  (sum-multiples-3-or-5 10)
  (time (sum-multiples-3-or-5 1000))
  ;; This next one would take to long
  ;; (sum-multiples-3-or-5 1000000000)

  (sum-multiples-3-or-5-v2 10)
  (time (sum-multiples-3-or-5-v2 1000))
  ;; answer for next one 233333333166666668
  ;; still take "Elapsed time: 116332.769858 msecs"
  (time (sum-multiples-3-or-5-v2 1000000000))



 ;; 
  )