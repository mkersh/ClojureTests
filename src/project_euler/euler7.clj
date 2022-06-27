;;; https://projecteuler.net/problem=7 
;;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;;; What is the 10001st prime number?
;;;
;;; MK: Reusing my prime-generator from euler3. This is not the most efficient method
;;; to generate prime numbers. Make look at some point to improve

(ns project-euler.euler7
  (:require [project-euler.support :as supp]
  [project_euler.euler3 :as euler3]
  ))

(defn get-nth-prime 
([n] (get-nth-prime n 999999))
([n max]
  (let [prime-list (into [] (euler3/prime-seq2 max))
        nth-prime (get prime-list (dec n) nil)]
    (if nth-prime
      (prn (str "The " n " prime number = " nth-prime))
      (prn "ERROR: get-nth-prime - max given too small"))
    nth-prime))
  
  )

(comment

(supp/start-task :job-pp1  (fn [] (time (get-nth-prime 10001))))
(supp/start-task :job-pp1  (fn [] (time (get-nth-prime 100001 9999999))))
(supp/wait-task :job-pp1 4000)
(supp/kill-task :job-pp1)


;;
)