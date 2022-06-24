;;; The prime factors of 13195 are 5, 7, 13 and 29.
;;; What is the largest prime factor of the number 600851475143 ?
(ns project-euler.problem3)

(defn is-prime? [num prime-list]
  (if (empty? prime-list)
    true
    (if (= 0 (mod num (first prime-list)))
      false ;; num is not a prime
      (recur num (rest prime-list)))))

(defn prime-seq [last-num prime-list max]
  (if (> last-num (quot max 2))
    prime-list
    (let [nxt-num (+ last-num 1)]
      (if (is-prime? nxt-num prime-list)
        (recur nxt-num (conj  prime-list nxt-num) max)
        (recur nxt-num prime-list max)))))

(defn prime-factors [num]
  (let [prime-list (prime-seq 3 [2 3] num)]
    (filter #(= (mod num %) 0) prime-list)))

(defn largest-prime-factor [num]
  (reduce max (prime-factors num)))


(comment
  (is-prime? 6 [2 3])
  (prime-seq 3 [2 3] 20)
  (prime-seq 3 [2 3] 13195)
  
  ;; test against the example result given
  ;; answer = (5 7 13 29)
  (prime-factors 13195)
  (largest-prime-factor 13195)

  ;; Now to solve the problem
  ;; answer = ???
  ;; Ran the below for several hours and it did not come back with a results
  ;; Need to write a more efficient prime-seq generation
 ;; (largest-prime-factor 600851475143)

  ;;
  )

