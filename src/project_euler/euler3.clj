;;; The prime factors of 13195 are 5, 7, 13 and 29.
;;; What is the largest prime factor of the number 600851475143 ?
(ns project_euler.euler3
  (:require [project-euler.support :as supp]))

(defonce LAST-PRIME (atom nil))

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
        (do
          (reset! LAST-PRIME nxt-num)
          (recur nxt-num (conj  prime-list nxt-num) max))
        (recur nxt-num prime-list max)))))

(defn prime-factors [num]
  (let [prime-list (prime-seq 3 [2 3] num)]
    (filter #(= (mod num %) 0) prime-list)))

(defn largest-prime-factor [num]
  (reduce max (prime-factors num)))


;; Need a more efficient way to generate primes

;; Add multiples of prime1 to the not-prime-map
(defn add-not-prime [prime1 multiple not-prime-map max]
  (let [mult-prime (* prime1 multiple)]
    (if (> mult-prime (quot max 2))
      not-prime-map
      (if (even? mult-prime)
        ;; no need to add even numbers to the not-prime-map
        (recur prime1 (inc multiple) not-prime-map max)
        (recur prime1 (inc multiple) (assoc not-prime-map mult-prime true) max)))))

;; Generate a sequence of prime using a sieve approach
;; In summary: 
;; (1) as primes are added maintain a not-prime-map which is all the multiples of the prime
;; (2) To check whether a new-num is a prime you just need to check that it is not on the  not-prime-map
;; (3) My concern is that for large max the not-prime-map is going to get too big
;; (3.1) Planning to reduce the size of max to mitigate this concern
(defn prime-seq2
  ([max] (prime-seq2 3 [2] {} max))
  ([num1 prime-list not-prime-map max]
   (if (> num1 (quot max 2)) ;; TBD - Need to do better than this to reduce the max-prime-num we need to consider
     prime-list
     (if (get not-prime-map num1)
       ;; num1 is not a prime
       (recur (+ num1 2) prime-list not-prime-map max)
       ;; only considering odd numbers as primes (after 2)
       (recur (+ num1 2) (conj prime-list num1) (add-not-prime num1 2 not-prime-map max) max)))))

(comment
  (is-prime? 6 [2 3])
  (prime-seq 3 [2 3] 20)
  (prime-seq 3 [2 3] 13195)
  (supp/start-task :job-lpf0 (fn [] (time (last (prime-seq 3 [2 3] 1319500)))))
  (supp/wait-task :job-lpf0 4000)
  (supp/kill-task :job-lpf0)

  ;; test against the example result given
  ;; answer = (5 7 13 29)
  (prime-factors 13195)
  (largest-prime-factor 13195)

  ;; Now to solve the problem
  ;; answer = ???
  ;; Ran the below for several hours and it did not come back with a results
  ;; Need to write a more efficient prime-seq generation
  (supp/start-task :job-lpf1 largest-prime-factor [600851475143])
  (supp/wait-task :job-lpf1 4000)
  (supp/kill-task :job-lpf1)
 ;; (largest-prime-factor 600851475143)
  @LAST-PRIME


  ;; [2] Approach-2: prime-eq2
  (prime-seq2 300)
  (supp/start-task :job-lpf2 (fn [] (time (last (prime-seq2 1319500)))))
  (supp/start-task :job-lpf2 (fn [] (time (last (prime-seq2 600851475143)))))
  (supp/wait-task :job-lpf2 4000)
  (supp/kill-task :job-lpf2)


  ;;
  )




;;; Related stuff from others


;; Another way of generating primes
;;https://github.com/AndrewSinclair/euler12-clojure/blob/master/src/euler12/core.clj
;;https://crossclj.info/fun/clojure.contrib.lazy-seqs/primes.html
(def primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
          (fn primes-from [n [f & r]]
            (if (some #(zero? (rem n %))
                      (take-while #(<= (* % %) n) primes))
              (recur (+ n f) r)
              (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))


(comment 

(time (last (take 100000 primes)))

(last (prime-seq 3 [2 3] 100000))
;;
)