;;; The prime factors of 13195 are 5, 7, 13 and 29.
;;; What is the largest prime factor of the number 600851475143 ?
;;;
;;; MK: I strugled with this one, because I had forgotten the basic maths around factoring.
;;; Eventually I worked it out (trial-and-error) and produced Approach-03
;;;
;;; If I had known the maths though then I should have gone with Approach-04
;;;
;;; Approach-04 will still take a very long time when the number to find factors for includes
;;; a very large prime factor. 
;;; TBD - There are hints in https://projecteuler.net/overview=003 as to how to handle this
;;;

(ns project_euler.euler3
  (:require [project-euler.support :as supp]))

(defonce LAST-PRIME (atom nil))
(defonce PRIME-LIST (atom []))

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
    (if (> mult-prime max)
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
  ([filter-func max] (prime-seq2 filter-func 3 (if (filter-func 2) [2] []) {} max))
  ([num1 prime-list not-prime-map max] (prime-seq2 identity num1 prime-list not-prime-map max))
  ([filter-func num1 prime-list not-prime-map max]
   (if (> num1 max)
     prime-list
     (if (get not-prime-map num1)
       ;; num1 is not a prime
       (recur filter-func (+ num1 2) prime-list not-prime-map max)
       ;; only considering odd numbers as primes (after 2)
       (recur filter-func (+ num1 2)
              (if (filter-func num1)
                (do (reset! LAST-PRIME num1) (conj prime-list num1))
                prime-list)
              (add-not-prime num1 2 not-prime-map max) max)))))

(defn get-max-prime [num]
  (let [plist0 (prime-seq2 1319500)] ;; This doesn't work for big num
    (loop [plist (rest plist0)]
      (if (empty? plist)
        :get-max-prime-error
        (let [div-num (first plist)]
          (if (= (mod num  div-num) 0)
            (quot num div-num)
            (recur (rest plist))))))))

(defn largest-prime-factor2 [num]
  (let [max-prime (get-max-prime num)
        _ (prn "Calculated max-prime" num max-prime)
        filter-func (fn [n] (= (mod num n) 0))
        primes-list (prime-seq2 filter-func max-prime)
        _ (prn "Calculated prime factors list" primes-list)]
    primes-list))

(def debug-count 9999999)
(defonce DEBUG-CNT (atom debug-count))
(defn prn-if-zero [msg]
  (let [cnt (dec @DEBUG-CNT)]
    (if (<= cnt 0)
      (do (reset! DEBUG-CNT debug-count)
          (prn msg))
      (reset! DEBUG-CNT cnt))))
(defn prn-if-zero-reset []
  (reset! DEBUG-CNT debug-count))

;; Is num1 a prime-divisssor of max
(defn is-prime-divisor? [max num1 prime-list]
  (prn-if-zero (str "Considering " num1))
  (if (= (mod max num1) 0) ;; 1st check that num1 is a divisor
    (is-prime? num1 prime-list) ;; next check whether it is a prime
    false))

;; *********************************************************
;; Approach-03 - First of my methods that works for 600851475143
;;
;; 
(defn prime-seq3
  ([max] (prn-if-zero-reset) (prime-seq3 3 [] max))
  ([num1 prime-list max]
   (if (> num1 max)
     prime-list
     (if (is-prime-divisor?  max num1 prime-list)
       ;; only considering odd numbers as primes (after 2)
       (recur  (+ num1 2)
               (do
                 (reset! LAST-PRIME num1)
                 (let [new-prime-list (conj prime-list num1)]
                   (reset! PRIME-LIST new-prime-list)
                   new-prime-list))
              ;; Next line is the thing that solved it for me
              ;; Previous to this I was alway passing max in here
              (quot max num1) ;; Can I do this?? Looks like I can :)
              )
       ;; num1 is not a prime
       (recur  (+ num1 2) prime-list  max)))))

(defn largest-prime-factor3 [num]
  (let [_ (prn "Calculating Prime Divisors for" num)
        primes-list (prime-seq3  num)
        _ (prn "Calculated prime factors list" primes-list)]
    (last primes-list)))

;; *********************************************************
;; Approach-04 - There is an easier way that is similar to my approach-3
;; 

;; Reduce max down as far as possible with num1
;; NOTE: if num1 does not divide into max then max=max at the end
;; We will call reduce-by-divisor for num1 = 2,3,5,7,9,11,13,..
;; The only num1's that will reduce max will be prime numbers (this is the key fact to understand)
;;
(defn reduce-by-divisor [max num1]
  (if (= (mod max num1) 0)
    (recur (quot max num1) num1)
    max))

(defn prime-seq4
  ([max] (prime-seq4 2 [] max))
  ([num1 prime-list max]
   (if (= max 1)
     prime-list
     (let [new-max (reduce-by-divisor max num1)
           divides? (< new-max max)
           nxt-num (if (= num1 2) 3 (+ num1 2)) ;; After 2 only consider odd numbers
          ]
       (if divides?
         (recur  nxt-num
                 (do
                   (reset! LAST-PRIME num1)
                   (conj prime-list num1))
                 new-max)
         (recur nxt-num prime-list  max))))))

(defn largest-prime-factor4 [num]
  (let [_ (prn "Calculating Prime Divisors for" num)
        primes-list (prime-seq4  num)
        _ (prn "Calculated prime factors list" primes-list)]
    (last primes-list)))

(comment
  ;; **************   Approach-4
  (reduce-by-divisor 1024 2)
  (supp/start-task :job-lpf5 (fn [] (largest-prime-factor4 13195)))
  (supp/start-task :job-lpf5 (fn [] (largest-prime-factor4 131950000)))
  (supp/start-task :job-lpf5 (fn [] (time (largest-prime-factor4 600851475143))))
  (supp/wait-task :job-lpf5 4000)
  (supp/kill-task :job-lpf5)
  @LAST-PRIME
  

  ;; **************   Approach-3
  (supp/start-task :job-lpf4 (fn [] (largest-prime-factor3 13195)))
  (supp/start-task :job-lpf4 (fn [] (largest-prime-factor3 131950000)))
  (supp/start-task :job-lpf4 (fn [] (time (largest-prime-factor3 600851475143))))
  (supp/wait-task :job-lpf4 4000)
  (supp/kill-task :job-lpf4)
  @LAST-PRIME
  (mod 600851475143 6857)


  ;; ************************  Approach-1
  ;; My first attempt - based around generating a sequence of primes and dividing these into num
  ;; Works for small numbers but not a chance with 600851475143
  ;; Ended up writen (supp/start-task, supp/kill-task) to avoid crashing/locking my REPL

  (get-max-prime 600851475143)
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


  ;; ************************  Approach-2
  ;; My second attempt - Same basic approach as approach-01 but a slightly more efficient
  ;; way of generating the prime-seq generation
  ;; Still not a chance to calculate tthe result for 600851475143



  ;; [2] Approach-2: prime-eq2
  (last (prime-seq2 1319500))
  (supp/start-task :job-lpf2 (fn [] (time (last (prime-seq2 1319500)))))
  (supp/start-task :job-lpf2 (fn [] (time (last (prime-seq2 600851475143)))))
  (supp/wait-task :job-lpf2 4000)
  (supp/kill-task :job-lpf2)

  (supp/start-task :job-lpf3 (fn [] (largest-prime-factor2 13195)))
  (supp/start-task :job-lpf3 (fn [] (largest-prime-factor2 131950000)))
  (supp/start-task :job-lpf3 (fn [] (largest-prime-factor2 600851475143)))
  (supp/wait-task :job-lpf3 4000)
  (supp/kill-task :job-lpf3)
  @LAST-PRIME
  (last (prime-seq2 1319500))



  (mod 600851475143 1319477)
  (mod 13195 13)

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