;; from  https://cuddly-octo-palm-tree.com/posts/2021-11-07-clj-primes/

(ns maths.primes)

(def integers
  (cons 1
        (lazy-seq (map inc integers))))


(defn divisors
  [n]
  (->> integers
       (take-while (fn [i] (<= i n)))
       (filter (fn [i] (zero? (rem n i))))))

(defn prime?
  [n]
  (= [1 n] (divisors n)))

;; "By making it a global lazy list, we get memoization for free"
(def primes
  (filter prime? integers))

(comment
  (time (nth primes 0))
  (time (nth primes 10))
  (time (nth primes 100))
  (time (nth primes 1000))


   (->> "inject param"
    (prn "1st" "2nd"))
;;
  )
