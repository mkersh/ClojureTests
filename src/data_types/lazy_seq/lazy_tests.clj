;;; Many sequences in Clojure are Lazy - http://clojure-doc.org/articles/language/laziness.html
;;; 
(ns data-types.lazy-seq.lazy-tests
;;   (:require
;;    [clojure.pprint :as pp]
;;    )
  )


(import java.util.UUID)

(defn uuid-seq
  []
  (lazy-seq
   (cons (str (UUID/randomUUID))
         (uuid-seq))))

;; The next line proves that take returns a lazy seq as weel
(take 100000000 (uuid-seq))

;; Try and force a large sequence and it will then take time
(dorun (take 1000 (uuid-seq)))


;; This is a very elegant way to specify the Fibonaaci sequence
(defn fib-seq
  "Returns a lazy sequence of Fibonacci numbers"
  ([]
   (fib-seq 0 1))
  ([a b]
   (lazy-seq
    (cons b (fib-seq b (+ a b))))))

(take 20 (fib-seq))

(defn inf-range
  ([]
   (inf-range 0))
  ([n]
   (lazy-seq
    (cons n (inf-range (+ n 1))))))

(defn printall [lst]
  (dorun (map prn lst) )
  )

(printall (take 100 (inf-range)))

(inf-range)

;; My inf-range is the same as the official range
(range 0 1000 3)

;; I thought the next function would go into an infinite loop
;; but it doesn't. The cons seems to be automatically workimng in a 
;; lazy seq way
(defn inf-range-wrong
  ([]
   (inf-range 0))
  ([n]
    (cons n (inf-range (+ n 1)))))

(printall (take 200 (inf-range-wrong)))


;; An example combining lazy sequences with higher order functions
;; Generate prime numbers using trial division.
;; Note that the starting set of sieved numbers should be
;; the set of integers starting with 2 i.e., (iterate inc 2) 
;; 
;; Wow that is quite clever how this works!
;; The magic is all happening with the filter that filters out all items divisible 
;; by the first item on the list remembering the the list has previously been filtered
;; by the items before 2, 3, 5 etc
;; How it is working behind the scene is still quite complicated though with multiple
;; lazy evaluation being applied at multiple steps.
(defn sieve [s]
  (cons (first s)
        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s))))))

(nth (sieve (iterate inc 2)) 2)

(iterate inc 2)

(take 3 (iterate #(+ % 3) 2))


(defn sieve2 [s]
  (cons (first s)
        (sieve (filter #(not= 0 (mod % (first s)))
                                 (rest s)))))

(take 5 (sieve2 (iterate inc 2)))

;; Both test-lazy and test-lazy2 work
;; BUT they have the lazy-seq in different places
;; I couldn't understand what was happening here at first but now I can
(defn test-lazy
  [n]
  (cons n
   (lazy-seq (test-lazy (+ n 1)))))

  (test-lazy 0)

(defn test-lazy2
  [n]
  (lazy-seq
   (cons n (test-lazy2 (+ n 1)))))

(test-lazy2 0)



