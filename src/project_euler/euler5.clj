;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
;;
;; MK: Not sure if this is the best approach but euler3 was till fresh in my mind
;; So I have devised and algorithm that is based around prime-factors
(ns project-euler.euler5
  (:require [project-euler.support :as supp]
            [project_euler.euler3 :as euler3]))

;; Helper function for raising a num by a power
(defn num-pow [num power]
  (loop
   [res 1N
    n power]
   (if (= n 0)
     res
     (recur (* res num) (- n 1)))))

;; How many times does prime1 divide into num
(defn divide-power [num prime1]
  (loop [res 0
         new-num num]
    (if (= (mod new-num prime1) 0)
      (recur (inc res) (quot new-num prime1))
      res)))

;; Increase the power that we need to raise a prime by
(defn increase-prime-power [map1 factor1 size1]
  (let [cur-size (get map1 factor1 1)
        new-size (max cur-size size1)]
    (assoc map1 factor1 new-size)))


;; Approach-1: Finding the smallest number that all numbers in (range 1 (inc n)) will divide
;; into without any remainder
;;
(defn smallest-num [n]
  (let [_ (prn "Calculating prime-factor sizes")
        prime-sizes (reduce (fn [prime-map1 num]
                              (let [num-factors (euler3/prime-seq2 num)] ;; get all prime factors for num
                                ;; Loop through each of the prime factors
                                (loop [prime-list num-factors
                                       prime-map2 prime-map1]
                                  (if-let [nxt-factor (first prime-list)]
                                    (let [size (divide-power num nxt-factor) ;; how many times can nxt-factor divide into num
                                          ;; see if we need to increase the power associated with nxt-factor
                                          prime-map3 (increase-prime-power prime-map2 nxt-factor size)]
                                      ;; iterate to the next factor
                                      (recur (rest prime-list) prime-map3))
                                    ;; End Loop: When no more prime-factors to consider
                                    prime-map2))))
                            {} ;; map to store the power of each prime
                            (range 2 (inc n)) ;; consider all numbers in this range
                            )
        _ (prn "Prime Sizes" prime-sizes)
        smallest-prod (reduce (fn [res [prim-num pow]]
                                (* res (num-pow prim-num pow))) 1 prime-sizes)
        _ (prn "ANSWER: " smallest-prod)]
    smallest-prod))

(comment
  
  (supp/start-task :job-pp1 (fn [] (smallest-num 10)))
  ;; This next one was the example question
  (supp/start-task :job-pp1 (fn [] (smallest-num 20)))
  ;; Let's see how big we can go
  (supp/start-task :job-pp1 (fn [] (smallest-num 400)))
  (supp/start-task :job-pp1 (fn [] (smallest-num 4000)))
  (supp/wait-task :job-pp1 4000)
  (supp/kill-task :job-pp1)
(get {} 1 "jj")

  ;;
)