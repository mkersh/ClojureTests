;;; Function to calculate nth root of a number using my own algorithm
;;; NOTE: I already have an algorithm for this in src/functions/pow.clj
;;; but I was thinking about exponents and roots again and want to created 
;;; my own version of this algorithm
(ns maths.nthroot)

(defn pow [num power]
  (loop
   [res 1N
    n power]
    (if (= n 0)
      res
      (recur (* res num) (- n 1)))))

(defn abs [n] (max n (- n)))

(defn nroot-check [number guess nthroot]
  (let
   [guess-total (pow guess nthroot) ;; Could be more efficient than this if we wanted. The minute pow is greater than number could stop
    diff (- number guess-total)
    delta (abs diff)]
    ;;(prn delta)
    (cond
      (< delta 0.00000000000001) :finish
      (< diff 0) :big  ;; The guess is too big
      :else :low ;; The guess is too low
      )))

;; nth-root iterates its way to a solution for finding the nth-root
(defn nth-root [number nthroot]
  (loop [previous-guess 0
         start-range 0
         end-range number]

    (let [guess (/ (+ start-range end-range) 2.0)
          guess-res (nroot-check number guess nthroot)]
        ;;(prn "Iteration - Guess: " guess)
      (if (= previous-guess guess) ;; check we are making progress else stop
        guess ;; This is the best we can do return result
        (condp = guess-res
          :finish guess
          :big (recur guess start-range guess)
          :low (recur guess guess end-range))))))

(comment 

(time (nth-root 2763 3))

(pow 14.032238630379794 3)

(+ 132 103 86 36 42 125 67 121 65 68 16)
(/ (* 861 5) 60.0)
(/ 71.75 7)

;;
)


;; TODO - Compare my version above against https://rosettacode.org/wiki/Nth_root#Clojure 

;; IMO this is a lot more difficult to understand than my version
(defn calc-delta [A x n]
  " nth root algorithm delta calculation "
  (/ (- (/ A (pow x (- n 1))) x) n))

(defn nth-root2
  " nth root of algorithm: A = numer, n = root"
  ([A n] (nth-root2 A n 0.5 1.0))  ; Takes only two arguments A, n and calls version which takes A, n, guess-prev, guess-current
  ([A n guess-prev guess-current] ; version take takes in four arguments (A, n, guess-prev, guess-current)
   (if (< (abs (- guess-prev guess-current)) 1e-6)
     guess-current
     (recur A n guess-current (+ guess-current (calc-delta A guess-current n)))))) ; iterate answer using tail recursion

(comment 

;; This rosetta stone version does appear to about twice as quick as mine above
(time (nth-root2 2763 3))

;; The difference in performance seems to be less for a more complicated case
(time (nth-root2 276344544554544554544545 3))
(time (nth-root 276344544554544554544545 3))

;; For this next one my algorithm is about 10 times faster
(time (nth-root2 276344544554544554544545 30))
(time (nth-root 276344544554544554544545 30))

(nth-root 4192 3)

;;
)