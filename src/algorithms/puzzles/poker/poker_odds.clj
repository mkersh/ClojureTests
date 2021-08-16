;;; Thinking about writing some functions to determine Texas Holdem odds
;;; Let's see if I ever do develop this :)
;;; Because my maths is really not good enough these days I may look to calculate these odds 
;;; using first-principles by running multiple trials on a pack of card??
;;; https://github.com/mkersh/ClojureTests/tree/master/src/algorithms/puzzles/poker/poker_odds.clj
(ns algorithms.puzzles.poker.poker-odds
(:require [functions.pow :as math1]))

;; Some basic maths stats revision regarding odds:
;; https://math.stackexchange.com/questions/729920/probability-of-head-in-coin-flip-when-coin-is-flipped-two-times
;; P(odds) = 1 - P(all tails on every flip)
(defn odds-of-heads-with-n-flips [n]
  (- 1 (math1/pow 0.5 n)))

(defn odds-of-all-heads-with-n-flips [n]
  (math1/pow 0.5 n))

(comment 

(odds-of-all-heads-with-n-flips 2)
(odds-of-heads-with-n-flips 2)
(odds-of-heads-with-n-flips 3)
(odds-of-heads-with-n-flips 4)
(odds-of-heads-with-n-flips 5)
(odds-of-heads-with-n-flips 25)
)

;; Let's make it a bit more generic
;; NOTE: This assumes that each attempt will have same probability (prob-event)
;; which will not be 100% true when we are analysing a poker deck but we will
;; consider that at a later time
(defn odds-of-event-with-n-attempts [prob-event n]
  (let [prob-not-event (- 1 prob-event)
        odds (- 1 (math1/pow prob-not-event n))
        odds-percentage (format "%.2f" (float (* 100 odds)))]
     (str odds-percentage "%") ;; return as a percentage   
    ))

(defn odds-of-all-event-with-n-attempts [prob-event n]
  (let [odds (math1/pow prob-event n)
        odds-percentage (format "%.2f" (float (* 100 odds)))
        ]
    (str odds-percentage "%") ;; return as a percentage   
    ))

(comment
  (def chances-of-ace (/ 4 52)) ;; 4 aces in a pack - so odds are 4/52 = 1/13 = 7.69%
  (odds-of-event-with-n-attempts chances-of-ace 1)
  (odds-of-event-with-n-attempts chances-of-ace 2)
  (odds-of-all-event-with-n-attempts chances-of-ace 3)
  
(int (* 100 (/ 1 13)))
  chances-of-ace


(format "%.2f" (float (/ 1 13)))

  )

