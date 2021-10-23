;;; Experiments in calculating loan schedules from algebraic expressions
;;; Uses a very simple algebraic library I have created maths.algebra
;;; GitHub: https://github.com/mkersh/ClojureTests/blob/master/src/maths/loan_schedule.clj  

(ns maths.loan-schedule
  (:require [maths.algebra :as cas]
            [clojure.pprint :as pp]))



(comment ;; Testing sanbox area

  (pp/pprint (cas/expand-schedule 5000 1 5))
  (pp/pprint (cas/expand-schedule 5000 10 12))
  (pp/pprint (cas/expand-schedule 100000 0.4 100))

;; Currently I am not expanding expr as we go along. This results in some very big expresssions
;; The next one was blowing the heap
;; I need to change my functions to allow for expr to be expanded as we go along
  (pp/pprint (cas/expand-schedule 100000 0.4 300)) ;; This one will blow the heap


;;
  )