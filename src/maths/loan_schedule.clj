;;; Experiments in calculating loan schedules from algebraic expressions
;;; Uses a very simple algebraic library I have created maths.algebra
;;; GitHub: https://github.com/mkersh/ClojureTests/blob/master/src/maths/loan_schedule.clj  

(ns maths.loan-schedule
  (:require [maths.algebra2 :as cas]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

;;--------------------------------------------------------------------
;; Loan Installments
;; Taken from my orignal: https://github.com/mkersh/MambuAPINotebook/blob/master/Interest%20Calculations.ipynb 

(defn add-loan-instalment [install-list i]
  (let [previous-index (- i 1)
        previous-principle_remaining (:principle_remaining (get install-list previous-index))
        interest_expected (cas/expr-multiply previous-principle_remaining  :r)
        principle_remaining (cas/expr previous-principle_remaining (cas/expr-multiply previous-principle_remaining :r) (cas/term -1 [:E]))
        nth-install {:num (+ i 1) :interest_expected interest_expected :principle_remaining principle_remaining}]
    (conj install-list nth-install)))

(defn loan-schedule [numInstalments]
  (let [interest_expected (cas/expr-multiply (cas/expr (cas/term 1 [:P])) :r)
        principle_remaining (cas/expr (cas/term 1 [:P]) interest_expected (cas/term -1 [:E]))
        first-install {:num 1 :interest_expected interest_expected :principle_remaining principle_remaining}]
    (reduce add-loan-instalment [first-install] (range 1 numInstalments))))

(defn expand-instalment [sub-values]
  (fn [instal-obj]
    (let [num (:num instal-obj)
          interest_expected (cas/expr-sub2 (:interest_expected instal-obj) sub-values)
          principle_remaining (cas/expr-sub2 (:principle_remaining instal-obj) sub-values)]
      {:num num :interest_expected interest_expected :principle_remaining principle_remaining})))
(defn expand-schedule [OrigPrinciple interestRatePerInstalment numInstalments]
  (let [loan-sched (loan-schedule numInstalments)
        prin-remain-last (:principle_remaining (get loan-sched (- numInstalments 1)))
        sub-values0 {:P OrigPrinciple :r (/ interestRatePerInstalment 100)}
        prin-remain-last-expanded (cas/expr-sub prin-remain-last sub-values0)
        equal-month-amount (cas/solve prin-remain-last-expanded :E)
        sub-values1 (assoc sub-values0 :E (:E equal-month-amount))
        expand-sched (mapv (expand-instalment sub-values1) loan-sched)]
  ;; Expr we need to solve to get E
    {:equal-month-amount equal-month-amount
     :instalments expand-sched}))

;;--------------------------------------------------------------------
;; Print to CSV functions
;; So that you can view in a spreadsheet tool

(defonce CSV-ROOT (atom "CSV-FILES/"))

(defn get-file-path [fn]
  (str @CSV-ROOT fn))

(defn round-num [num]
  (format "%.2f" num))

(defn dump-sched-to-csv [instal-list]
  (let [next-instal (first instal-list)
        rest-instal (rest instal-list)]
    (println
     (str
      (:num next-instal) ","
      (round-num (:interest_expected next-instal)) ","
      (round-num (:principle_remaining next-instal))))
    ;; recurse to next line
    (when (not-empty rest-instal) (dump-sched-to-csv rest-instal))))

(defn save-to-csv-file [fn sched]
  (let [fpath (get-file-path fn)]
    (io/make-parents fpath)
    (spit fpath "" :append false)
    (with-open [out-data (io/writer fpath)]
      (binding [*out* out-data]
        (println "#, Interest Expected, Principle Remaining")
        (dump-sched-to-csv (:instalments sched))))))

(comment ;; Testing sanbox area
  (ns-unalias *ns* 'cas)

  (save-to-csv-file "test.csv" (expand-schedule 5000 1 5))
  (pp/pprint (expand-schedule 5000 1 5))
  (pp/pprint (expand-schedule 5000 10 12))
  (save-to-csv-file "test.csv" (expand-schedule 100000 0.4 100))
  (pp/pprint (expand-schedule 100000 0.4 100))

;; Currently I am not expanding expr as we go along. This results in some very big expresssions
;; The next one was blowing the heap
;; I need to change my functions to allow for expr to be expanded as we go along
  (pp/pprint (expand-schedule 100000 0.4 300)) ;; This one will blow the heap


;;
  )