;;; Experiments in calculating loan schedules from algebraic expressions
;;; Uses a very simple algebraic library I have created maths.algebra
;;; 
;;; This library is an enhanced version of maths.loan-schedule2. It adds support for:
;;; (1) If the interest due between (disbursement-date → first-pay-date) can not be
;;      paid off in the first instalment: 
;;      We do not capitalise this interest. 
;;      We just carry it forward and pay it off first from the monthly instalment until there is no longer any outstanding interest. 
;;;
;;; GitHub: https://github.com/mkersh/ClojureTests/blob/master/src/maths/loan_schedule2.clj  

(ns maths.loan-schedule3
  (:require [maths.algebra2 :as cas]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [java-time :as t]))

;;--------------------------------------------------------------------
;; Functions to calculate R0 - First Instalment interest-rate to use

(defn days-diff [date1 date2]
  (let [date2-local (t/local-date "yyyy-MM-dd" (subs date2 0 10))
        date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (t/time-between :days date1-local date2-local)))

(defn months-diff [date1 date2]
  (let [date2-local (t/local-date "yyyy-MM-dd" (subs date2 0 10))
        date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (t/time-between :months date1-local date2-local)))

(defn get-r0-interest-rate [disburement-date first-payment-date monthly-interest-rate]
(let [
      ;; Keeping it simple and assuming that the monthly-interest-rate is for 31 days
      ;; NOTE: Really need to see how the 30/360 daycount-method would properly handle this
      daily-interest-rate (/ monthly-interest-rate 31.0) ;; Force to a decimal else we get an error later
      days-diff (days-diff disburement-date first-payment-date)]
  (* daily-interest-rate days-diff)))


;;--------------------------------------------------------------------
;; Loan Installments
;; Taken from my orignal: https://github.com/mkersh/MambuAPINotebook/blob/master/Interest%20Calculations.ipynb 

(defn add-loan-instalment [sub-values]
  (fn [install-list i]
    (let [previous-index (- i 1)
          previous-principle_remaining (:principle_remaining (get install-list previous-index))
          interest_expected0 (cas/expr-multiply previous-principle_remaining  :r)
          ;; Try and simplify the expressions at every opportunity. That's why we are calling expr-sub
          interest_expected (cas/expr-sub interest_expected0 sub-values)
          principal_expected0 (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1))
          principal_expected (cas/expr-sub principal_expected0 sub-values)
          principle_remaining0 (cas/expr previous-principle_remaining (cas/expr-multiply previous-principle_remaining :r) (cas/term -1 [:E]))
          principle_remaining (cas/expr-sub principle_remaining0 sub-values)
          interest_remaining0 (cas/expr interest_expected (cas/term -1 [:E]))
          interest_remaining (cas/expr-sub interest_remaining0 sub-values)
          total_payment_due (cas/expr (cas/term 1 [:E]))
          nth-install {:num (+ i 1) :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_payment_due total_payment_due}]
      (conj install-list nth-install))))

(defn loan-schedule [numInstalments sub-values]
  (let [r0 (get-r0-interest-rate (:disbursement-date sub-values) (:first-payment-date sub-values) (:r sub-values))
        interest_expected0 (cas/expr-multiply (cas/expr (cas/term 1 [:P])) r0)
        interest_expected (cas/expr-sub interest_expected0 sub-values)
        principal_expected0 (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1))
        principal_expected (cas/expr-sub principal_expected0 sub-values)
        principle_remaining0 (cas/expr (cas/term 1 [:P]) interest_expected (cas/term -1 [:E]))
        principle_remaining (cas/expr-sub principle_remaining0 sub-values)
        interest_remaining0 (cas/expr interest_expected (cas/term -1 [:E]))
        interest_remaining (cas/expr-sub interest_remaining0 sub-values)
        total_payment_due (cas/expr (cas/term 1 [:E]))
        first-install {:num 1 :interest_expected interest_expected :principal_expected principal_expected  :principle_remaining principle_remaining :interest_remaining interest_remaining :total_payment_due total_payment_due}]
    (reduce (add-loan-instalment sub-values) [first-install] (range 1 numInstalments))))

(defn expand-instalment [sub-values]
  (fn [instal-obj]
    (let [num (:num instal-obj)
          mod1-applied (:mod1-applied instal-obj)
          interest_expected (cas/expr-sub2 (:interest_expected instal-obj) sub-values)
          principal_expected (cas/expr-sub2 (:principal_expected instal-obj) sub-values)
          principle_remaining (cas/expr-sub2 (:principle_remaining instal-obj) sub-values)
          interest_remaining (cas/expr-sub2 (:interest_remaining instal-obj) sub-values)
          total_payment_due (cas/expr-sub2 (:total_payment_due instal-obj) sub-values)]
      {:mod1-applied mod1-applied :num num :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_payment_due total_payment_due})))

(defn update-instalment [old-loan-sched sub-values instal-obj expanded-instal-obj i]
  (let [previous-index (- i 1)
        previous-principle_remaining (:principle_remaining (get old-loan-sched previous-index))
        previous-interest_remaining (:interest_remaining (get old-loan-sched previous-index))
        interest_expected0 (if (= i 0)
                             (:interest_expected instal-obj) ;; Get previous expression
                             (cas/expr-multiply previous-principle_remaining  :r))
        ;; Try and simplify the expressions at every opportunity. That's why we are calling expr-sub
        interest_expected (cas/expr-sub interest_expected0 sub-values)
        interest_remaining0 (if previous-interest_remaining
                              (cas/expr previous-interest_remaining interest_expected (cas/term -1 [:E]))
                              (cas/expr interest_expected (cas/term -1 [:E])))
        interest_remaining (cas/expr-sub interest_remaining0 sub-values)
        total_payment_due (cas/expr (cas/term 1 [:E]))]

        (if (> (:interest_remaining expanded-instal-obj) 0)
          (let [principal_expected0 (cas/expr (cas/term 0 []))
                principal_expected (cas/expr-sub principal_expected0 sub-values)
                principle_remaining0 (or previous-principle_remaining (cas/expr (cas/term (:P sub-values) [])))
                principle_remaining (cas/expr-sub principle_remaining0 sub-values)
                ;; mark the instalment with :mod1-applied to prevent recursing on it again
                nth-install {:mod1-applied true :num (+ i 1) :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_payment_due total_payment_due}]
            nth-install)
          (let [principal_expected0 (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1))
                principal_expected (cas/expr-sub principal_expected0 sub-values)
                principle_remaining0 (cas/expr previous-principle_remaining (cas/expr-multiply previous-principle_remaining :r) (cas/term -1 [:E]))
                principle_remaining (cas/expr-sub principle_remaining0 sub-values)
                nth-install {:num (+ i 1) :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_payment_due total_payment_due}]
            nth-install))))
                         

(defn check-for-remain-int-greater-zero0 [old-loan-sched sub-values0]
  (fn [instal-obj expanded-instal-obj i]
    (if (> (:interest_remaining expanded-instal-obj) 0)
      (let [_  (prn "Interest remain is still +ve")
            principle_remaining (or (:principle_remaining (get old-loan-sched (- i 1))) (cas/expr (cas/term (:P sub-values0) [])))
            instal-obj1 (assoc instal-obj :principle_remaining principle_remaining)
            instal-obj2 (assoc instal-obj1 :interest_remaining (cas/expr (cas/term -99 [])))]
        instal-obj2)
      instal-obj)))

(defn check-for-remain-int-greater-zero [old-loan-sched sub-values0]
  (fn [instal-obj expanded-instal-obj i]
    (let [instal-obj (update-instalment old-loan-sched sub-values0 instal-obj expanded-instal-obj i)]
    ;;(prn "InstalObj" instal-obj)
    ;;(assert false "abort abort abort")
    instal-obj
    )))

(defn need-to-recalcuate [expand-sched]
  (> (count
      (filter
       (fn [instal] (and (not (:mod1-applied instal))(> (:interest_remaining instal) 0)))
       expand-sched))
     0))

(defonce recalc-count (atom 0)) ;; Using this for debugging purposes

(defn expand-schedule0 [loan-sched numInstalments sub-values0]
  (let [prin-remain-last (:principle_remaining (get loan-sched (- numInstalments 1)))
        prin-remain-last-expanded (cas/expr-sub prin-remain-last sub-values0)
        equal-month-amount (cas/solve prin-remain-last-expanded :E)
        sub-values1 (assoc sub-values0 :E (:E equal-month-amount))
        expand-sched (mapv (expand-instalment sub-values1) loan-sched)]
    (if (need-to-recalcuate expand-sched)
      ;; Recalculate the schedule based on the modified loan-sched2
      (let
       [_ (prn "recalculate schedule" @recalc-count)
        ;; Debug code I used to figure out a problem
        ;;_ (prn "Install-0:" (get expand-sched 0))
        ;;_ (reset! recalc-count (+ @recalc-count 1))
        ;;_ (assert (< @recalc-count 30) "abort abort abort")
        loan-sched2 (mapv (check-for-remain-int-greater-zero loan-sched sub-values0) loan-sched expand-sched (range 0 numInstalments))]
        (recur loan-sched2 numInstalments sub-values0))
      ;; Expr we need to solve to get E
      {:equal-month-amount equal-month-amount
       :instalments expand-sched})))

 (defn expand-schedule [OrigPrinciple interestRatePerInstalment numInstalments disbursement-date first-payment-date]
    (let [sub-values0 {:P OrigPrinciple :r (/ interestRatePerInstalment 100) :disbursement-date disbursement-date :first-payment-date first-payment-date}
          loan-sched (loan-schedule numInstalments sub-values0)]
      (expand-schedule0 loan-sched numInstalments sub-values0)))
     
(comment
(= [1 2 3] [1 2 3])
(reset! recalc-count 0)
)

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
      (round-num (:principal_expected next-instal)) ","
      (round-num (:principle_remaining next-instal)) ","
      (round-num (:interest_remaining next-instal)) ","
      (round-num (:total_payment_due next-instal))))
      
    ;; recurse to next line
    (when (not-empty rest-instal) (dump-sched-to-csv rest-instal))))

(defn save-to-csv-file [fn sched]
  (let [fpath (get-file-path fn)]
    (io/make-parents fpath)
    (spit fpath "" :append false)
    (with-open [out-data (io/writer fpath)]
      (binding [*out* out-data]
        (println "#, Interest Expected, Principal Expected, Principle Remaining, Interest Remaining, Total Amount Due")
        (dump-sched-to-csv (:instalments sched))))))

(def test-disbursement-date "2021-01-01")
(def test-first-payment-date "2022-01-01")
(comment ;; Testing sanbox area
  (ns-unalias *ns* 'cas)
  
  (save-to-csv-file "real2-schedule1b.csv" (expand-schedule 5000 1 5 test-disbursement-date test-first-payment-date))
  (pp/pprint (expand-schedule 5000 1 5 test-disbursement-date test-first-payment-date))
  (save-to-csv-file "real2-schedule2b.csv" (expand-schedule 100000 0.4 100 test-disbursement-date test-first-payment-date))
  (pp/pprint (expand-schedule 100000 0.4 100 test-disbursement-date test-first-payment-date))) 
  
  
  ;;
