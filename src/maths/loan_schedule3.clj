;;; Experiments in calculating loan schedules from algebraic expressions
;;; Uses a very simple algebraic library I have created maths.algebra2
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

(def debug (atom false))

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
  (let [;; Keeping it simple and assuming that the monthly-interest-rate is for 31 days
      ;; NOTE: Really need to see how the 30/360 daycount-method would properly handle this
        daily-interest-rate (/ monthly-interest-rate 31.0) ;; Force to a decimal else we get an error later
        days-diff (days-diff disburement-date first-payment-date)]
    (* daily-interest-rate days-diff)))

;; This below matches Excels DAYS360
(defn get-r0-interest-rate1 [disburement-date first-payment-date monthly-interest-rate]
  (let [months-diff (months-diff disburement-date first-payment-date)]
    (* monthly-interest-rate months-diff)))

(comment
(let [r0 (get-r0-interest-rate "2019-09-25" "2019-12-25" 5.00M)]
(* 1000000.0M (/ r0 100))

)

;;
)


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
          total_remain principle_remaining
          total_payment_due (cas/expr (cas/term 1 [:E]))
          nth-install {:num (+ i 1) :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_remain total_remain :total_payment_due total_payment_due}]
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
        total_remain principle_remaining 
        total_payment_due (cas/expr (cas/term 1 [:E]))
        first-install {:num 1 :interest_expected interest_expected :principal_expected principal_expected  :principle_remaining principle_remaining :interest_remaining interest_remaining :total_remain total_remain  :total_payment_due total_payment_due}]
    (reduce (add-loan-instalment sub-values) [first-install] (range 1 numInstalments))))

(defn expand-instalment [sub-values]
  (fn [instal-obj]
    (let [num (:num instal-obj)
          mod1-applied (:mod1-applied instal-obj)
          interest_expected (cas/expr-sub2 (:interest_expected instal-obj) sub-values)
          principal_expected (cas/expr-sub2 (:principal_expected instal-obj) sub-values)
          principle_remaining (cas/expr-sub2 (:principle_remaining instal-obj) sub-values)
          interest_remaining (cas/expr-sub2 (:interest_remaining instal-obj) sub-values)
          total_remain (cas/expr-sub2 (:total_remain instal-obj) sub-values)
          total_payment_due (cas/expr-sub2 (:total_payment_due instal-obj) sub-values)]
      {:mod1-applied mod1-applied :num num :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_remain total_remain :total_payment_due total_payment_due})))

(defn revert-install? [recalc-list i]
  ;; is i in recalc-list
  (some #{i} recalc-list))

(defn update-instalment
  [old-loan-sched sub-values install-list expanded-instal-obj i recalc-list]
  (let [previous-index (- i 1)
        previous-principle_remaining (if (= i 0)
                                       (cas/expr (cas/term (:P sub-values) []))
                                       (:principle_remaining (get install-list previous-index)))
        previous-interest_remaining (if (= i 0)
                                      (cas/expr (cas/term 0 []))
                                      (:interest_remaining (get install-list previous-index)))
        interest_expected0 (if (= i 0)
                             (:interest_expected (get old-loan-sched 0)) ;; Get previous expression
                             (cas/expr-multiply previous-principle_remaining  :r))
        ;; Try and simplify the expressions at every opportunity. That's why we are calling expr-sub
        interest_expected (cas/expr-sub interest_expected0 sub-values)
        interest_remaining0 (if (= i 0)
                              (cas/expr interest_expected (cas/term -1 [:E]))
                              (cas/expr previous-interest_remaining interest_expected (cas/term -1 [:E])))
        interest_remaining (cas/expr-sub interest_remaining0 sub-values)
        total_payment_due (cas/expr (cas/term 1 [:E]))]

    ;; if recalc-list <> [] then always force principal+interest
    (if (and (not (revert-install? recalc-list i)) (> (:interest_remaining expanded-instal-obj) 0))
      (let [principal_expected0 (cas/expr (cas/term 0 []))
            principal_expected (cas/expr-sub principal_expected0 sub-values)
            principle_remaining0 (or previous-principle_remaining (cas/expr (cas/term (:P sub-values) [])))
            principle_remaining (cas/expr-sub principle_remaining0 sub-values)
            total_remain0 (cas/expr principle_remaining  interest_remaining)
            total_remain (cas/expr-sub total_remain0 sub-values)
            ;; mark the instalment with :mod1-applied to prevent recursing on it again
            nth-install {:mod1-applied true :num (+ i 1) :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_remain total_remain :total_payment_due total_payment_due}]
        nth-install)
      (let [principal_expected0 (if (revert-install? recalc-list i)
                                  ;; If i is on recalc-list then need to clear some remaining interest_remaining
                                  (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1) (cas/expr-multiply interest_remaining -1))
                                  (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1)))
            principal_expected (cas/expr-sub principal_expected0 sub-values)
            principle_remaining0 (cas/expr previous-principle_remaining interest_expected (cas/term -1 [:E]))
            principle_remaining (cas/expr-sub principle_remaining0 sub-values)
            total_remain0 (cas/expr principle_remaining)
            total_remain (cas/expr-sub total_remain0 sub-values)
            nth-install {:num (+ i 1) :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_remain total_remain :total_payment_due total_payment_due}]
        nth-install))))

;; Returns a function to use in a reduce
(defn check-for-remain-int-greater-zero [old-loan-sched sub-values0 expand-sched recalc-list]
  (fn [install-list i]
    (let [instal-obj (update-instalment old-loan-sched sub-values0 install-list (get expand-sched i) i recalc-list)]
      (conj install-list instal-obj))))

(defn enumerate [c]
  (map
   (fn [i obj] [i obj])
   (iterate inc 0) c))


(defn need-to-recalcuate [expand-sched]
  (let [expand-sched1 (enumerate expand-sched)
        recalc-needed (filter
                      (fn [[_ instal]] (and (not (:mod1-applied instal)) (> (:interest_remaining instal) 0)))
                      expand-sched1)]
    (if (> (count recalc-needed) 0)
      (mapv (fn [[i _]] i) recalc-needed)
      nil)))

(defn expand-schedule-final [loan-sched numInstalments sub-values0]
  (let [total-remain-last (:total_remain (get loan-sched (- numInstalments 1)))
        total-remain-last-expanded (cas/expr-sub total-remain-last sub-values0)
        equal-month-amount (cas/solve total-remain-last-expanded :E)
        sub-values1 (assoc sub-values0 :E (:E equal-month-amount))
        expand-sched (mapv (expand-instalment sub-values1) loan-sched)]
    ;; After expanding check to see if the last of the interest-only instalments is still valid
    (if-let [recalc-list (need-to-recalcuate expand-sched)]
      (let
       [loan-sched2 (reduce (check-for-remain-int-greater-zero loan-sched sub-values0 expand-sched recalc-list) [] (range 0 numInstalments))
        total-remain-last (:total_remain (get loan-sched2 (- numInstalments 1)))
        total-remain-last-expanded (cas/expr-sub total-remain-last sub-values0)
        equal-month-amount (cas/solve total-remain-last-expanded :E)
        sub-values1 (assoc sub-values0 :E (:E equal-month-amount))
        expand-sched (mapv (expand-instalment sub-values1) loan-sched2)]
        {:equal-month-amount equal-month-amount
         :instalments expand-sched})
      ;; Else  
      {:equal-month-amount equal-month-amount
       :instalments expand-sched})))


(defn expand-schedule0 [loan-sched numInstalments sub-values0]
  (let [total-remain-last (:total_remain (get loan-sched (- numInstalments 1)))
        total-remain-last-expanded (cas/expr-sub total-remain-last sub-values0)
        equal-month-amount (cas/solve total-remain-last-expanded :E)
        sub-values1 (assoc sub-values0 :E (:E equal-month-amount))
        expand-sched (mapv (expand-instalment sub-values1) loan-sched)]
    (if (need-to-recalcuate expand-sched)
      ;; Recalculate the schedule based on the modified loan-sched2
      (let
       [loan-sched2 (reduce (check-for-remain-int-greater-zero loan-sched sub-values0 expand-sched []) [] (range 0 numInstalments))]
        (recur loan-sched2 numInstalments sub-values0))
      ;; Expr we need to solve to get E
      (let [_ (reset! debug true)
            loan-sched2 (reduce (check-for-remain-int-greater-zero loan-sched sub-values0 expand-sched []) [] (range 0 numInstalments))]
        (expand-schedule-final loan-sched2 numInstalments sub-values0)))))

 (defn expand-schedule [OrigPrinciple interestRatePerInstalment numInstalments disbursement-date first-payment-date]
   (reset! debug false)
    (let [sub-values0 {:P OrigPrinciple :r (/ interestRatePerInstalment 100) :disbursement-date disbursement-date :first-payment-date first-payment-date}
          loan-sched (loan-schedule numInstalments sub-values0)]
      (expand-schedule0 loan-sched numInstalments sub-values0)))

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
      (:mod1-applied next-instal) ","
      (round-num (:interest_expected next-instal)) ","
      (round-num (:principal_expected next-instal)) ","
      (round-num (:principle_remaining next-instal)) ","
      (round-num (:interest_remaining next-instal)) ","
      (round-num (:total_remain next-instal)) ","
      (round-num (:total_payment_due next-instal))))
      
    ;; recurse to next line
    (when (not-empty rest-instal) (dump-sched-to-csv rest-instal))))

(defn save-to-csv-file [fn sched]
  (let [fpath (get-file-path fn)]
    (io/make-parents fpath)
    (spit fpath "" :append false)
    (with-open [out-data (io/writer fpath)]
      (binding [*out* out-data]
        (println "#, ModFlag, Interest Expected, Principal Expected, Principle Remaining, Interest Remaining, Total Remaining, Total Amount Due")
        (dump-sched-to-csv (:instalments sched))))))

(def test-disbursement-date "2021-01-01")
(def test-first-payment-date "2022-01-01")
(def test-first-payment-date2 "2021-01-05")
(comment ;; Testing sanbox area
  (ns-unalias *ns* 'cas)

  (save-to-csv-file "real2-schedule1b.csv" (expand-schedule 5000 1 5 test-disbursement-date test-first-payment-date))
  (pp/pprint (expand-schedule 5000 1 5 test-disbursement-date test-first-payment-date))
  (save-to-csv-file "real2-schedule2b.csv" (expand-schedule 100000 0.4 100 test-disbursement-date test-first-payment-date))
  (pp/pprint (expand-schedule 100000 0.4 100 test-disbursement-date test-first-payment-date))
  (save-to-csv-file "real2-schedule1c.csv" (expand-schedule 5000 1 5 test-disbursement-date test-first-payment-date2))


  ;; Testing some specific loans examples
  (save-to-csv-file "testsch1.csv" (expand-schedule 10000 (/ 9.9M 12.0) 84 "2021-04-14" "2021-06-15"))
  (save-to-csv-file "testsch2.csv" (expand-schedule 12550 (/ 19.4M 12.0) 78 "2020-07-08" "2020-10-18"))

  ;; Example from client P
  (save-to-csv-file "testsch3.csv" (expand-schedule 1000 4.2350610718397075M 24 "2021-03-21" "2021-04-04")) ;; 65.70 emi example
  (save-to-csv-file "testsch3b.csv" (expand-schedule 1000 4.24M 24 "2021-03-21" "2021-04-19")) ;; 67.05 emi example
  (save-to-csv-file "testsch3c.csv" (expand-schedule 1000 4.24M 48 "2021-03-21" "2021-05-04")) ;; 49.94, 48m
  (save-to-csv-file "testsch4.csv" (expand-schedule 1000 4.24M 24 "2021-08-01" "2021-09-14")) ;; 68.39 emi example
  (save-to-csv-file "testsch5.csv" (expand-schedule 1000 4.24M 36 "2021-08-01" "2021-09-14")) ;; 55.60 emi example
  (save-to-csv-file "testsch6.csv" (expand-schedule 1000 4.24M 48 "2021-08-01" "2021-09-14")) ;; 49.92 emi example
  

(save-to-csv-file "testsch7b.csv" (expand-schedule 1000000 5.00M 24 "2019-09-25" "2019-12-25")) ;; Simar's eg

(get-r0-interest-rate "2019-09-25" "2019-12-25" 5.00M)

  (days-diff "2019-09-25" "2019-12-25")
  (months-diff "2021-12-15" "2026-09-15")
(/ 57.0 3)


  (/ 60 365)
  (* 1000000 14.67741935483871M)
  ;; 6.16
  (/ 6.16M 100.0M)
  (* 1000.0M 0.0616M)

  (defn calc-int [stdate edate dailyPercentageRate prin]
  (let [days (days-diff stdate edate)
        day-rate (/ dailyPercentageRate 100.0M)
        period-rate (* days day-rate)
        interest (* prin period-rate)]
    interest)
  )

  (defn calc-int2 [stdate edate monthPercentageRate prin]
    (let [days (days-diff stdate edate)
          month-rate (/ monthPercentageRate 100.0M)
          averageDaysPerMonth (/ 365 12.0)
          daysToUse (/ days averageDaysPerMonth)
          interest (* prin daysToUse month-rate)]
      interest))

(defn pow [b e] (Math/pow b e))

;; MonthlyRate=(1+APR)^(1/12)-1

(defn month-interest-from-apr [apr]
(let [base (+ 1 apr)
      exp (/ 1 12)
      p (pow base exp)
      ]
      (- p 1)
      )
)

;; (MonthlyRate+1)^12 -1=APR
(defn apr-interest-from-monthly [monthly]
  (let [base (+ 1 monthly)
        exp 12
        p (pow base exp)]
    (- p 1)))


(defn apr-calc [prin total-int n]
(let [int-over-prin (/ total-int prin)
      per-n (/ int-over-prin n )
      per-year (* per-n 12) ;; assuming n is months
      apr-percent (* per-year 100)
      ]
      apr-percent
      )
)

(apr-calc 1000.0M 1396.03M 48.0)
(month-interest-from-apr (/ 64.50M 100.0))
(apr-interest-from-monthly 0.042350610718397075)

(* 0.042350610718397075 12 100)
(pow 2 (/ 1 2))
  (calc-int "2021-08-01" "2021-09-14" 0.14 1000.0M)
  (calc-int2 "2021-08-01" "2021-09-14" 4.2350610718397075M 1000.0M)
  ;;
  ) 
  


  

