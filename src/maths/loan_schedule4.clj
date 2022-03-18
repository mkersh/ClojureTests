;;; Experiments in calculating loan schedules from algebraic expressions
;;; Uses a very simple algebraic library I have created maths.algebra2
;;; 
;;; This library is an enhanced version of maths.loan-schedule3. It adds support for:
;;; (1) planned-payment holidays within the schedule. Where a holiday can be
;;;     - principle-holiday 
;;;     - principal+interest-holiday
;;;         - interest-accrued (probably will only implement this one initially)
;;;         - interest-writeoff
;;;         - nterest-capitalise
;;;
;;; #bookmark= 97d12c61-e5ad-4037-88fb-0957be83a1d7
;;; GitHub: https://github.com/mkersh/ClojureTests/blob/master/src/maths/loan_schedule4.clj  

(ns maths.loan-schedule4
  (:require [maths.algebra2 :as cas]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [java-time :as t]))

(defonce LOAN-SCHEDULE-EDIT (atom {}))

(def debug (atom false))

;;--------------------------------------------------------------------
;; Functions to calculate R0 - First Instalment interest-rate to use

(defn days-diff [date1 date2]
  (let [date2-local (t/local-date "yyyy-MM-dd" (subs date2 0 10))
        date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (t/time-between :days date1-local date2-local)))

(defn months-diff [date1 date2]
  (let [date2-local (t/local-date "yyyy-MM-dd" (subs date2 0 10))
        date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))
        months-diff  (t/time-between :months date1-local date2-local)
        date1-day (.getDayOfMonth date1-local)
        last-day1 (.lengthOfMonth date1-local)
        date2-day (.getDayOfMonth date2-local)
        date2-month (.getMonthValue date2-local)
        last-day2 (.lengthOfMonth date2-local)]
    ;; This next if is handling an anomaly with (t/time-between :months ...)
    ;; without it the following were returning wrong number of months:
    ;;        (months-diff "2020-12-31" "2021-02-28")
    ;;        (months-diff "2021-01-31" "2021-02-28")
    (if (and (= date2-month 2)
             (= date1-day last-day1)
             (= date2-day last-day2))
      (+ months-diff 1)
      months-diff)))

;; The next function tries to replicate the EXCEL DAYS360 function 
;; NOTE: I still don't have a 100% match when dates involve 28/29 February
(defn days360 [date1 date2]
  (let [date2-local (t/local-date "yyyy-MM-dd" (subs date2 0 10))
        date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))
        date1-day0 (.getDayOfMonth date1-local)
        date1-last-dayofmonth (= date1-day0 (.lengthOfMonth date1-local))
        date1-day  (if date1-last-dayofmonth
                     30
                     date1-day0)
        date1-month (.getMonthValue date1-local)
        date2-day0 (.getDayOfMonth date2-local)
        date2-day  (if (and (= date2-day0 (.lengthOfMonth date2-local))
                            (or (not= date1-month 2)
                                date1-last-dayofmonth))
                     30
                     date2-day0)
        months-diff (months-diff date1 date2)]
    (if (>= date2-day date1-day)
      (let [days-diff (- date2-day  date1-day)
         ;;_ (prn "here1" date1-day date2-day date1-month  date1-last-dayofmonth (not= date1-month 2))
            ]
        (+ (* months-diff 30) days-diff))
      (let [date1-day0 (- 30 (.getDayOfMonth date1-local))
            date1-day (if (< date1-day0 0) 0 date1-day0)
            days-diff0 (+ date2-day date1-day)
            _ (prn "here")
            days-diff (if (> days-diff0 30) 30 days-diff0)]
        (+ (* months-diff 30) days-diff)))))


(defn months-diff2 [date1 date2]
  (let [days360 (days360 date1 date2)]
    (/ days360 30.00)))


(defn get-r0-interest-rate0 [disburement-date first-payment-date monthly-interest-rate]
  (let [;; Keeping it simple and assuming that the monthly-interest-rate is for 31 days
      ;; NOTE: Really need to see how the 30/360 daycount-method would properly handle this
        daily-interest-rate (/ monthly-interest-rate 31.0) ;; Force to a decimal else we get an error later
        days-diff (days-diff disburement-date first-payment-date)]
    (* daily-interest-rate days-diff)))

;; This below matches Excels DAYS360
(defn get-r0-interest-rate [disburement-date first-payment-date monthly-interest-rate]
  (let [months-diff (months-diff2 disburement-date first-payment-date)]
    (* monthly-interest-rate months-diff)))


;;--------------------------------------------------------------------
;; Loan Installments
;; Taken from my orignal: https://github.com/mkersh/MambuAPINotebook/blob/master/Interest%20Calculations.ipynb 

(declare check-for-principle-holiday check-for-specific-total-amount check-for-prin-remain-holiday)

(defn add-loan-instalment [sub-values]
  (fn [install-list i]
    (let [previous-index (- i 1)
          previous-principle_remaining (:principle_remaining (get install-list previous-index))
          interest_expected0 (cas/expr-multiply previous-principle_remaining  :r)
          ;; Try and simplify the expressions at every opportunity. That's why we are calling expr-sub
          interest_expected (cas/expr-sub interest_expected0 sub-values)
          principal_expected0 (check-for-principle-holiday i (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1)))
          principal_expected (cas/expr-sub principal_expected0 sub-values)
          principle_remaining0 (check-for-prin-remain-holiday i (cas/expr previous-principle_remaining (cas/expr-multiply previous-principle_remaining :r) (cas/term -1 [:E])) previous-principle_remaining)
          principle_remaining (cas/expr-sub principle_remaining0 sub-values)
          interest_remaining0 (cas/expr interest_expected (cas/term -1 [:E]))
          interest_remaining (cas/expr-sub interest_remaining0 sub-values)
          total_remain principle_remaining
          total_payment_due (check-for-specific-total-amount i (cas/expr (cas/term 1 [:E])) principal_expected interest_expected)
          nth-install {:num (+ i 1) :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_remain total_remain :total_payment_due total_payment_due}]
      (conj install-list nth-install))))

(defn loan-schedule [numInstalments sub-values]
  (let [r0 (get-r0-interest-rate (:disbursement-date sub-values) (:first-payment-date sub-values) (:r sub-values))
        interest_expected0 (cas/expr-multiply (cas/expr (cas/term 1 [:P])) r0)
        interest_expected (cas/expr-sub interest_expected0 sub-values)
        principal_expected0 (check-for-principle-holiday 0 (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1)))
        principal_expected (cas/expr-sub principal_expected0 sub-values)
        principle_remaining0 (check-for-prin-remain-holiday 0 (cas/expr (cas/term 1 [:P]) interest_expected (cas/term -1 [:E])) (cas/expr (cas/term 1 [:P])))
        principle_remaining (cas/expr-sub principle_remaining0 sub-values)
        interest_remaining0 (cas/expr interest_expected (cas/term -1 [:E]))
        interest_remaining (cas/expr-sub interest_remaining0 sub-values)
        total_remain principle_remaining
        total_payment_due (check-for-specific-total-amount 0 (cas/expr (cas/term 1 [:E])) principal_expected interest_expected)
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
        ;;total_payment_due (cas/expr (cas/term 1 [:E]))
        ]

    ;; if recalc-list <> [] then always force principal+interest
    (if (and (not (revert-install? recalc-list i)) (> (:interest_remaining expanded-instal-obj) 0))
      (let [principal_expected0 (cas/expr (cas/term 0 []))
            principal_expected (cas/expr-sub principal_expected0 sub-values)
            principle_remaining0 (or previous-principle_remaining (cas/expr (cas/term (:P sub-values) [])))
            principle_remaining (cas/expr-sub principle_remaining0 sub-values)
            total_remain0 (cas/expr principle_remaining  interest_remaining)
            total_remain (cas/expr-sub total_remain0 sub-values)
            total_payment_due (check-for-specific-total-amount i (cas/expr (cas/term 1 [:E])) principal_expected interest_expected)
            ;; mark the instalment with :mod1-applied to prevent recursing on it again
            nth-install {:mod1-applied true :num (+ i 1) :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_remain total_remain :total_payment_due total_payment_due}]
        nth-install)
      (let [principal_expected0 (if (revert-install? recalc-list i)
                                  ;; If i is on recalc-list then need to clear some remaining interest_remaining
                                  (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1) (cas/expr-multiply interest_remaining -1))
                                  (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1)))

            prev-instal-mod1 (:mod1-applied (get install-list previous-index))
            ;; principal expected+remaining is different if the previous instalment had previous-interest_remaining > 0
            ;; If this is the case then prev-instal-mod1 will equal true as well
            principal_expected1a (if prev-instal-mod1
                                 (cas/expr-sub (cas/expr principal_expected0 (cas/expr-multiply previous-interest_remaining -1)) sub-values)
                                 (cas/expr-sub principal_expected0 sub-values))
            principal_expected (check-for-principle-holiday i principal_expected1a)
            principle_remaining0a (if prev-instal-mod1
                                   (cas/expr previous-principle_remaining interest_expected previous-interest_remaining (cas/term -1 [:E]))
                                   (cas/expr previous-principle_remaining interest_expected (cas/term -1 [:E])))
            principle_remaining0 (check-for-prin-remain-holiday i principle_remaining0a previous-principle_remaining)
            principle_remaining (cas/expr-sub principle_remaining0 sub-values)
            total_remain0 (cas/expr principle_remaining)
            total_remain (cas/expr-sub total_remain0 sub-values)
            total_payment_due (check-for-specific-total-amount i (cas/expr (cas/term 1 [:E])) principal_expected interest_expected)
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

;; #bookmark= 1031c4ec-f363-4294-8d2a-bd29b099f130
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
        rest-instal (rest instal-list)
        prin-exp (round-num (:principal_expected next-instal))]
    ;; Debug to track a problem
    (when (= prin-exp "506.62")
      (prn "Interest Expected" (:interest_expected next-instal))
      (prn "Prin expected" (:principal_expected next-instal))
      (prn "Total payment" (:total_payment_due next-instal)))
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

;;--------------------------------------------------------------------
;; Edit-schedule functions
;; Features to allow for planned-payment holidays
;;
;; uses @LOAN-SCHEDULE-EDIT map

(defn edit-map-field [edit-map inst-num field val]
  (let [
        inst-obj  (get edit-map inst-num {})
        inst-obj2 (assoc inst-obj field val)
        edit-map2 (assoc edit-map inst-num inst-obj2)]
    edit-map2))

(defn clear-schedule-edits []
  (reset! LOAN-SCHEDULE-EDIT {}))

(defn edit-sched-interest-only [inst-num]
  (let [edit-map @LOAN-SCHEDULE-EDIT
        edit-map2 (edit-map-field edit-map inst-num :pricipal-to-pay 0)]
    (reset! LOAN-SCHEDULE-EDIT edit-map2)))

(defn edit-sched-interest-only2 [inst-list]
  (clear-schedule-edits)
  (map edit-sched-interest-only inst-list))

;; No payments for instalment=inst-num 
;; Accrued-interest to be paid after holiday ends
(defn edit-sched-full-holiday [inst-num]
(let [edit-map @LOAN-SCHEDULE-EDIT
      edit-map2 (edit-map-field edit-map inst-num :pricipal-to-pay 0)
      edit-map3 (edit-map-field edit-map2 inst-num :interest-to-pay 0)]
  (reset! LOAN-SCHEDULE-EDIT edit-map3))
)

(defn check-for-principle-holiday [inst-num-1 calculated-expr]
  (let [inst-num (+ inst-num-1 1)
        edit-map @LOAN-SCHEDULE-EDIT
        inst-obj  (get edit-map inst-num)
        pricipal-to-pay (:pricipal-to-pay inst-obj)]
    (if pricipal-to-pay
          ;; specific principal amount has been defined use this
     (do
       (prn "Holiday" inst-num-1)
       (cas/expr (cas/term pricipal-to-pay [])))
          ;; else use
      calculated-expr)))

(defn check-for-prin-remain-holiday [inst-num-1 no-holiday-expr previous-prin-expr ]
  (let [inst-num (+ inst-num-1 1)
        edit-map @LOAN-SCHEDULE-EDIT
        inst-obj  (get edit-map inst-num)
        pricipal-to-pay (:pricipal-to-pay inst-obj)]
    (if pricipal-to-pay
      ;; specific principal amount has been defined use this
      ;;(cas/expr previous-prin-expr (cas/expr-multiply previous-prin-expr :r) (cas/term (* -1 pricipal-to-pay) []))
      previous-prin-expr
      ;; else use
      no-holiday-expr)))


(defn check-for-specific-total-amount [inst-num-1 calculated-expr prin-expected int-expected]
  (let [inst-num (+ inst-num-1 1)
        edit-map @LOAN-SCHEDULE-EDIT
        inst-obj  (get edit-map inst-num)]
    (if inst-obj
      ;; specific total-amount
      (cas/expr prin-expected int-expected)
      ;; else use
      calculated-expr)))

(comment
(* -1 0)
;;
)


(comment ;; Testing sanbox area
  (ns-unalias *ns* 'cas)
  
  @LOAN-SCHEDULE-EDIT
  (clear-schedule-edits)


  (edit-sched-interest-only2 [1 2 3 4 5 6 7 8 9 10])
  (edit-sched-interest-only2 [3 5 7 9 11])
  (save-to-csv-file "test-ls4-1b.csv" (expand-schedule 10000 (/ 9.9M 12.0) 12 "2022-01-01" "2022-02-01"))

(* 11 954.71)
  (save-to-csv-file "test-ls4-2b.csv" (expand-schedule 10000 (/ 9.9M 12.0) 84 "2022-01-01" "2023-01-01"))

  ;;
  )





