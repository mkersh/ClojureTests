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
(defonce NON-BUSDAY-CALENDAR (atom {}))
(defonce HOLIDAY-INTEREST_CAP (atom 0))
(defonce INT_REMAIN-ZERO-TOGGLE (atom true))
(defonce DEBUG-COUNT (atom 0))

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

(defn add-month [date1]
  (let [date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (str (t/plus date1-local (t/months 1)))))

(defn add-day [date1]
  (let [date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (str (t/plus date1-local (t/days 1)))))


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
      (let [days-diff (- date2-day  date1-day)]
         ;;_ (prn "here1" date1-day date2-day date1-month  date1-last-dayofmonth (not= date1-month 2))

        (+ (* months-diff 30) days-diff))
      (let [date1-day0 (- 30 (.getDayOfMonth date1-local))
            date1-day (if (< date1-day0 0) 0 date1-day0)
            days-diff0 (+ date2-day date1-day)
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
(defn get-r0-interest-rate [daycount-model disburement-date first-payment-date monthly-interest-rate]
  (condp = daycount-model
    :30-360
    (let [months-diff (months-diff2 disburement-date first-payment-date)]
      (* monthly-interest-rate months-diff))
    :actual-365
    (let [apr (* monthly-interest-rate 12.0)
          day-rate (/ apr 365.0)
          days-diff (days-diff disburement-date first-payment-date)]
      (* days-diff day-rate))  
      ))

;;--------------------------------------------------------------------
;; basic support functions

(defn enumerate [c]
  (map
   (fn [i obj] [i obj])
   (iterate inc 0) c))

(defn revert-install? [recalc-list i]
  ;; is i in recalc-list
  (some #{i} recalc-list))

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
      (:payment_duedate next-instal) ","
      (:int_days next-instal) ","
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
        (println "#, ModFlag, DueDate, Int Days, Interest Expected, Principal Expected, Principle Remaining, Interest Remaining, Total Remaining, Total Amount Due")
        (dump-sched-to-csv (:instalments sched))))))


;;--------------------------------------------------------------------
;; Edit-schedule functions
;; Features to allow for planned-payment holidays
;;
;; uses @LOAN-SCHEDULE-EDIT map

(defn edit-map-field [edit-map inst-num field val]
  (let [inst-obj  (get edit-map inst-num {})
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
  (dorun (map edit-sched-interest-only inst-list)))

;; No payments for instalment=inst-num 
;; Accrued-interest to be paid after holiday ends
(defn edit-sched-full-holiday [[inst-num obj]]
  (let [edit-map @LOAN-SCHEDULE-EDIT
        edit-map2 (edit-map-field edit-map inst-num :pricipal-to-pay (:pricipal-to-pay obj))
        edit-map3 (edit-map-field edit-map2 inst-num :interest-to-pay (:interest-to-pay obj))]
    (reset! LOAN-SCHEDULE-EDIT edit-map3)))

(defn edit-schedule [inst-list]
  (clear-schedule-edits)
  (dorun (map edit-sched-full-holiday inst-list)))


(defn check-for-principle-holiday [inst-num-1 calculated-expr]
  (let [inst-num (+ inst-num-1 1)
        edit-map @LOAN-SCHEDULE-EDIT
        inst-obj  (get edit-map inst-num)
        pricipal-to-pay (:pricipal-to-pay inst-obj)]
    (if pricipal-to-pay
      ;; specific principal amount has been defined use this
      (cas/expr (cas/term pricipal-to-pay []))
      ;; else use
      calculated-expr)))

;; During a holiday how much interest do we charge?
;; We support:
;; (1) Determine the interest to pay from @HOLIDAY-INTEREST_CAP
;;     NOTE: This would never be used in production but is very useful for testing
;; (2) Have an explicit interest amount defined in @LOAN-SCHEDULE-EDIT
;; (3) For default principal-only holiday rule:
;;         interest-to-pay = principal-currently-remaining * interest-rate-for-this-instalment-period
;;
(defn holiday-interest-cap [install-list expand-sched inst-num-1 new-inst-obj int-expected]
  (let [inst-num (+ inst-num-1 1)
        edit-map @LOAN-SCHEDULE-EDIT
        inst-obj  (get edit-map inst-num)
        pricipal-to-pay (:pricipal-to-pay inst-obj)
        interest-to-pay (:interest-to-pay inst-obj)]
    (if pricipal-to-pay
      ;;int-expected
      (let [int-cap0 (or interest-to-pay @HOLIDAY-INTEREST_CAP)
            interest_expected (if expand-sched (:interest_expected expand-sched) int-cap0)
            int-cap (if (> int-cap0 interest_expected) interest_expected int-cap0)
            prev-instal (get install-list (- inst-num-1 1))
            principle_remaining (if prev-instal (:principle_remaining prev-instal)
                                    (cas/expr (cas/term 1 [:P])))
            int-rate (:r new-inst-obj)]
        (if (and (= int-cap 0) (not interest-to-pay))
          ;; mechanism to override/force the interest charged during a holiday - useful for testing
          (cas/expr (cas/term pricipal-to-pay []) (cas/expr-multiply principle_remaining int-rate))
          ;; During a principal-holiday only charge the standard monthly interest
          (cas/expr (cas/term pricipal-to-pay []) (cas/term int-cap []))))
      int-expected)))

;;--------------------------------------------------------------------
;; Calendar functions
;; Allow for business-day calendar to be configured
;; @NON-BUSDAY-CALENDAR is our datastore for non-business-day(s)

(defn set-non-business-days [dates-list]
  (let [bd-map (reduce (fn [res-map date-str]
                         (assoc res-map date-str true))
                       {} dates-list)]
    (reset! NON-BUSDAY-CALENDAR bd-map)))

(defn clear-non-business-days []
    (reset! NON-BUSDAY-CALENDAR {}))

;; Given a date-str return the nearest business day
(defn get-nearest-business-day [date-str]
(let [is-bus-day? (not (get @NON-BUSDAY-CALENDAR date-str))]
  (if is-bus-day?
    date-str
    ;; At the moment we will always move forward to find nearest business-day
    ;; In a production system you really need to support either moving forward or backward
    ;; to find the nearest business-day
    (get-nearest-business-day (add-day date-str)))))

(comment
(clear-non-business-days)
(set-non-business-days ["2022-04-03" "2022-04-04" "2022-04-05"])
(get-nearest-business-day "2022-04-03")
;;
)

;;--------------------------------------------------------------------
;; Loan Installments
;; Taken from my orignal: https://github.com/mkersh/MambuAPINotebook/blob/master/Interest%20Calculations.ipynb 

;; ------------------------------------------
;; Logic for calculating the value of a specific instalment field
;; Function call multiple times from get-inst-obj to generate the entire instalment
;;
(defn install-value [new-inst-obj i field install-list install-previous-list sub-values expand-sched recalc-list]
  (let [previous-index (- i 1)
        prin-holiday (check-for-principle-holiday i nil) 
        daycount-model (:daycount-model sub-values)
        total_payment_due (:total_payment_due new-inst-obj)
        previous-principle_remaining (or (:principle_remaining (get install-list previous-index))
                                         (cas/expr (cas/term 1 [:P])))
        previous-interest_remaining (if (= i 0)
                                      (cas/expr (cas/term 0 []))
                                      (:interest_remaining (get install-list previous-index)))
        interest_expected (:interest_expected new-inst-obj)
        interest_expected_capped (:interest_expected_capped new-inst-obj)
        interest_remaining0 (:interest_remaining0 new-inst-obj)
        interest_remaining (:interest_remaining new-inst-obj)
        principle_remaining (:principle_remaining new-inst-obj)
        principal_expected (:principal_expected new-inst-obj)
        prev-instal-mod1 (:mod1-applied (get install-list previous-index))
        interest_remaining_check (and recalc-list expand-sched (not (revert-install? recalc-list i)) (> (:interest_remaining expand-sched) 0.009))
        new-inst-obj (if interest_remaining_check
                       (assoc new-inst-obj :mod1-applied true)
                       new-inst-obj)
        field-val (condp = field
                    :num (+ i 1)
                    :r0 (:r0 sub-values)
                    :r
                    (if (= i 0)
                      (:r sub-values)
                      (condp = daycount-model
                        :30-360
                        (let [int_days (:int_days new-inst-obj)
                              months (/ int_days 30.0)
                              int-rate (/ (:apr sub-values) 12.0)]
                          (* months int-rate))))
                    :payment_duedate0
                    (let [pay-duedate (if (= i 0)
                                        (:first-payment-date sub-values)
                                        (add-month (:payment_duedate0 (get install-list previous-index))))]
                      pay-duedate)
                    :payment_duedate
                    (get-nearest-business-day (:payment_duedate0 new-inst-obj))
                    :int_days
                    (let [days-diff-fn (condp = daycount-model :30-360 days360 :actual-365 days-diff)]
                      (if (= i 0)
                        (days-diff-fn (:disbursement-date sub-values) (:first-payment-date sub-values))
                        (days-diff-fn (:payment_duedate (get install-list previous-index)) (:payment_duedate new-inst-obj))))
                    :interest_expected
                    (if (= i 0)
                      (let [r0 (:r0 sub-values)]
                        (cas/expr-multiply previous-principle_remaining  r0))
                      (let [r (bigdec (:r new-inst-obj))]
                        ;; #bookmark= 7f8f53c0-ea5e-4dc3-ad22-d29aebf2669c
                        ;; Previously was:
                        ;;(cas/expr-multiply previous-principle_remaining  :r)
                        (cas/expr-multiply previous-principle_remaining  r)))
                    :interest_expected_capped
                    (holiday-interest-cap install-list expand-sched i new-inst-obj interest_expected)
                    :principal_expected
                    (if install-previous-list
                      ;; update pass
                      (if interest_remaining_check
                        (cas/expr (cas/term 0 []))
                        (let [prin-exp1 (if (revert-install? recalc-list i)
                                          ;; If i is on recalc-list then need to clear some remaining interest_remaining
                                          (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1) (cas/expr-multiply interest_remaining -1))
                                          (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1)))
                              prin-exp2 (if prev-instal-mod1
                                          (cas/expr-sub (cas/expr prin-exp1 (cas/expr-multiply previous-interest_remaining -1)) sub-values)
                                          prin-exp1)]
                          (or prin-holiday prin-exp2)))
                      ;; 1st pass
                      (or prin-holiday
                          (cas/expr (cas/term 1 [:E]) (cas/expr-multiply interest_expected -1))))
                    :principle_remaining
                    (if install-previous-list
                      ;; update pass
                      (if interest_remaining_check
                        (or previous-principle_remaining (cas/expr (cas/term (:P sub-values) [])))
                        (let [prin-remain1 (if prev-instal-mod1
                                             (cas/expr previous-principle_remaining interest_expected_capped previous-interest_remaining (cas/term -1 [:E]))
                                             (cas/expr previous-principle_remaining interest_expected_capped (cas/term -1 [:E])))]
                          (if prin-holiday
                            previous-principle_remaining
                            prin-remain1)))
                      ;; 1st pass
                      (if prin-holiday
                        previous-principle_remaining
                        (cas/expr previous-principle_remaining (cas/expr-multiply previous-principle_remaining :r) (cas/term -1 [:E]))))
                    :interest_remaining0
                    (if (= i 0)
                      (cas/expr interest_expected (cas/expr-multiply total_payment_due -1))
                      (cas/expr previous-interest_remaining interest_expected (cas/expr-multiply total_payment_due -1)))
                    :interest_remaining
                    ;; determine whether to zero the interest_remaining balance or not
                    (if @INT_REMAIN-ZERO-TOGGLE
                      ;; ** feature is enabled
                      (if (and install-previous-list (not interest_remaining_check))
                          ;; We only consider zeroing after the initial-pass (i.e. when install-previous-list is non-nil)
                          ;; We should zero in all cases apart from when interest_expected<>interest_expected_capped.    
                        (if (= interest_expected interest_expected_capped)
                          (cas/expr (cas/term 0 []))
                          (cas/expr interest_expected (cas/expr-multiply interest_expected_capped -1)))
                        interest_remaining0)
                      ;; ** feature not enabled - use the old approach
                      interest_remaining0)
                    :total_remain
                    (if install-previous-list
                      ;; update pass
                      (if interest_remaining_check
                        (cas/expr principle_remaining  interest_remaining)
                        (cas/expr principle_remaining))
                      ;; 1st pass
                      principle_remaining)
                    :total_payment_due
                    (if prin-holiday
                      (cas/expr principal_expected interest_expected_capped)
                      (cas/expr (cas/term 1 [:E]))))
        field-val-expand (if (#{:num :r0 :r :payment_duedate0 :payment_duedate :int_days} field)
                           field-val
                           (cas/expr-sub field-val sub-values))]
    (assoc new-inst-obj field field-val-expand)))

(defn get-inst-obj
  ([i install-list install-previous-list sub-values]
   (get-inst-obj i install-list install-previous-list sub-values nil nil))
  ([i install-list install-previous-list sub-values expand-sched recalc-list]
   (-> {}
       (install-value i :num install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :payment_duedate0 install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :payment_duedate install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :int_days install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :r0 install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :r install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :interest_expected install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :interest_expected_capped install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :principal_expected install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :principle_remaining install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :total_payment_due install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :interest_remaining0 install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :interest_remaining install-list install-previous-list sub-values expand-sched recalc-list)
       (install-value i :total_remain install-list install-previous-list sub-values expand-sched recalc-list))))

;; --------------------------------------

(defn add-loan-instalment [sub-values]
  (fn [install-list i]
    (let [inst-obj (get-inst-obj i install-list nil sub-values)]
      (conj install-list inst-obj))))

(defn loan-schedule [numInstalments sub-values]
  (let [first-install (get-inst-obj 0 {} nil sub-values)]
    (reduce (add-loan-instalment sub-values) [first-install] (range 1 numInstalments))))

(defn expand-instalment [sub-values]
  (fn [instal-obj]
    (let [num (:num instal-obj)
          mod1-applied (:mod1-applied instal-obj)
          payment_duedate0 (:payment_duedate0 instal-obj)
          payment_duedate (:payment_duedate instal-obj)
          int_days (:int_days instal-obj)
          interest_expected (cas/expr-sub2 (:interest_expected instal-obj) sub-values)
          principal_expected (cas/expr-sub2 (:principal_expected instal-obj) sub-values)
          principle_remaining (cas/expr-sub2 (:principle_remaining instal-obj) sub-values)
          interest_remaining (cas/expr-sub2 (:interest_remaining instal-obj) sub-values)
          total_remain (cas/expr-sub2 (:total_remain instal-obj) sub-values)
          total_payment_due (cas/expr-sub2 (:total_payment_due instal-obj) sub-values)]
      {:mod1-applied mod1-applied :num num :payment_duedate0 payment_duedate0 :payment_duedate payment_duedate :int_days int_days :interest_expected interest_expected :principal_expected principal_expected :principle_remaining principle_remaining :interest_remaining interest_remaining :total_remain total_remain :total_payment_due total_payment_due})))

(defn update-instalment
  [old-loan-sched sub-values install-list expanded-instal-obj i recalc-list]
  (get-inst-obj i install-list old-loan-sched sub-values expanded-instal-obj recalc-list))

;; Returns a function to use in a reduce
(defn check-for-remain-int-greater-zero [old-loan-sched sub-values0 expand-sched recalc-list]
  (fn [install-list i]
    (let [instal-obj (update-instalment old-loan-sched sub-values0 install-list (get expand-sched i) i recalc-list)]
      (conj install-list instal-obj))))

(defn need-to-recalcuate [expand-sched]
  (let [expand-sched1 (enumerate expand-sched)
        recalc-needed (filter
                       (fn [[_ instal]] (and (not (:mod1-applied instal)) (> (:interest_remaining instal) 0.009)))
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
       [loan-sched2 (reduce (check-for-remain-int-greater-zero loan-sched sub-values0 expand-sched []) [] (range 0 numInstalments))
        dbgcnt (reset! DEBUG-COUNT (+ @DEBUG-COUNT 1))]
       (if (< dbgcnt 50)
         (recur loan-sched2 numInstalments sub-values0)
         (assert false "ABORT - looped too many times")))
          
      ;; Expr we need to solve to get E
      (let [loan-sched2 (reduce (check-for-remain-int-greater-zero loan-sched sub-values0 expand-sched []) [] (range 0 numInstalments))]
        (expand-schedule-final loan-sched2 numInstalments sub-values0)))))

;; #bookmark= 1031c4ec-f363-4294-8d2a-bd29b099f130
(defn expand-schedule
  ([OrigPrinciple interestRatePerInstalment numInstalments disbursement-date first-payment-date]
   (expand-schedule OrigPrinciple interestRatePerInstalment numInstalments disbursement-date first-payment-date :30-360))
  ([OrigPrinciple interestRatePerInstalment numInstalments disbursement-date first-payment-date daycount-model]
   (reset! DEBUG-COUNT 0) ;; mechanism to prevent looping forever
   (let [int-rate (/ interestRatePerInstalment 100)
         apr (* int-rate 12.0)
         r0 (get-r0-interest-rate daycount-model disbursement-date first-payment-date int-rate)
         sub-values0 {:P OrigPrinciple :r int-rate  :r0 r0 :apr apr :daycount-model daycount-model :disbursement-date disbursement-date :first-payment-date first-payment-date}
         loan-sched (loan-schedule numInstalments sub-values0)]
     (expand-schedule0 loan-sched numInstalments sub-values0))))


(comment ;; Testing sanbox area
  (ns-unalias *ns* 'cas)

  ;; Regression test suite:
  ;; http://localhost:3000/goto-file?&bookmark=7501ef93-3bb5-414e-9bc4-8726e8ac2611

  @LOAN-SCHEDULE-EDIT
  (clear-schedule-edits)
  (reset! INT_REMAIN-ZERO-TOGGLE true)

  (reset! HOLIDAY-INTEREST_CAP 30)
  (edit-sched-interest-only2 [1 3 5 7 9 11])
  (save-to-csv-file "test-ls4-1a.csv" (expand-schedule 10000 (/ 9.9M 12.0) 12 "2022-01-01" "2022-02-01"))

  (edit-sched-interest-only2 [1 2 3 4 5 6 7 8 9 10 11])
  (save-to-csv-file "test-ls4-1b.csv" (expand-schedule 10000 (/ 9.9M 12.0) 12 "2022-01-01" "2022-02-01"))

  (edit-sched-interest-only2 [1 2 3 4 5 6 7 8 9 10])
  (save-to-csv-file "test-ls4-1c.csv" (expand-schedule 10000 (/ 9.9M 12.0) 12 "2022-01-01" "2022-02-01"))

  (reset! INT_REMAIN-ZERO-TOGGLE false)
  (reset! HOLIDAY-INTEREST_CAP 30)
  (edit-sched-interest-only2 [1 2 3 4 5 6 7 8 9 10 30 31 32 59 60 61 74 75 76])
  (save-to-csv-file "test-ls4-2b2.csv" (expand-schedule 10000 (/ 9.9M 12.0) 84 "2022-01-01" "2023-01-01"))

  ;; This is the real principal-only holiday scenario. 
  ;; customer pays off interest-only but not any large interest-remainin balance
  (reset! HOLIDAY-INTEREST_CAP 0)
  (edit-sched-interest-only2 [1 2 3 4 5 6 7 8 9 10 30 31 32 59 60 61 74 75 76])
  (save-to-csv-file "test-ls4-2b2.csv" (expand-schedule 10000 (/ 9.9M 12.0) 84 "2022-01-01" "2023-01-01"))

  ;; This next one allows you to simulate principal+interest holiday
  (reset! HOLIDAY-INTEREST_CAP 0.0000001)
  (edit-sched-interest-only2 [1 2 3 4 5 6 7 8 9 10 30 31 32 59 60 61 74 75 76])
  (save-to-csv-file "test-ls4-2b2.csv" (expand-schedule 10000 (/ 9.9M 12.0) 84 "2022-01-01" "2023-01-01"))

(edit-schedule [[1 {:pricipal-to-pay 500 :interest-to-pay 0}] ])
(save-to-csv-file "test-ls4-2b2.csv" (expand-schedule 10000 (/ 9.9M 12.0) 84 "2022-01-01" "2023-01-01"))

(reset! HOLIDAY-INTEREST_CAP 0)
(edit-schedule [[-1 {:pricipal-to-pay 0 :interest-to-pay 0}]
                [-10 {:pricipal-to-pay 0 :interest-to-pay 0}]])
(save-to-csv-file "test-ls4-2b2b.csv" (expand-schedule 10000 (/ 9.9M 12.0) 84 "2022-01-01" "2022-02-01"))

;; test  business-day features
(clear-schedule-edits)
(clear-non-business-days)
(set-non-business-days ["2022-03-01" "2022-03-02" "2022-03-03" "2022-03-04"])
(save-to-csv-file "test-ls4-030422-1.csv" (expand-schedule 10000 (/ 9.9M 12.0) 12 "2022-01-01" "2022-02-01"))


  ;;
  )
  






