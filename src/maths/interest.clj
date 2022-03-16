;; #bookmark= 8569537e-3b6f-4f08-9e66-dfd027f10515
(ns maths.interest
  (:require [clojure.math.numeric-tower :as math]))

;; exp function to raise a number by a power
;; different ways of doing this http://localhost:3000/goto-file?&bookmark=9de28b85-a797-4682-9403-1329daaf8cc3
;;
(defn pow [num power]
  (math/expt num power))

(defn comp-interest-rate-per-period [interest-rate-annually% period-fraction-per-year]
  (let [interest-rate-annual (/ interest-rate-annually% 100.0M)
        _1+IR% (+ 1 interest-rate-annual)
        int-rate-period1 (pow _1+IR% period-fraction-per-year)
        int-rate-period2 (- int-rate-period1 1)]
    int-rate-period2))

(defn simple-interest-rate-per-period [interest-rate-annually% period-fraction-per-year]
  (let [interest-rate-annual (/ interest-rate-annually% 100.0M)
        _ (prn "here1")
        period-fraction-per-year-decimal (double  period-fraction-per-year)
         _ (prn "here2")
        int-rate-period1 (* interest-rate-annual period-fraction-per-year-decimal)]
    int-rate-period1))

(defn amount-monthly-interest [principal interest-rate-annually% int-rate-fn]
  (let [monthly-int-rate (int-rate-fn interest-rate-annually% 1/12)
        month-interest-amount (* principal monthly-int-rate)]
    month-interest-amount))

(defn calc-comp-total-amount [p0 daily-interest-rate num-days]
  (loop [n num-days
         total-bal p0]
    (let [int-amount (* total-bal daily-interest-rate)
          new-total-bal (+ total-bal int-amount)]
      (if  (= n 1)
        new-total-bal
        (recur (- n 1) new-total-bal)))))

(defn calc-simp-total-amount [p0 daily-interest-rate num-days]
  (loop [n num-days
         prin-bal p0
         int-bal 0
         ]
    (let [int-amount (* prin-bal daily-interest-rate)
          new-int-bal (+ int-bal int-amount)]
      (if  (= n 1)
        (+ prin-bal new-int-bal)
        (recur (- n 1) prin-bal new-int-bal)))))

;; Single equation/formulae for calculating the compound interest
(defn calc-comp-total-amount2
  ([p0 daily-interest-rate num-days]
   (calc-comp-total-amount2 p0 daily-interest-rate num-days 1))
  ([p0 daily-interest-rate num-days t]
   (let [_1plusr-overn (+ 1 (/ daily-interest-rate num-days))
         exp1 (pow _1plusr-overn (* num-days t))
         res (* p0 exp1)]
     res)))

;; #bookmark= a07afdb9-608d-4f57-af44-81abcb558da6
;; P(t)=P_{0}e^{rt}
(defn continuous-comp-amount [p0 annual-interesst-rate% t]
  (let [interest-rate-annual (/ annual-interesst-rate% 100.0M)
        e-pow-rt  (pow (Math/E) (* interest-rate-annual t))
        res (* p0 e-pow-rt)]
    res))

(comment

;; ways to calculate the interest being acrued

;; accrue interest based on current total-balance (principal + accrued-interest)
(calc-comp-total-amount 1000 0.0002777777777777778 30)
;; accrue interest using simple-interest formulae. Interest only calculated on current principal
(calc-simp-total-amount 1000 0.000277 30)

;; These next methods use the A = (1+r/n)^(nt) formulae for calculating compound interest
;; Ref: https://www.thecalculatorsite.com/articles/finance/compound-interest-formula.php 
;; NOTE: Should deliver same results as calc-comp-total-amount function

;; compounding using the monthly rate
(calc-comp-total-amount2 1000 0.00833333 30)
;; compunding using the daily rate
(calc-comp-total-amount2 1000 0.0002777777777777778 1 30)
;; calculating on the compound monthly rate is way off
(calc-comp-total-amount2 1000 0.007974140428903764 30)


;; Calculate compounded interest for 1 year
(calc-comp-total-amount 1000 0.0002777777777777778 360)
(calc-comp-total-amount2 1000 0.00833333 30 12)
(calc-simp-total-amount 1000 0.000277 360)

;; Test if compound interest and simple interest equate to the same amounts
;; They do over a 1 year period (which is what I expected)
(calc-comp-total-amount 1000 0.0002647855489630313 360)
(calc-simp-total-amount 1000 0.0002777777777777778 360)

;; Compare against when compounding continuously for 1 year
(continuous-comp-amount 1000 10.0M 1)
;; continuous compunding for 1 month
(continuous-comp-amount 1000 10.0M (double 1/12))

;; tests to calculate the daily and monthlt interest-rate from the annual
(/ 0.1 360)
(/ 0.1 12)

;; Using Compound interest results in the customer paying less interest
;; NOTE: Not what you normally think of when you talk about compounded interest
(amount-monthly-interest 1000 10 comp-interest-rate-per-period)
(amount-monthly-interest 1000 10 simple-interest-rate-per-period)


(pow 2.7 4)
(comp-interest-rate-per-period 10 1) ;; Annual Interest Rate
(simple-interest-rate-per-period 10 1)
(comp-interest-rate-per-period 10 (/ 1 12)) ;; Monthly
(simple-interest-rate-per-period 10 (/ 1 12))
(comp-interest-rate-per-period 10 (/ 1 360)) ;; Daily using a 30/360 day-count-model
(simple-interest-rate-per-period 10 (/ 1 360))
(bigdec  (/ 1 12))
(bigdec (with-precision 10 1/12))
(with-precision 10 1/12)
(double (/ 1 12))
;;
)