(ns maths.interest
  (:require [clojure.math.numeric-tower :as math]))

;; exp function to raise a number by a power
;; different ways of doing this http://localhost:3000/goto-file?&bookmark=9de28b85-a797-4682-9403-1329daaf8cc3 - If using my GotoFile mechanism
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

(defn calc-comp-total-amount2 [p0 daily-interest-rate num-days]
(let [_1plusr-overn (+ 1 (/ daily-interest-rate num-days))
      exp1 (pow _1plusr-overn num-days)
      res (* p0 exp1)]
  res)

)

(comment

(calc-comp-total-amount 1000 0.000277 30)
(calc-simp-total-amount 1000 0.000277 30)
(calc-comp-total-amount2 1000 0.00797 30)

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