(ns maths.loan-schedule-tests
  (:use clojure.test)
  (:require [maths.loan-schedule3 :as ls3]
            [maths.loan-schedule4 :as ls4]
            ))

(defn compare-schedules [expected-map actual-map]
  (let [expected-res (:instalments expected-map)
        actual-res (:instalments actual-map) ]
    (dorun (map (fn [e a]
                  (testing (str "Instalment " (:num e))
                    (is (= e a))))
                expected-res actual-res))))

(deftest loan-sch-tests
  (testing "real2-schedule1b.csv"
    (let [expected-res {:equal-month-amount {:E 1507.796459M}, :instalments [{:mod1-applied true, :num 1, :interest_expected 2400.00M, :principal_expected 0M, :principle_remaining 5000M, :interest_remaining 892.203541M, :total_remain 5892.203541M, :total_payment_due 1507.796459M} {:mod1-applied nil, :num 2, :interest_expected 50.00M, :principal_expected 565.592918M, :principle_remaining 4434.407082M, :interest_remaining -565.592918M, :total_remain 4434.407082M, :total_payment_due 1507.796459M} {:mod1-applied nil, :num 3, :interest_expected 44.34407082M, :principal_expected 1463.45238818M, :principle_remaining 2970.95469382M, :interest_remaining -2029.04530618M, :total_remain 2970.95469382M, :total_payment_due 1507.796459M} {:mod1-applied nil, :num 4, :interest_expected 29.7095469382M, :principal_expected 1478.0869120618M, :principle_remaining 1492.8677817582M, :interest_remaining -3507.1322182418M, :total_remain 1492.8677817582M, :total_payment_due 1507.796459M} {:mod1-applied nil, :num 5, :interest_expected 14.928677817582M, :principal_expected 1492.867781182418M, :principle_remaining 5.75782E-7M, :interest_remaining -4999.999999424218M, :total_remain 5.75782E-7M, :total_payment_due 1507.796459M}]}]
      (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 5000 1 5 "2021-01-01" "2025-01-01")))
      (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 5000 1 5 "2021-01-01" "2025-01-01"))))
                       ))


(comment


(run-all-tests #"here")

;;
)