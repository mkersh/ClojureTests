;; Regression tests for loan_schedulexx.clj
;; #bookmark= 7501ef93-3bb5-414e-9bc4-8726e8ac2611
(ns maths.loan-schedule-tests.loan-schedule-tests
  (:use clojure.test)
  (:require [maths.loan-schedule3 :as ls3]
            [maths.loan-schedule4 :as ls4]
            [clojure.pprint :as pp]))

(defn save-to-file
  [file-name s]
  (spit file-name s))

(defn get-object-str [object]
  (let [out (java.io.StringWriter.)]
    (pp/pprint object out)
    (.toString out)))

(defn save-object [object fpath]
  (let [object-str (get-object-str object)]
    ;;(io/make-parents file-path)
    (save-to-file fpath object-str)))

(defn read-object [fpath]
  (read-string (slurp fpath)))

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
      (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 5000 1 5 "2021-01-01" "2025-01-01")))))

  (testing "real2-schedule2b.csv"
    (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/real2-schedule2b.txt")]
      (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 100000 0.4 100 "2021-01-01" "2025-01-01")))
      (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 100000 0.4 100 "2021-01-01" "2025-01-01")))))

  (testing "testsch1.csv"
    (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch1.txt")]
      (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 10000 (/ 9.9M 12.0) 84 "2021-04-14" "2021-06-15")))
      (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 10000 (/ 9.9M 12.0) 84 "2021-04-14" "2021-06-15")))))

  (testing "testsch2.csv"
    (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch2.txt")]
      (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 12550 (/ 19.4M 12.0) 78 "2020-07-08" "2020-10-18")))
      (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 12550 (/ 19.4M 12.0) 78 "2020-07-08" "2020-10-18")))))

(testing "testsch3-v2.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch3-v2.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000 4.2350610718397075M 24 "2021-03-21" "2021-04-04")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000 4.2350610718397075M 24 "2021-03-21" "2021-04-04")))))

(testing "testsch3b.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch3b.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000 4.24M 24 "2021-03-21" "2021-04-19")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000 4.24M 24 "2021-03-21" "2021-04-19")))))

(testing "testsch3c.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch3c.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000 4.24M 48 "2021-03-21" "2021-05-04")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000 4.24M 48 "2021-03-21" "2021-05-04")))))

(testing "testsch4.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch4.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000 4.24M 24 "2021-08-01" "2021-09-14")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000 4.24M 24 "2021-08-01" "2021-09-14")))))

(testing "testsch5.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch5.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000 4.24M 36 "2021-08-01" "2021-09-14")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000 4.24M 36 "2021-08-01" "2021-09-14")))))

(testing "testsch6.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch6.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000 4.24M 48 "2021-08-01" "2021-09-14")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000 4.24M 48 "2021-08-01" "2021-09-14")))))

(testing "testsch7b.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch7b.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000000 5.00M 24 "2019-09-25" "2019-12-25")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000000 5.00M 24 "2019-09-25" "2019-12-25")))))


(testing "testsch7c.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch7c.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000000 0.50M 12 "2019-09-25" "2020-01-25")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000000 0.50M 12 "2019-09-25" "2020-01-25")))))

(testing "testsch7d.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/testsch7d.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000 4.24M 24 "2021-03-21" "2021-04-04")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000 4.24M 24 "2021-03-21" "2021-04-04")))))

(testing "test-balloon1.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/test-balloon1.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 10000 (/ 5.00 12.0) 20 "2022-01-26" "2023-01-26")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 10000 (/ 5.00 12.0) 20 "2022-01-26" "2023-01-26")))))

(testing "test-balloon1b.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/test-balloon1b.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 10000 (/ 5.00 12.0) 20 "2022-01-26" "2024-01-26")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 10000 (/ 5.00 12.0) 20 "2022-01-26" "2024-01-26")))))

(testing "comp-against-formula.csv"
  (let [expected-res (read-object "src/maths/loan_schedule_tests/expected_results/comp-against-formula.txt")]
    (testing "loan-schedule3" (compare-schedules expected-res (ls3/expand-schedule 1000000 (/ 8.50 12.0) (* 15 12) "2022-01-26" "2022-02-26")))
    (testing "loan-schedule4" (compare-schedules expected-res (ls4/expand-schedule 1000000 (/ 8.50 12.0) (* 15 12) "2022-01-26" "2022-02-26")))))

 ;;
  )


(comment

;; Save results into a file and then create a regression test to ensure that we do not break
(save-object (ls3/expand-schedule 1000000 (/ 8.50 12.0) (* 15 12) "2022-01-26" "2022-02-26")
             "src/maths/loan_schedule_tests/expected_results/comp-against-formula.txt")

;; Run all the tests in this namespace
(run-all-tests #"maths.loan_schedule_tests.loan_schedule_tests/other-test")

;;
)