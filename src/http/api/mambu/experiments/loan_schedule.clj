;;; https://github.com/mkersh/ClojureTests/blob/master/src/http/api/mambu/experiments/loan_schedule.clj
(ns http.api.mambu.experiments.loan_schedule
(:require [http.api.json_helper :as api]
          [http.api.api_pipe :as steps]
          [java-time :as t])
)

;; Some atoms that will hold IDs/data used in multiple places
(defonce TIMEZONE (atom "Europe/Berlin"))
(defonce PRODUCT_ID (atom nil))
(defonce PRODUCT_KEY (atom nil))
(defonce CUSTID (atom nil))
(defonce CUSTKEY (atom nil))
(defonce LOANID (atom nil))
(defonce LOANAMOUNT (atom 5000))
(defonce INTEREST_RATE (atom 5))
(defonce NUM_INSTALS (atom 12))
(defonce GRACE_PERIOD (atom 0))
(defonce REAL_DISBURSE_DATE (atom nil))
(defonce FIRST_PAY_DATE (atom nil))
(defonce VALUE_DATE (atom "2021-01-01T13:37:50+01:00"))
(defonce FIRST_DATE (atom "2021-02-01T13:37:50+01:00"))

;; Adjust the timezone according to date passed
(defn adjust-timezone [dateStr]
  (let [date-local (t/zoned-date-time dateStr)
        date2 (t/with-zone date-local @TIMEZONE)
        zone-offset (str (t/zone-offset date2))
        date-minus-offset (subs dateStr 0 (- (count dateStr) 6))]
    (str date-minus-offset zone-offset)))

 (defn adjust-timezone2 [dateStr timezone]
  (let [date-local (t/zoned-date-time dateStr)
        date2 (t/with-zone date-local timezone)
        zone-offset (str (t/zone-offset date2))
        date-minus-offset (subs dateStr 0 (- (count dateStr) 6))]
    (str date-minus-offset zone-offset)))

(defn get-all-loans-api [context]
  {:url (str "{{*env*}}/loans")
   :method api/GET
   :query-params {"detailsLevel" "BASIC"
   "accountHolderType" "CLIENT"
   "accountHolderId" (:cust-key context)
   "accountState" (:status context)
   }

   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-schedule-api [context]
   {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/schedule")
    :method api/GET
    :query-params {"detailsLevel" "FULL"}
    :headers {"Accept" "application/vnd.mambu.v2+json"
              "Content-Type" "application/json"}})

(defn disburse-loan-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/disbursement-transactions")
   :method api/POST
   :body {
       "valueDate" (:value-date context) 
       "firstRepaymentDate" (:first-date context) 
       "notes" "Disbursement from clojure"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn days-diff [date1 date2]
  (let [date2-local (t/local-date "yyyy-MM-dd" (subs date2 0 10))
        date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (t/time-between :days date1-local date2-local)))

(defn months-diff [date1 date2]
  (let [date2-local (t/local-date "yyyy-MM-dd" (subs date2 0 10))
        date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (t/time-between :months date1-local date2-local)))

(defn round-to-2dp [num]
  (read-string (api/round-num num)))

(defn grace-period-interest-amount [disburse-amount start-date first-pay-date annual-interest-rate]
  (let [day-rate (/ (/ annual-interest-rate 100) 365)
        day-rate1 (float day-rate)
        num-days (days-diff start-date first-pay-date)]
    (round-to-2dp (* (float disburse-amount) day-rate1 num-days))))

(defn adjusted-disburse-amount [disburse-amount backdated-days annual-interest-rate]
  (let [day-rate (/ (/ annual-interest-rate 100) 365)
        day-rate1 (float day-rate)]
     (round-to-2dp (/ (float disburse-amount) (+ 1 (* backdated-days day-rate1))))))
  

(defn approveLoanAccount [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) ":changeState")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"action" "APPROVE"
          "notes" "Approved from the API"}})

(defn writeoffLoanAccount [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) ":writeOff")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {}})

(defn create-installment-loan-api [context]
   {:url (str "{{*env*}}/loans")
    :method api/POST
    :headers {"Accept" "application/vnd.mambu.v2+json"
              "Content-Type" "application/json"}
    :query-params {}
    :body  {"loanAmount" (:amount context)
            "loanName" (:acc-name context)
            "accountHolderKey" (:cust-key context)
            "productTypeKey" (:prod-key context)
            "accountHolderType" "CLIENT"
            "interestFromArrearsAccrued" 0.0
            "interestSettings" {"interestRate" (:interest-rate context)}
            "scheduleSettings" {"gracePeriod" (:grace_period context)
                                "repaymentInstallments" (:num-installments context)}}})

(defn get-loan-product [context]
  {:url (str "{{*env*}}/loanproducts/" (:product-id context))
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {"detailsLevel" "FULL"}
   })

(defn zap-a-loan []
  (try (steps/apply-api approveLoanAccount {:loanAccountId @LOANID}) (catch Exception _ nil))
  (try (steps/apply-api disburse-loan-api {:loanAccountId @LOANID}) (catch Exception _ nil))
  (try (steps/apply-api writeoffLoanAccount {:loanAccountId @LOANID}) (catch Exception _ nil)))

(defn zap-all-loans-aux [acc-list]
  (doall
   (map
    (fn [obj]
         (reset! LOANID (get obj "id"))
         (prn "Zapping: " @LOANID)
         (zap-a-loan))
    acc-list)))

(defn zap-all-loans []
  (let
   [active-list (api/extract-attrs ["id"] (:last-call (steps/apply-api get-all-loans-api {:cust-key @CUSTKEY :status "ACTIVE"})))
    active-in-arrears-list (api/extract-attrs ["id"] (:last-call (steps/apply-api get-all-loans-api {:cust-key @CUSTKEY :status "ACTIVE_IN_ARREARS"})))]
  (prn "Zapping ACTIVE loans")
  (zap-all-loans-aux active-list)
  (prn "Zapping ACTIVE_IN_ARREARS loans")
  (zap-all-loans-aux active-in-arrears-list)))

  (defn zap-all-loans2 [cust-key]
    (let
     [active-list (api/extract-attrs ["id"] (:last-call (steps/apply-api get-all-loans-api {:cust-key cust-key :status "ACTIVE"})))
      active-in-arrears-list (api/extract-attrs ["id"] (:last-call (steps/apply-api get-all-loans-api {:cust-key cust-key :status "ACTIVE_IN_ARREARS"})))]
      (prn "Zapping ACTIVE loans")
      (zap-all-loans-aux active-list)
      (prn "Zapping ACTIVE_IN_ARREARS loans")
      (zap-all-loans-aux active-in-arrears-list)))

(defn get-cust-api [context]
  {:url (str "{{*env*}}/clients/" (:cust-id context))
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {"detailsLevel" "FULL"}})

(defn make-context []
  {:cust-key @CUSTKEY
   :prod-key @PRODUCT_KEY})



(comment ;; Run functions in this section manually
;; Define the environment to use for testing
(api/setenv "env2") ;; https://markkershaw.mambu.com


;------------------------
;; [1] Get Customer details
;;

;; Code to find a customer's encodedKey
(reset! CUSTID "843556103")
(let  [res (steps/apply-api get-cust-api {:cust-id @CUSTID})
       encKey (get-in res [:last-call "encodedKey"])]
  (reset! CUSTKEY encKey)
  (api/PRINT res)
  (prn "encoded key = " encKey))

;;------------------------
;; [2] Get product details
;;

;; Code to find a product's encodedKey
;; PROVEQ1 - equal instalments 30/360 - prin-round-into-last
(reset! PRODUCT_ID "PROVEQ1")
;; PROVEQ1b - equal instalments 30/360 - no-rounding-principle
(reset! PRODUCT_ID "PROVEQ1b")
;; PROVEQ1c - equal instalments 30/360 - no-rounding-principle (v2)
(reset! PRODUCT_ID "PROVEQ1c")
;; PROVEQ1c - DecBal 30/360 - no-rounding-principle
(reset! PRODUCT_ID "PROVEQ1d")
;; PROVEQ1c - equal instalments 30/360 - compound interest
(reset! PRODUCT_ID "PROVEQ1e3")
;; PROVEQ1c - equal instalments 30/360 - capitalized interest
(reset! PRODUCT_ID "PROVEQ1e4")
;; PROVEQ2 - equal instalments actual/365 - prin-round-into-last
(reset! PRODUCT_ID "PROVEQ2")
;; PROVEQ2 - equal instalments actual/365 - no-rounding-principle
(reset! PRODUCT_ID "PROVEQ2b")
;; PROVEQ2 - equal instalments actual/365 - compound interest
(reset! PRODUCT_ID "PROVEQ2c")
;; PROVEQ - equal instalments actual/365 (optimised) - prin-round-into-last
(reset! PRODUCT_ID "PROVEQ3")
;; PROVEQ - equal instalments actual/365 (optimised) - no-rounding-principle
(reset! PRODUCT_ID "PROVEQ3b")
;;PROVEQ1 - equal instalments 30/360 - optimised
(reset! PRODUCT_ID "PROVEQ1f")

;; [2.1] Make sure you execute the following if you change the PRODUCT_ID
;;       This will set the corresponding PRODUCT_KEY
(let  [res (steps/apply-api get-loan-product {:product-id @PRODUCT_ID})
       encKey (get-in res [:last-call "encodedKey"])]
  ;;(api/PRINT res)
  (reset! PRODUCT_KEY encKey)
  (prn "encoded key = " encKey))

;; Run the following to see what the PRODUCT_ID is currently set to
@PRODUCT_ID

)

;; If you add a new PRODUCT_ID above, then give it a name below
;; This is the name that the accounts will be given when we create a new loan in step #3 below
(defn get-product-accname [prodid]
(condp = prodid
  "PROVEQ1" "Equi 30/360 - prin-round-into-last"
  "PROVEQ1f" "Equi 30/360 - optimised"
  "PROVEQ1b" "Equi 30/360 - no-rounding-principle"
  "PROVEQ1c" "Equi 30/360 - no-rounding-principle (v2)"
  "PROVEQ2" "Equi actual/365 - prin-round-into-last"
  "PROVEQ2b" "Equi actual/365 - no-rounding-principle"
  "PROVEQ2c" "Equi actual/365 - comp int"
  "PROVEQ3" "Equi actual/365 (optimised) - prin-round-into-last"
  "PROVEQ3b" "Equi actual/365 (optimised) - no-rounding-principle"
  "PROVEQ1d" "DecBal 30/360 - prin-round-into-last"
  "PROVEQ1e3" "Equi 30/360 - Compound Interest3"
  "PROVEQ1e4" "Equi 30/360 - Capitalized Interest"
  (throw (Exception. "Unknown @PRODUCT_ID - Update get-product-accname function"))))

(comment ;; Run functions in this section manually

;;------------------------
;; [3] Create a new loan
;;    Using PRODUCT_KEY set above
;;

;; Set the following if you want to change the loan amount (default 5K)
(reset! LOANAMOUNT 12550)
(reset! INTEREST_RATE 19.4)
(reset! VALUE_DATE (adjust-timezone "2020-06-18T13:37:50+00:00")) ;; Change these dates as required
(reset! REAL_DISBURSE_DATE (adjust-timezone "2020-07-08T13:37:50+00:00"))
(reset! FIRST_DATE (adjust-timezone "2020-07-18T13:37:50+00:00")) ;; Make sure the timezone offset is set correct!!! This will change throughout the year
(reset! FIRST_PAY_DATE (adjust-timezone "2020-10-18T13:37:50+00:00"))

(reset! NUM_INSTALS 81)
(reset! GRACE_PERIOD (months-diff @FIRST_DATE @FIRST_PAY_DATE)) ;; Number of grace periods. 
;; Use next function to adjust the loan amount - for equal instalment workaround
(let [adjust-for-backdated-days (adjusted-disburse-amount @LOANAMOUNT (days-diff @VALUE_DATE @REAL_DISBURSE_DATE) @INTEREST_RATE)
      grace-period-interest (grace-period-interest-amount @LOANAMOUNT @FIRST_DATE @FIRST_PAY_DATE @INTEREST_RATE) ;; Was using 202.89 for grace-period-capitalization
      cap-grace-period-total (+ adjust-for-backdated-days grace-period-interest)]
  (reset! LOANAMOUNT cap-grace-period-total))

@LOANAMOUNT

;; Run this let to create a new loan account
(let [res (steps/apply-api create-installment-loan-api
                           (merge (make-context) {:amount @LOANAMOUNT
                                                  :acc-name (get-product-accname @PRODUCT_ID)
                                                  :interest-rate @INTEREST_RATE
                                                  :grace_period @GRACE_PERIOD
                                                  :num-installments @NUM_INSTALS}))
      id (get-in res [:last-call "id"])]
  (reset! LOANID id)
  ;;(api/PRINT res)
  (prn "encoded key = " id)
  ;; Approve the Loan 
  (steps/apply-api approveLoanAccount {:loanAccountId @LOANID}))

;; [3.1] Disburse the Loan 
(steps/apply-api disburse-loan-api {:loanAccountId @LOANID :value-date @VALUE_DATE :first-date @FIRST_DATE})
@LOANID

;; [3.3] Zap the loan
(zap-a-loan) ;; uses @LOANID
(zap-all-loans) ;; Zap all active and in-arrears loans for @CUSTKEY
;; Set below first to change LOANID (if needed)
(reset! LOANID "LTMY547") ;; To link to an existing loan set this

;------------------------
;; [4] Get a loan details
;;
(api/PRINT (steps/apply-api get-loan-api {:loanAccountId @LOANID}))


;;
  )