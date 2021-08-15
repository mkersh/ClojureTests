(ns http.api.mambu.demo.credit_card.clj
  (:require [http.api.json_helper :as api]))

(defn DELETE [url]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {}}]
    (api/PRINT (api/DELETE url options))))

(defn GET [url]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {}}]
    (api/PRINT (api/GET url options))))


(defn create-customer [env]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body  {"firstName" (:first-name env)
                         "lastName" (:last-name env)
                         "preferredLanguage" "ENGLISH"
                         "addresses" [{"country" "UK"
                                       "city" "Liverpool"}]
                         "notes" "Some Notes on this person"
                         "gender" "MALE"
                         "assignedBranchKey" (:branchid env)}}]
    (api/POST "{{*env*}}/clients" options)))


(defn create-credit-arrangement [env]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body  {"amount" 10000.0
                         "availableCreditAmount" 10000.0
                         "expireDate" "2030-08-23T00:00:00+02:00"
                         "exposureLimitType" "OUTSTANDING_AMOUNT"
                         "holderKey" (:cust-key env)
                         "holderType" "CLIENT"
                         "notes" ""
                         "startDate" "2019-08-23T00:00:00+02:00"
                         "state" "APPROVED"
                         "_URepayOptions" {"AutoRepayMethod" "Direct-Debit"
                                           "PaymentDueDay" "1"
                                           "ShortMonthOption" "late"}}}]
    (api/POST "{{*env*}}/creditarrangements" options)))

(defn createLoanAccount [body]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body  body}]
    (api/POST "{{*env*}}/loans" options)))


(defn createRCABucket [env prodKey accName]
  (let [body {"loanAmount" 30000.0
              "loanName" accName
              "accountHolderKey" (:cust-key env)
              "productTypeKey" prodKey
              "accountHolderType" "CLIENT"
              "assignedBranchKey" "8a818f156ccf5fb1016cd2e8e4532b09"
              "interestFromArrearsAccrued" 0.0
              "interestSettings" {"accrueInterestAfterMaturity" false
                                  "interestApplicationMethod" "REPAYMENT_DUE_DATE"
                                  "interestBalanceCalculationMethod" "ONLY_PRINCIPAL"
                                  "interestCalculationMethod" "DECLINING_BALANCE"
                                  "interestChargeFrequency" "ANNUALIZED"
                                  "interestRateReviewCount" 31
                                  "interestRateReviewUnit" "DAYS"
                                  "interestRateSource" "INDEX_INTEREST_RATE"
                                  "interestSpread" 0.0
                                  "interestType" "SIMPLE_INTEREST"}
              "scheduleSettings" {"fixedDaysOfMonth" [1]
                                  "gracePeriod" 0
                                  "gracePeriodType" "NONE"
                                  "paymentPlan" []
                                  "periodicPayment" 0.0
                                  "principalRepaymentInterval" 1
                                  "repaymentPeriodUnit" "DAYS"
                                  "repaymentScheduleMethod" "DYNAMIC"
                                  "scheduleDueDatesMethod" "FIXED_DAYS_OF_MONTH"
                                  "shortMonthHandlingMethod" "LAST_DAY_IN_MONTH"}}]
    (createLoanAccount body)))

(defn createInstallmentLoan [env prodKey accName amount]
  (let [body {"loanAmount" amount
              "loanName" accName
              "accountHolderKey" (:cust-key env)
              "productTypeKey" prodKey
              "accountHolderType" "CLIENT"
              "assignedBranchKey" "8a818f156ccf5fb1016cd2e8e4532b09"
              "interestFromArrearsAccrued" 0.0
              "interestSettings" {"accrueInterestAfterMaturity" false
                                  "interestApplicationMethod" "REPAYMENT_DUE_DATE"
                                  "interestBalanceCalculationMethod" "ONLY_PRINCIPAL"
                                  "interestCalculationMethod" "DECLINING_BALANCE_DISCOUNTED"
                                  "interestChargeFrequency" "ANNUALIZED"
                                  "interestRateReviewCount" 31
                                  "interestRateReviewUnit" "DAYS"
                                  "interestRateSource" "INDEX_INTEREST_RATE"
                                  "interestSpread" 0.0
                                  "interestType" "SIMPLE_INTEREST"}
              "scheduleSettings" {"gracePeriod" 0
                                  "gracePeriodType" "NONE"
                                  "paymentPlan" []
                                  "periodicPayment" 0.0
                                  "principalRepaymentInterval" 1
                                  "repaymentInstallments" 12
                                  "repaymentPeriodCount" 1
                                  "repaymentPeriodUnit" "MONTHS"
                                  "repaymentScheduleMethod" "DYNAMIC"
                                  "scheduleDueDatesMethod" "INTERVAL"}}]
    (createLoanAccount body)))

(defn createDepositAccount [body]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body  body}]
    (api/POST "{{*env*}}/deposits" options)))

(defn createCurrentAccount [env]
  (let [body {"overdraftInterestSettings"
              {"interestRateSettings" {"encodedKey" "8a818f9f6cc14d6e016cc2fcdbef4923"
                                       "interestChargeFrequency" "ANNUALIZED"
                                       "interestChargeFrequencyCount" 1
                                       "interestRateReviewCount" 31
                                       "interestRateReviewUnit" "DAYS"
                                       "interestRateSource" "INDEX_INTEREST_RATE"
                                       "interestRateTerms" "FIXED"
                                       "interestSpread" 0.0}}
              "overdraftSettings" {"allowOverdraft" true
                                   "overdraftExpiryDate" "2020-05-02T00:00:00+02:00"
                                   "overdraftLimit" 0.0}
              "accountType" "CURRENT_ACCOUNT"
              "name" "Current Account (with overdraft)"
              "accountHolderKey" (:cust-key env)
              "productTypeKey" (:curr-acc-product env)
              "currencyCode" "EUR"
              "accountHolderType" "CLIENT"}]
    (createDepositAccount body)))

(defn createTEMPCardDDAAccount [env]
  (let [body {"overdraftInterestSettings" {"interestRateSettings" {"encodedKey" "8a818f9c6cd48156016cd6ef16ec3c25"
                                                                   "interestChargeFrequency" "ANNUALIZED"
                                                                   "interestChargeFrequencyCount" 1
                                                                   "interestRate" 0.0
                                                                   "interestRateSource" "FIXED_INTEREST_RATE"
                                                                   "interestRateTerms" "FIXED"
                                                                   "interestRateTiers" []}}
              "overdraftSettings" {"allowOverdraft" true
                                   "overdraftExpiryDate" "2020-05-02T00:00:00+02:00"
                                   "overdraftLimit" 0.0}

              "accountType" "CURRENT_ACCOUNT"
              "name" "TEMP Card Account"
              "accountHolderKey" (:cust-key env)
              "productTypeKey" (:tempdda-product env)
              "currencyCode" "EUR"
              "accountHolderType" "CLIENT"}]
    ;;(api/PRINT body)
    (createDepositAccount body)))

(defn createLotaltyPointsAccount [env]
  (let [body {"overdraftSettings" {"allowOverdraft" true
                                   "overdraftExpiryDate" "2020-05-02T00:00:00+02:00"
                                   "overdraftLimit" 0.0}
              "accountType" "CURRENT_ACCOUNT"
              "name" "Loyalty Cashback"
              "accountHolderKey" (:cust-key env)
              "productTypeKey" (:loyalty-product env)
              "currencyCode" "EUR"
              "accountHolderType" "CLIENT"}]
    ;;(api/PRINT body)
    (createDepositAccount body)))

(defn addAccountToCreditLine [caID accID]
(let [options {:headers {"Content-Type" "application/json"}
               :query-params {}}
      url (str "{{*env*}}/linesofcredit/" caID "/savings/" accID )]
  (api/POST url options))
)

(defn addLoanToCreditLine [caID loanID]
  (let [options {:headers {"Content-Type" "application/json"}
                 :query-params {}}
        url (str "{{*env*}}/linesofcredit/" caID "/loans/" loanID)]
    (api/POST url options)))

(defn approveDepositAccount [accid]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body {"action" "APPROVE"
                        "notes" "Approved from the API"}}
        url (str "{{*env*}}/deposits/" accid ":changeState")]
    (api/POST url options)))

(defn approveLoanAccount [accid]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body {"action" "APPROVE"
                        "notes" "Approved from the API"}}
        url (str "{{*env*}}/loans/" accid ":changeState")]
    (api/POST url options)))

(defn add-to-env [env res-list id new-keyword]
  ;;(prn env res-list id new-keyword)
  (assoc env new-keyword (get res-list id)))

(defn create-loyalty-card-customer [env]
  (let [env1 (add-to-env env (create-customer env) "encodedKey" :cust-key)
        env2 (add-to-env env1 (create-credit-arrangement env1) "id" :ca-id)
        env3 (add-to-env env2 (createCurrentAccount env2) "id" :curr-acc-id)
        env4 (add-to-env env3 (createTEMPCardDDAAccount env3) "id" :tempdda-acc-id)
        env5 (add-to-env env4 (createLotaltyPointsAccount env4) "id" :loyalty-acc-id)
        ]
    (prn "Add Curr Account to CA")
    (addAccountToCreditLine (:ca-id env5) (:curr-acc-id env5))
    (prn "Approve Curr Account")
    (approveDepositAccount (:curr-acc-id env5))
    (prn "Add Temp DDA Account to CA")
    (addAccountToCreditLine (:ca-id env5) (:tempdda-acc-id env5))
    (prn "Approve Temp DDA Account")
    (approveDepositAccount (:tempdda-acc-id env5))
    (prn "Approve Loyalty Account")
    (approveDepositAccount (:loyalty-acc-id env5))
    env5)
    )

(comment
  (api/setenv "env2")
  (def context
    {:cust-key "8a818f3976334f8e0176338ccc0b112b"
     :first-name "John"
     :last-name "Barry5"
     :branchid-test "8a818f156ccf5fb1016cd2e8e4532b09"
     :branchid "8a818f5f6cbe6621016cbf217c9e5060"
     :curr-acc-product "8a818f5f6cbe6621016cbf4f2db95756"
     :tempdda-product "8a818f5f6cbe6621016cbf7310ff6064"
     :loyalty-product "8a818f5f713625dd017144cb4df05106"
     :ca-id "KVI609"
     :curr-acc-id "AYPV442"
     :tempdda-acc-id "RARJ286"
     :loyalty-acc-id "UUTT451"})


  (create-customer context)
  (DELETE "{{*env*}}/clients/048875193")
  (GET "{{*env*}}/clients/019327031")
  (create-credit-arrangement context)
  (createCurrentAccount context)
  (createTEMPCardDDAAccount context)
  (createLotaltyPointsAccount context)
  (addAccountToCreditLine (:ca-id context) (:curr-acc-id context))
  (approveDepositAccount (:curr-acc-id context))
  (addAccountToCreditLine (:ca-id context) (:tempdda-acc-id context))
  (approveDepositAccount (:tempdda-acc-id context))
  (approveDepositAccount (:loyalty-acc-id context))

  ;; Create a new customer for the Lotalty Points Demo
  (create-loyalty-card-customer context)


  (createRCABucket context "8a818f5f6cbe6621016cbf3cf8675424" "CC - Cash") ;; cash bucket
  (createRCABucket context "8a818f5f6cbe6621016cbf6d66075e54" "CC - Purchases") ;; Purchases
  (addLoanToCreditLine "PKS526" "XIVK246")
  (approveLoanAccount "XIVK246")
  (addLoanToCreditLine "PKS526" "KKNQ549")
  (approveLoanAccount "KKNQ549")


  (createInstallmentLoan context "8a818f5f6cbe6621016cbf627bbc5af4" "Instalment Loan" 555)

  (addLoanToCreditLine "PKS526" "GUQP075")
  (approveLoanAccount "GUQP075")
  
  
  )
