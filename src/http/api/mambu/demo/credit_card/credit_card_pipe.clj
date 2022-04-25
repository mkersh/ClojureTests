;;; https://github.com/mkersh/ClojureTests/tree/master/src/http/api/mambu/demo/credit_card/credit_card_pipe.clj
(ns http.api.mambu.demo.credit_card.credit_card_pipe
  (:require [http.api.json_helper :as api]
            [http.api.mambu.demo.credit_card.zap_cust :as ext]
            [http.api.api_pipe :as steps]))


(def create-customer
  (fn [context]
    {:url (str "{{*env*}}/clients")
     :method api/POST
     :headers {"Accept" "application/vnd.mambu.v2+json"
               "Content-Type" "application/json"}
     :query-params {}
     :body {"firstName" (:first-name context)
            "lastName" (:last-name context)
            "preferredLanguage" "ENGLISH"
            "addresses" [{"country" "UK"
                          "city" "Liverpool"}]
            "notes" "Some Notes on this person"
            "gender" "MALE"
            "assignedBranchKey" (:branchid context)}
     }))

(defn create-credit-arrangement [context]
  {:url (str "{{*env*}}/creditarrangements")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body  {"amount" (:card-limit context)
           "availableCreditAmount" (:card-limit context)
           "expireDate" "2030-08-23T00:00:00+02:00"
           "exposureLimitType" "OUTSTANDING_AMOUNT"
           "holderKey" (:cust-key context)
           "holderType" "CLIENT"
           "notes" ""
           "startDate" "2019-08-23T00:00:00+02:00"
           "state" "APPROVED"
           "_URepayOptions" {"AutoRepayMethod" "Direct-Debit"
                             "PaymentDueDay" (:payment-day context)
                             "ShortMonthOption" "late"}}})

(defn createRCABucket [context]
  {:url (str "{{*env*}}/loans")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body  {"loanAmount" (:card-limit context)
           "loanName" (:acc-name context)
           "accountHolderKey" (:cust-key context)
           "productTypeKey" (:prod-key context)
           "accountHolderType" "CLIENT"
           "assignedBranchKey" (:branchid context)
           "interestFromArrearsAccrued" 0.0
           "interestSettings" {"accrueInterestAfterMaturity" false
                               "interestApplicationMethod" "REPAYMENT_DUE_DATE"
                               "interestBalanceCalculationMethod" "ONLY_PRINCIPAL"
                               "interestCalculationMethod" "DECLINING_BALANCE"
                               "interestChargeFrequency" "ANNUALIZED"
                               "interestRateReviewCount" 31
                               "interestRateReviewUnit" "DAYS"
                               "interestRateSource" "INDEX_INTEREST_RATE"
                               "interestSpread" (:interestspread context)
                               "interestType" "SIMPLE_INTEREST"}
           "scheduleSettings" {"fixedDaysOfMonth" [(:payment-day context)]
                               "gracePeriod" 0
                               "gracePeriodType" "NONE"
                               "paymentPlan" []
                               "periodicPayment" 0.0
                               "principalRepaymentInterval" 1
                               "repaymentPeriodUnit" "DAYS"
                               "repaymentScheduleMethod" "DYNAMIC"
                               "scheduleDueDatesMethod" "FIXED_DAYS_OF_MONTH"
                               "shortMonthHandlingMethod" "LAST_DAY_IN_MONTH"}}})

(defn createTEMPCardDDAAccount [context]
  {:url (str "{{*env*}}/deposits")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"overdraftInterestSettings" {"interestRateSettings" {"encodedKey" "8a818f9c6cd48156016cd6ef16ec3c25"
                                                               "interestChargeFrequency" "ANNUALIZED"
                                                               "interestChargeFrequencyCount" 1
                                                               "interestRate" 0.0
                                                               "interestRateSource" "FIXED_INTEREST_RATE"
                                                               "interestRateTerms" "FIXED"
                                                               "interestRateTiers" []}}
          "overdraftSettings" {"allowOverdraft" true
                               "overdraftExpiryDate" "2030-05-02T00:00:00+02:00"
                               "overdraftLimit" (:card-limit context)}

          "accountType" "CURRENT_ACCOUNT"
          "name" "TEMP Card Account"
          "accountHolderKey" (:cust-key context)
          "productTypeKey" (:tempdda-product context)
          "currencyCode" "EUR"
          "accountHolderType" "CLIENT"}})

(defn createPaymentSettlementAccount [context]
  {:url (str "{{*env*}}/deposits")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"accountType" "REGULAR_SAVINGS"
          "name" "CC Pay/Settle Account"
          "currencyCode" "EUR"
          "accountHolderKey" (:cust-key context)
          "productTypeKey" (:ccpay-product context)
          "accountHolderType" "CLIENT"}})

(defn addDepositToCreditLine [context]
  {:url (str "{{*env*}}/linesofcredit/" (:ca-id context) "/savings/" (:accid context) )
   :method api/POST
   :headers {"Content-Type" "application/json"}})

(defn addLoanToCreditLine [context]
  {:url (str "{{*env*}}/linesofcredit/" (:ca-id context) "/loans/" (:accid context))
   :method api/POST
   :headers {"Content-Type" "application/json"}})

(defn approveDepositAccount [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) ":changeState")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"action" "APPROVE"
          "notes" "Approved from the API"}})

(defn approveLoanAccount [context]
  {:url (str "{{*env*}}/loans/" (:accid context) ":changeState")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"action" "APPROVE"
          "notes" "Approved from the API"}})

(def delete-customer
  (fn [context]
    {:url (str "{{*env*}}/clients/" (:custid context))
     :method api/DELETE
     :headers {"Accept" "application/vnd.mambu.v2+json"
               "Content-Type" "application/json"}
     }))

;; A collection of steps     
(defn create-cc-collection [first-name last-name]
  {:context {:show-only false ; when true only prints steps, doesn't execute
             :first-name first-name, :last-name last-name 
             :branchid "8a818f5f6cbe6621016cbf217c9e5060"
             :card-limit 1000.00
             :payment-day 3
             :verbose true
             ;; **** Debug Only settings next - allows you to jump to individual steps
             :cust-key "8a818ec676334eb101763471a68b3b1b"
             :ca-id "LPJ539"
             :tempdda-id "OXVX863"
             :rcashop-id "GSOT125"
             :rcacash-id "RUFK377"
             :payacc-id "FRGO251"}
   :Xjump-to-step [:step2 :one-only] ; set ":id :jump-here" in a step 
   :steps [;; [STEP-1] Create Customer
           {:label "Create Customer"
            :request create-customer
            :post-filter [;(steps/save-last-to-context :cust-create)
                          (steps/save-part-to-context ["encodedKey"] :cust-key)
                          (steps/save-part-to-context ["id"] :custid)]}
           ;; [STEP-2] Create CA
           {:label "Create credit-arrangement"
            :request create-credit-arrangement
            :post-filter [(steps/save-part-to-context ["id"] :ca-id)
                          (steps/save-value-to-context  false :ignore-rest)]}
           ;; [STEP-2b] Create Payment/Settlement Account
           ;; This is automatically linked to RCA bucket accounts
           {:label "Create payment-settlement account"
            :pre-filter (steps/save-value-to-context "8a818e3b763684f8017637bf5c5c0fb5" :ccpay-product)
            :request createPaymentSettlementAccount
            :post-filter [(steps/save-part-to-context ["id"] :payacc-id)]}
           ;; [STEP-3] Create RCA-CASH
           {:label "Create RCA-CASH account"
            :pre-filter [(steps/save-value-to-context "8a818f5f6cbe6621016cbf3cf8675424" :prod-key)
                         (steps/save-value-to-context 5 :interestspread)
                         (steps/save-value-to-context  "CC - Cash" :acc-name)]
            :request createRCABucket
            :post-filter [;(steps/print-context "**RCA-CASH**:")
                          (steps/save-part-to-context ["id"] :rcacash-id)]}
            ;; [STEP-4] Create RCA-Purchase
           {:label "Create RCA-Purchase account"
            :pre-filter [(steps/save-value-to-context "8a818f5f6cbe6621016cbf6d66075e54" :prod-key)
                         (steps/save-value-to-context 0.0 :interestspread)
                         (steps/save-value-to-context  "CC - Purchases" :acc-name)]
            :request createRCABucket
            :post-filter [(steps/save-part-to-context ["id"] :rcashop-id)]}

           ;; [STEP-5] Create TempDDA
           {:label "Create TempDDA account"
            :pre-filter [(steps/save-value-to-context "8a818f5f6cbe6621016cbf7310ff6064" :tempdda-product)
                         (steps/save-value-to-context  false :show-only)]
            :request createTEMPCardDDAAccount
            :post-filter [(steps/save-part-to-context ["id"] :tempdda-id)]}

            ;; [STEP-6] Add Accounts to Credit-Arrangement
           {:label "Add accounts to credit-arrangement"
            :pre-filter [(steps/save-context-value-to-context :tempdda-id :accid)]
            :request addDepositToCreditLine}
           {:pre-filter [(steps/save-context-value-to-context :rcacash-id :accid)]
            :request addLoanToCreditLine}
           {:pre-filter [(steps/save-context-value-to-context :rcashop-id :accid)]
            :request addLoanToCreditLine}

            ;; [STEP-7] Approve Accounts
           {:label "Approve Accounts"
            :pre-filter [(steps/save-context-value-to-context :payacc-id :accid)]
            :request approveDepositAccount}
           {:pre-filter [(steps/save-context-value-to-context :tempdda-id :accid)]
            :request approveDepositAccount}
           {:pre-filter [(steps/save-context-value-to-context :rcacash-id :accid)]
            :request approveLoanAccount}
           {:pre-filter [(steps/save-context-value-to-context :rcashop-id :accid)]
            :request approveLoanAccount}]}
           
           ;; [STEP-7] Activate the TempDDA
           ;; Need to deposit some cash and then withdraw
           )

(defonce CUSTKEY (atom nil))
(defonce CUSTID (atom nil))

(defn create-new-cc-customer [first-name last-name]
  (let  [res-obj (steps/process-collection (create-cc-collection first-name last-name))]
    (reset! CUSTKEY (:cust-key res-obj))
    (reset! CUSTID (:custid res-obj))
    (prn (str "Customer create with ID " @CUSTID))))

(comment

  ;; Create a new credit-card customer with initial set of accounts
  ;; #bookmark= 21ea216a-8446-4e6a-ae7d-9face4f7d8d1
  (api/setenv "env2")
  (create-new-cc-customer "Apr24" "Tester1")
  
  ;; To see thee keys of the custoner just created
  @CUSTKEY
  @CUSTID


  ;; [0] Next function deletes/zaps a custoner and all related accounts
  (ext/zap-cust {:cust-key @CUSTKEY :custid @CUSTID})
  (reset! CUSTKEY "8a818fbc80510e2501805bc3ee9e4230")
  )