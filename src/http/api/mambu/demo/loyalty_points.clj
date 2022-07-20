;;; #bookmark= b331f4c7-75f0-4a23-9d4b-cfa49cdfe973
;;; GiHub: https://github.com/mkersh/ClojureTests/blob/master/src/http/api/mambu/demo/loyalty_points.clj
(ns http.api.mambu.demo.loyalty_points
  (:require [http.api.json_helper :as api]
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
            "assignedBranchKey" (:branchid context)}}))

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
                             "PaymentDueDay" "1"
                             "ShortMonthOption" "late"}}})

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

(defn createLoyaltyPointAccount [context]
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
                               "overdraftLimit" 0}
          "accountType" "CURRENT_ACCOUNT"
          "name" "LoyaltyPoint Account"
          "accountHolderKey" (:cust-key context)
          "productTypeKey" (:loyalty-product context)
          "currencyCode" "EUR"
          "accountHolderType" "CLIENT"}})


(defn addDepositToCreditLine [context]
  {:url (str "{{*env*}}/linesofcredit/" (:ca-id context) "/savings/" (:accid context))
   :method api/POST
   :headers {"Content-Type" "application/json"}})

(defn approveDepositAccount [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) ":changeState")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"action" "APPROVE"
          "notes" "Approved from the API"}})

;; A collection of steps     
(defn create-loyalty-customer [num]
  {:context {:show-only false ; when true only prints steps, doesn't execute
             :verbose true ;; print out step :label(s)
             :first-name "Loyalty", :last-name (str "Tester " num)
             :branchid "8a818f5f6cbe6621016cbf217c9e5060"
             :card-limit 1000.00
             :payment-day 3
             ;; **** Debug Only settings next - allows you to jump to individual steps
             :cust-key "8a818ec676334eb101763471a68b3b1b"
             :ca-id "LPJ539"
             :tempdda-id "OXVX863"
             :rcashop-id "GSOT125"
             :rcacash-id "RUFK377"
             :payacc-id "FRGO251"}
   :Xjump-to-step [:step2 :one-only] ; set ":id :jump-here" in a step 
   :steps [;; [STEP-1] Create Customer
           {:label "[STEP-1] Create Customer"
            :request create-customer
            :post-filter [;(steps/save-last-to-context :cust-create)
                          (steps/save-part-to-context ["encodedKey"] :cust-key)
                          (steps/save-part-to-context ["id"] :custid)]}
           ;; [STEP-2] Create CA
           {:label "[STEP-2] Create CA"
            :request create-credit-arrangement
            :post-filter [(steps/save-part-to-context ["id"] :ca-id)
                          (steps/save-value-to-context  false :ignore-rest)]}
           ;; [STEP-5] Create TempDDA
           {:label "[STEP-5] Create TempDDA"
            :pre-filter [(steps/save-value-to-context "8a818f5f6cbe6621016cbf7310ff6064" :tempdda-product)
                         ;;(steps/save-value-to-context  false :show-only)
                         ]
            :request createTEMPCardDDAAccount
            :post-filter [(steps/save-part-to-context ["id"] :tempdda-id)]}

            ;; [STEP-5] Create LoyaltyPoint Account
           {:label "[STEP-5] Create LoyaltyPoint Account"
            :pre-filter [(steps/save-value-to-context "8a818f5f713625dd017144cb4df05106" :loyalty-product)]
            :request createLoyaltyPointAccount
            :post-filter [(steps/save-part-to-context ["id"] :loyaltyAcc-id)]}

            ;; [STEP-6] Add Accounts to Credit-Arrangement
           {:label "[STEP-6] Add Accounts to Credit-Arrangement"
            :pre-filter [(steps/save-context-value-to-context :tempdda-id :accid)]
            :request addDepositToCreditLine}

            ;; [STEP-7] Approve Accounts
           {:label "[STEP-7] Approve Accounts"
            :pre-filter [(steps/save-context-value-to-context :tempdda-id :accid)]
            :request approveDepositAccount}
            {:pre-filter [(steps/save-context-value-to-context :loyaltyAcc-id :accid)]
             :request approveDepositAccount}
            ]}
  )


(comment
  (api/setenv "env2")

  ;; [1] Create a new clean customer for the LoyaltyPoint demo
  ;;
  (steps/process-collection (create-loyalty-customer 8)) ;; The number passed is used for the name ""Loyalty Tester n""


  ;; *******************************************************************
  ;; Calls used whilst testing but you use [1] above to create a new customer and all the sub-accounts needed
  (steps/apply-api createLoyaltyPointAccount {:cust-key "8a818fdc7a80c056017a85257f69441c" :loyalty-product "8a818f5f713625dd017144cb4df05106"})

  ;;
  )