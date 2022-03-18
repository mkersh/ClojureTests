;;; #bookmark= 102cb3af-9613-4130-9c01-dfafe894cda2
(ns http.api.mambu.demo.multi-currency-daily-bank-account
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.customer_account :as cust]
            ))

(defonce CUSTID (atom nil))
(defonce CUSTKEY (atom nil))
(defonce BRANCH (atom "8a818f5f6cbe6621016cbf217c9e5060"))
(defonce PRODUCT-BASE-ACC (atom "8a818f5f6cbe6621016cbf7310ff6064"))

(def create-customer-api
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

(defn create-customer [context]
  (let [res-obj (:last-call (steps/apply-api create-customer-api context))
        custid (get res-obj "id")
        custkey (get res-obj "encodedKey")]
    (reset! CUSTID custid)
    (reset! CUSTKEY custkey)
    (api/PRINT res-obj)))


(defn create-current-account-api [context]
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
          "name" (:acc-name context)
          "accountHolderKey" (:cust-key context)
          "productTypeKey" (:product-key context)
          "currencyCode" "EUR"
          "accountHolderType" "CLIENT"}})

(defn approveDepositAccount-api [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) ":changeState")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"action" "APPROVE"
          "notes" "Approved from the API"}})

(comment
  (ns-unalias *ns* 'PRODUCT-BASE-ACC)
  (api/setenv "env2")

  (cust/close-customer @CUSTID)

  ;; 
  (create-customer {:first-name "MCA" :last-name "Tester33" :branchid @BRANCH})


  (api/PRINT (:last-call (steps/apply-api create-current-account-api {:cust-key @CUSTKEY :acc-name "Main Current Account" :product-key @PRODUCT-BASE-ACC })))
  @CUSTKEY
  @PRODUCT-BASE-ACC
  ;;
  )