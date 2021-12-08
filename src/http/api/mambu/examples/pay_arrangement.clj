(ns http.api.mambu.examples.pay-arrangement
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.experiments.loan_schedule :as ext]
            [clojure.string :as str]
            [java-time :as t]))

;; Some atoms that will hold IDs/data used in multiple places
(defonce PRODUCT_ID (atom nil))
(defonce LOAN_PRODUCT_KEY (atom nil))
(defonce PA_PRODUCT_KEY (atom nil))
(defonce CUSTID (atom nil))
(defonce CUSTKEY (atom nil))
(defonce LOANID (atom nil))
(defonce LOANAMOUNT (atom 5000))
(defonce INTEREST_RATE (atom 5))
(defonce NUM_INSTALS (atom 12))
(defonce GRACE_PERIOD (atom 0))
(defonce VALUE_DATE (atom nil))
(defonce LOAN-FIRST_DATE (atom nil))
(defonce PA-FIRST_DATE (atom nil))




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
            "scheduleSettings" {"periodicPayment" (:periodic-payment context)
                                "gracePeriod" (:grace_period context)
                                "repaymentInstallments" (:num-installments context)}}})

;; https://api.mambu.com/#loan-transactions-makerepayment
(defn repayment-api [context0]
  (let [defaults {:recalc-method nil}
        context (merge defaults context0)]
    {:url (str "{{*env*}}/loans/" (:accid context) "/repayment-transactions")
     :method api/POST
     :body
     {"amount" (:amount context)}
     :headers {"Accept" "application/vnd.mambu.v2+json"
               "Content-Type" "application/json"}}))

(defn get-account [context]
  {:url (str "{{*env*}}/loans/" (:accid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(reset! LOAN_PRODUCT_KEY "8a19cf397d98c717017d99999e660768") ;; MK-loan-account (cap. int)
(reset! PA_PRODUCT_KEY "8a19cf397d98c717017d994ce29102ce") ;; MK-payment-arrangement2
(reset! CUSTKEY "8a19cf397d98c717017d9af99735271f") ;; MK Tester2
(reset! LOANAMOUNT 50000)
(reset! INTEREST_RATE 9.00)
(reset! GRACE_PERIOD 0)
(reset! NUM_INSTALS 20)

(defn set-dates [year]
  (reset! VALUE_DATE (ext/adjust-timezone2 (str year "-11-18T13:37:50+00:00") "Europe/London")) ;; Change these dates as required
  (reset! LOAN-FIRST_DATE (ext/adjust-timezone2 (str year "-12-15T13:37:50+00:00") "Europe/London"))
  (reset! PA-FIRST_DATE (ext/adjust-timezone2 (str (+ year 1) "-01-01T13:37:50+00:00") "Europe/London")))

(set-dates 2021) ;; default year

(defn create-loan-account [accname]
  (let [res (steps/apply-api create-installment-loan-api
                             {:cust-key @CUSTKEY
                              :prod-key @LOAN_PRODUCT_KEY
                              :amount @LOANAMOUNT
                              :periodic-payment nil
                              :acc-name accname
                              :interest-rate @INTEREST_RATE
                              :grace_period @GRACE_PERIOD
                              :num-installments @NUM_INSTALS})
        id (get-in res [:last-call "id"])]
    (reset! LOANID id)
    ))

(defn create-pa-account [accname]
  (let [res (steps/apply-api create-installment-loan-api
                             {:cust-key @CUSTKEY
                              :prod-key @PA_PRODUCT_KEY
                              :amount 1000000.00 ;; Amount doesn't matter on this PA
                              :periodic-payment 1053.83
                              :acc-name accname
                              :interest-rate 0
                              :grace_period @GRACE_PERIOD
                              :num-installments (* @NUM_INSTALS 3)})
        id (get-in res [:last-call "id"])]
    (reset! LOANID id)
    
    ))

(defn patch-account-aliases-api [context]
  {:url (str "{{*env*}}/loans/" (:accountId context))
   :method api/PATCH
   :body [;; Patching the AccountAlias DataFieldSet
          {"op" "add" ;; "replace"
           "path" "_PaymentArrangement"
           "value" (:value context)}]
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn create-loan-and-pa [acc-name]
  (let [pa-accid (create-pa-account (str acc-name "-PA"))
        loan-accid (create-loan-account acc-name)
        link-details {"LinkedLoanAccount" loan-accid}]
    (steps/apply-api patch-account-aliases-api {:accountId pa-accid :value link-details})
    (steps/apply-api ext/approveLoanAccount {:loanAccountId pa-accid})
    (steps/apply-api ext/approveLoanAccount {:loanAccountId loan-accid})
    (steps/apply-api ext/disburse-loan-api {:loanAccountId pa-accid :value-date @VALUE_DATE :first-date @PA-FIRST_DATE})
    (steps/apply-api ext/disburse-loan-api {:loanAccountId loan-accid :value-date @VALUE_DATE :first-date @LOAN-FIRST_DATE})))


(comment
(api/setenv "env11") ;; AL sandbox env
(def accid "GYDN163")

;; create a new set of linked loan + pa accounts
(set-dates 2021) ;; Call to set the dates that will be used - This was the example dates given
(set-dates 2016) ;; This will allow us to test
(create-loan-and-pa "EXAM5")

;; next function will remove all active accounts
(ext/zap-all-loans2 @CUSTKEY)


;;; Low level test function call
(steps/apply-api ext/disburse-loan-api {:loanAccountId "LMPV876" :value-date @VALUE_DATE :first-date @LOAN-FIRST_DATE})
(create-pa-account (str "EEE" "-PA"))
(steps/apply-api patch-account-aliases-api {:accountId "WTTF324" :value {"LinkedLoanAccount" "VZFT418"}})


;; Careful with the next one
(ext/zap-all-loans2 @CUSTKEY)

(let [accid "FNND040" ;; main = YLKK457 pay-arrange = FNND040
      amount 999.99]
  (api/PRINT (:last-call (steps/apply-api repayment-api {:accid accid :amount amount}))))

(api/PRINT (:last-call (steps/apply-api get-account {:accid accid})))


)