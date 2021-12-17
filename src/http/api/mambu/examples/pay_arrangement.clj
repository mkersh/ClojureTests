(ns http.api.mambu.examples.pay-arrangement
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.experiments.loan_schedule :as ext]
            [clojure.string :as str]
            [java-time :as t]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

;; Some atoms that will hold IDs/data used in multiple places
(defonce PRODUCT_ID (atom nil))
(defonce LOAN_PRODUCT_KEY (atom nil))
(defonce PA_PRODUCT_KEY (atom nil))
(defonce CUSTID (atom nil))
(defonce CUSTKEY (atom nil))
(defonce LOANID (atom nil))
(defonce LOANID2 (atom nil))
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
;;(reset! LOAN_PRODUCT_KEY "8a19d7f07d9e27da017d9ff0a47402cf") ;; MK-loan-account (cap. int, prin first) - does make any difference

;;(reset! LOAN_PRODUCT_KEY "8a19b6a07d94027d017d944b03ab3c34") ;; MK-loan-account
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

(set-dates 2016) ;; default year

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
         _ (reset! LOANID pa-accid)
         _ (reset! LOANID2 loan-accid)
        link-details {"LinkedLoanAccount" loan-accid}]
    (steps/apply-api patch-account-aliases-api {:accountId pa-accid :value link-details})
    (steps/apply-api ext/approveLoanAccount {:loanAccountId pa-accid})
    (steps/apply-api ext/approveLoanAccount {:loanAccountId loan-accid})
    (steps/apply-api ext/disburse-loan-api {:loanAccountId pa-accid :value-date @VALUE_DATE :first-date @PA-FIRST_DATE})
    (steps/apply-api ext/disburse-loan-api {:loanAccountId loan-accid :value-date @VALUE_DATE :first-date @LOAN-FIRST_DATE})))

(defn get-loan-schedule [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/schedule")
   :method api/GET
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; *******
;; [START]**********************************************
;; Functions to save transactions to a CSV file

(defonce CSV-ROOT (atom "CSV-FILES/"))
(defonce CSV-REC-NUM (atom 0))

(defn get-file-path [fn]
  (str @CSV-ROOT fn))

(defn round-num [num]
  (format "%.2f" num))

(defn dump-trans-to-csv [trans-list]
  
  (let [next-tran (first trans-list)
        rest-tran (rest trans-list)]
    (println
     (str
      @CSV-REC-NUM ","
      (subs (get next-tran "valueDate") 0 10) ","
      (round-num (get next-tran "amount")) ","
      (get next-tran "type") ","
      (get-in next-tran ["accountBalances" "totalBalance"])))

    (swap! CSV-REC-NUM inc)
    ;; recurse to next line
    (when (not-empty rest-tran) (dump-trans-to-csv rest-tran))))

(defn save-to-csv-file [fn trans-list]
  (let [fpath (get-file-path fn)]
    (reset! CSV-REC-NUM 1)
    (io/make-parents fpath)
    (spit fpath "" :append false)
    (with-open [out-data (io/writer fpath)]
      (binding [*out* out-data]
        (println "#, Date, Amount, Type, TotalBalance")
        (dump-trans-to-csv trans-list)))))

;; *******
;; [END]**********************************************

(defn get-loan-transactions [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/transactions")
   :method api/GET
   :query-params {"limit" 1000} ;; return all in 1 go
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn save-transactions-to-file [accid fn]
  (let [trans-list0 (:last-call (steps/apply-api get-loan-transactions {:accid accid}))
        trans-list (into [] (reverse trans-list0))]
    (save-to-csv-file fn trans-list)))

(defn repayment-loan-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/repayment-transactions")
   :method api/POST
   :body {"amount" (:amount context)
          ;;"externalId" (:externalId context)
          ;;"installmentEncodedKey" (:installmentEncodedKey context)
          "valueDate" (:value-date context)
          "notes" "#NO-MIRROR"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn repay-instalment [pa-accid inst-num]
  (let [inst-list (get-in (steps/apply-api get-loan-schedule {:accid pa-accid}) [:last-call "installments"])
        _ (assert (and (> inst-num 0) (> (count inst-list) inst-num)) "ERROR: repay-instalment")
        inst-obj (get inst-list (- inst-num 1))
        amount (get-in inst-obj ["principal" "amount" "expected"]) ;; could have used "due" rather than expected
        due-date (get inst-obj "dueDate")
        pa-acc (steps/apply-api get-account {:accid pa-accid})
        loan-accid (get-in pa-acc [:last-call "_PaymentArrangement" "LinkedLoanAccount"])]
    (prn "Repay PA" pa-accid amount)
    (steps/apply-api repayment-loan-api {:loanAccountId pa-accid :amount amount :value-date due-date})
    (prn "Repay Loan" loan-accid amount)
    ;;(pp/pprint pa-acc)
    (steps/apply-api repayment-loan-api {:loanAccountId loan-accid :amount amount :value-date due-date})))

(defn repay-instalments [pa-accid start-num end-num]
  (map (fn [num]
  (prn "Repay instalment:" num)
         (repay-instalment pa-accid num))
       (range start-num (+ end-num 1))))

(api/setenv "env11")
(comment
(api/setenv "env11") ;; AL sandbox env
(def accid "OJKN485") ;; Identify the PA account 

;; [Main Functions] ********************************************************************************
;;
;; create a new set of linked loan + pa accounts
(set-dates 2021) ;; Call to set the dates that will be used - This was the example dates given
(set-dates 2016) ;; This will allow us to test
(create-loan-and-pa "EXAM1")

;; Repay instalments
(reset! LOANID "OJKN485") ;; create-loan-and-pa will reset
(repay-instalment @LOANID 59)
(repay-instalments @LOANID 19 59)
;; next function will remove all active accounts
(ext/zap-all-loans2 @CUSTKEY)

;; Get loan transactions (from the main loan-account)
(save-transactions-to-file @LOANID2 (str @LOANID2 "_TRANS.csv"))

;; [END] *************************************************************************************************

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

(api/PRINT (:last-call (steps/apply-api get-loan-schedule {:accid accid})))

(api/PRINT (:last-call (steps/apply-api get-loan-transactions {:accid @LOANID2})))


;; "2017-11-01T00:00:00Z"
)