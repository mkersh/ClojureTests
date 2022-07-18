;;; adjustable-interest-rate (AIR) mechanism examples
;;;
;;; TBD - Merge into http://localhost:3000/goto-file?&bookmark=30c41be8-396e-491a-ac51-cec8b41b6859
;;;

(ns http.api.mambu.demo.workshop0722.air_demo
(:require [http.api.json_helper :as api]
          [http.api.api_pipe :as steps]
          [clojure.data.json :as json]
          [mambu.extensions.product_factory.product_factory :as pf]
          [http.api.mambu.experiments.loan_schedule :as ext]
          ))

(defn call-api [api context]
  (:last-call (steps/apply-api api context)))


;; *****************************************************************
;; Mambu core level APIs for custom-fields config-as-code 
;;

(defn create-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/")
   :method api/POST
   :body (:body context)
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn delete-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/DELETE
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-account-api [context]
  {:url (str "{{*env*}}/loans/" (:loanid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn delete-loan-account-api [context]
  {:url (str "{{*env*}}/loans/" (:loanid context))
   :method api/DELETE
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn create-loan-api [context]
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
           "disbursementDetails" {
               "expectedDisbursementDate" (get-in context [:accountInterestRateSettings 0 "validFrom"])
           }
           "interestSettings" {"accountInterestRateSettings"
                               (:accountInterestRateSettings context)}
           "scheduleSettings" {"periodicPayment" (:periodic-payment context)
                               "gracePeriod" (:grace_period context)
                               "repaymentInstallments" (:num-installments context)
                               "principalRepaymentInterval" 1,
                               "repaymentPeriodCount" 1,
                               "scheduleDueDatesMethod" "INTERVAL",
                               "repaymentPeriodUnit" "MONTHS"}
           "redrawSettings" {"restrictNextDueWithdrawal" false}}})


(defn json-file-to-edn [fpath1]
  (let [json-str (slurp fpath1)]
    (json/read-str json-str)))

;; *****************************************************************
;; Higher level functions 
;;

(defn proddef-airprod1 [prod-temp]
  (-> {}
      (pf/product-default-template prod-temp)
      (pf/prod-name-id-desc "MK AIR PROD3" "mkairprod3" "")
      (pf/product-type-def :dynamic-term) ;; :fixed-term :dynamic-term :interest-free :tranched :revolving-credit
      (pf/prod-avail :client ["mk-test"])))

(defn create-air-product [prod-def-fn prod-template-file]
  (let [prod-temp (json-file-to-edn prod-template-file)
        prod-def (prod-def-fn prod-temp)]
    (pf/generate-loan-product prod-def)))

(defonce LOANID (atom ""))

(defn create-loan-account [context]
  (let [res (call-api create-loan-api context)
        loanid (get res "id")
        _ (reset! LOANID loanid)
        _ (prn (str "Created new loan-account: " loanid))
        ]
    loanid))

(defonce CUSTKEY (atom "8a1936ca8210c27a018211cfa8f05e48"))
(defonce PRODKEY (atom "8a194fd982119c470182119ebb1903ea"))

(comment

  (api/setenv "env18") ;; SalS demo

  ;; [1] Create a new adjustable-interest-rate product
  ;; NOTE: When moving to a new tenant change "indexSourceKey" in "AIR/air-prod1.txt"
  ;;
  ;; [1.1] DBEI product
  (create-air-product proddef-airprod1 "AIR/air-prod1.txt")
  (call-api delete-loan-product-api {:prodid "mkairprod3"})
  (get (call-api get-loan-product-api {:prodid "mkairprod3"}) "encodedKey")

  ;; [2] Create a loan-account linked to AIR product
  (create-loan-account {:prod-key @PRODKEY :acc-name "AIR ACCOUNT1"
                        :cust-key @CUSTKEY :amount 100000 :grace_period 0 :num-installments 60
                        :accountInterestRateSettings
                        [{"validFrom" "2022-08-18T10:27:47+02:00",
                          "interestRateSource" "FIXED_INTEREST_RATE",
                          "interestRate" 0.0},
                         {"validFrom" "2023-08-18T10:27:47+02:00",
                          "interestRateSource" "INDEX_INTEREST_RATE",
                          "indexSourceKey" "8a194466793b77a601793d0be8da691c",
                          "interestRateFloorValue" 2.0,
                          "interestRateCeilingValue" 20.0,
                          "interestRateReviewUnit" "DAYS",
                          "interestRateReviewCount" 31,
                          "interestSpread" 3}
                          ]})
  (call-api delete-loan-account-api {:loanid @LOANID})
  (ext/zap-all-loans2 @CUSTKEY)

;;
  )