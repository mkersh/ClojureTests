(ns http.api.mambu.demo.sandbox
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))


(defn get-deposit-accounts-api [context]
  {:url (str "{{*env*}}/deposits/")
   :method api/GET
    :query-params {
        "detailsLevel" "FULL"
        "accountHolderType" "CLIENT"
        "accountHolderId" (:custid context)
        }
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-accounts-api [context]
  {:url (str "{{*env*}}/loans/")
   :method api/GET
   :query-params {"detailsLevel" "FULL"
                  "accountHolderType" "CLIENT"
                  "accountHolderId" (:custid context)}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-detail-api [context]
  {:url (str "{{*env*}}/loans/" (:accid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(defn match-productkey [context accListID]
(let [accList (accListID context)
      productKey (:productKey context)]
  (filter #(= (get % "productTypeKey") productKey) accList))
)
(defn find-customer-account [context]
  (let [context1 (steps/apply-api get-deposit-accounts-api context :trans_details)
        resList (match-productkey context1 :trans_details)]
    (if (> (count resList) 0)
      (first resList)
      ;; No deposit account match look in loan accounts
      (let [context1 (steps/apply-api get-loan-accounts-api context :trans_details)
            resList2 (match-productkey context1 :trans_details)]
        (if (> (count resList2) 0)
          (first resList2)
          nil)))))


(defn createLoan-api2 [_]
  {:url (str "{{*env*}}/loans/")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"_mambuLoanCFieldSet" {"mambuLoanCFieldId" "Test it"}
          "loanName" "Opp MK2"
          "arrearsTolerancePeriod" 0
          "loanAmount" 7000
          "productTypeKey" "8a8187d569e40ebe0169e70b778a2572"
          "interestSettings" {"interestRate" 2.5
                              "interestRateSource" "FIXED_INTEREST_RATE"}
          "disbursementDetails" {"expectedDisbursementDate" "2021-02-18T00:00:00+01:00"
                                 "firstRepaymentDate" "2021-03-18T00:00:00+01:00"
                                 }
          "scheduleSettings" {"gracePeriod" 0
                              "repaymentInstallments" 12
                              "repaymentPeriodCount" 1
                              "repaymentPeriodUnit" "MONTHS"}

          "accountHolderKey" "8a8186aa77b50bf70177b6884e98276b"
          "accountHolderType" "CLIENT"}})

(defn createLoan-api [_]
  {:url (str "{{*env*}}/loans/")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {
        "_mambuLoanCFieldSet" {
            "mambuLoanCFieldId" "Test it"
        },
        "loanName" "Opp MK2",
        "arrearsTolerancePeriod" 0,
        "loanAmount" 7000,
        "productTypeKey" "8a19a0ad77b435a90177b491abbd0cf5",
        "interestSettings" {
            "interestRate" 2.5,
            "interestRateSource" "FIXED_INTEREST_RATE"
        },
        "disbursementDetails" {
            "expectedDisbursementDate" "2021-02-18T00:00:00+01:00",
            "firstRepaymentDate" "2021-03-18T00:00:00+01:00",
            "fees" [
                
            ]
        },
        "scheduleSettings" {
            "gracePeriod" 0,
            "repaymentInstallments" 12,
            "repaymentPeriodCount" 1,
            "repaymentPeriodUnit" "MONTHS"
        }

        "accountHolderKey" "8a19ddf377b43f9a0177b48b5dc608fd",
        "accountHolderType" "GROUP"
    }})

(comment
  (api/setenv "env2")
  ;; 8a818f5f6cbe6621016cbf6d66075e54 - RCA purchase PROD ID
  ;; 8a818f5f6cbe6621016cbf7310ff6064 - TEMP Card PROD ID
  (def context {:custid "267931320" :productKey "8a818f5f6cbe6621016cbf6d66075e54"})
  (find-customer-account context)

  (count [])

(api/setenv "env4")
(steps/apply-api createLoan-api {} :loan_details)
(steps/apply-api get-loan-detail-api {:accid "JBIM204"} :loan_details)

(api/setenv "env1")
(steps/apply-api createLoan-api2 {} :loan_details)
(steps/apply-api get-loan-detail-api {:accid "LFTZ124"} :loan_details)

(+ 898.77 50)
)