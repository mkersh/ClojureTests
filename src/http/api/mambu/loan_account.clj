(ns http.api.mambu.loan-account
  (:require [http.api.json_helper :as api]
            [http.api.mambu.customer :as cust]))

(defn list-loans [& opt-overrides]
  (let [moreOpts (first opt-overrides)
        detailLevel (or (:details-level moreOpts) "FULL")
        limitVal (or (:limit moreOpts) 50)
        offset (or (:offset moreOpts) 0)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" (or detailLevel "FULL")
                                "paginationDetails" "ON"
                                "limit" limitVal
                                "offset" offset}}
        options (merge optdefs moreOpts)
        url "{{*env*}}/loans"]
    (api/PRINT (api/GET url options))))

(defn get-loan [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        detailLevel (or (:details-level moreOpts) "FULL")
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" detailLevel}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/loans/" id)]
    (prn "**get-loan " moreOpts)
    (api/PRINT (api/GET url options) moreOpts)))

(defn create-loan [prodid clientIdOrEncId & opt-overrides]
  (let [moreOpts (first opt-overrides)
        clientId (cust/get-customer-encid clientIdOrEncId)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body  {"loanAmount" 30000.0
                         "loanName" "MKCurTest1"
                         "accountHolderKey" clientId
                         "productTypeKey" prodid
                         "accountHolderType" "CLIENT"
                         "scheduleSettings" {"defaultFirstRepaymentDueDateOffset" 0
                                             "gracePeriod" 0
                                             "gracePeriodType" "NONE"
                                             "paymentPlan" []
                                             "periodicPayment" 0.0
                                             "principalRepaymentInterval" 1
                                             "repaymentInstallments" 12
                                             "repaymentPeriodCount" 1
                                             "repaymentPeriodUnit" "MONTHS"
                                             "repaymentScheduleMethod" "DYNAMIC"
                                             "scheduleDueDatesMethod" "INTERVAL"}
                         "interestSettings" {"interestRate" 2.0}}}
        options (merge optdefs moreOpts)]
    (api/PRINT (api/POST "{{*env*}}/loans" options))))


(defn delete-loan [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/loans/" id)]
    (api/PRINT (api/DELETE url options))))

(defn put-loan [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body
                 {}}
        url (str "{{*env*}}/loans/" id)
        options (merge optdefs moreOpts)]
    (api/PRINT (api/PUT url options))))

(defn patch-loan [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body [{"op" "ADD"
                         "path" "loanName"
                         "value" "Lease3BBBBB"}]}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/loans/" id)]
    (api/PRINT (api/PATCH url options))))

(defn writeoff-loan [accid & opt-overrides]
  (prn "*****IN writeoff-loan")
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Content-Type" "application/json"}
                 :query-params {}
                 :body {"type" "WRITE_OFF"
                        "notes" "Account written off"}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/loans/" accid "/transactions")]
    (api/PRINT (api/POST url options)))
  )


(defn reject-loan [accid & opt-overrides]
  (prn "*****IN reject-loan")
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Content-Type" "application/json"}
                 :query-params {}
                 :body {"type" "REJECT"
                        "notes" "Account REJECTED"}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/loans/" accid "/transactions")]
    (api/PRINT (api/POST url options))))

(defn close-loan [acc-obj]
  (prn "*******In close-loan " acc-obj)
  (let [accid (get acc-obj "id")]
    (try
      (delete-loan accid {:throw-errors true})
      (catch Exception _ 
        (try (reject-loan accid {:throw-errors true})
             (catch Exception _ (writeoff-loan accid)))))
    )
  )

  (defn loan-schedule [id & opt-overrides]
    (let [moreOpts (first opt-overrides)
          optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                   :query-params {}}
          options (merge optdefs moreOpts)
          url (str "{{*env*}}/loans/" id "/schedule")]
       (api/GET url options)))

; Test in your REPL: Select line to run ctl+alt+c <space>
; Use api/find-path and api/extract-attrs to navigate through results
(comment
  (api/setenv "env2")
  (time (list-loans {:details-level "BASIC" :limit 1000 :offset 110}))
  (time (list-loans))

  ;; ({"id" "WBOS115"} {"id" "OJBS671"} {"id" "ARNM393"} {"id" "BKDP961"} {"id" "TVTV962"} {"id" "LLQB002"} {"id" "ADNP290"})

  (def NewAccountID "WVJN135")
  (time (get-loan NewAccountID))
  (time (get-loan NewAccountID {:details-level "BASIC"}))

  (time (create-loan "8a8187366a01d4a1016a023792a500b9" "896933805"))
 
  (time (close-loan (get-loan NewAccountID {:no-print true})))
  (time (writeoff-loan NewAccountID))
  (time (delete-loan NewAccountID))

  (time (patch-loan NewAccountID {:body [{"op" "ADD"
                                               "path" "loanName"
                                               "value" "LeaseLoan33332"}]}))
  (time (patch-loan NewAccountID))

  (time (put-loan NewAccountID {:body {"creationDate" "2020-01-08T09:01:13+02:00"
                                      "guarantors" []
                                      "approvedDate" "2020-01-08T08:01:18+02:00"
                                      "activationTransactionKey" "8a8187f76f83bafd016f83f50f70029c"
                                      "accruedInterest" 0.44
                                      "prepaymentSettings"
                                      {"prepaymentRecalculationMethod" "NO_RECALCULATION"
                                       "principalPaidInstallmentStatus" "PARTIALLY_PAID"
                                       "applyInterestOnPrepaymentMethod" "AUTOMATIC"}
                                      "productTypeKey" "8a8187f76f83bafd016f83f2dcb1026d"
                                      
                                      "id" "JOEM230"
                                      "futurePaymentsAcceptance" "NO_FUTURE_PAYMENTS"
                                      "lastModifiedDate" "2020-01-08T09:07:45+02:00"
                                      "daysInArrears" 567
                                      "lastAccountAppraisalDate" "2020-08-21T00:34:47+02:00"
                                      "arrearsTolerancePeriod" 0
                                      "latePaymentsRecalculationMethod" "OVERDUE_INSTALLMENTS_INCREASE"
                                      "assignedBranchKey" "8a8187136eaec3f5016eb324a5242dd8"
                                      "encodedKey" "8a8187f76f83bafd016f83f3abd00284"
                                      "accountArrearsSettings"
                                      {"encodedKey" "8a8187f76f83bafd016f83f50f5c028c"
                                       "toleranceCalculationMethod" "ARREARS_TOLERANCE_PERIOD"
                                       "dateCalculationMethod" "ACCOUNT_FIRST_WENT_TO_ARREARS"
                                       "nonWorkingDaysMethod" "EXCLUDED"
                                       "tolerancePeriod" 0}
                                      "lastInterestAppliedDate" "2020-01-08T08:07:46+02:00"
                                      "penaltySettings" {"loanPenaltyCalculationMethod" "NONE"}
                                      "interestFromArrearsAccrued" 6194.44
                                      "tranches" []
                                      "disbursementDetails"
                                      {"encodedKey" "8a8187f76f83bafd016f83f50f5c028a"
                                       "expectedDisbursementDate" "2019-01-01T00:00:00+02:00"
                                       "disbursementDate" "2019-01-01T00:00:00+02:00"
                                       "transactionDetails"
                                       {"encodedKey" "8a8187f76f83bafd016f83f50f5c028b"
                                        "transactionChannelKey" "8a818e74677a2e9201677ec2b4c336a6"
                                        "transactionChannelId" "cash"
                                        "internalTransfer" false}
                                       "fees" []}
                                      "assets" []
                                      "accountState" "ACTIVE_IN_ARREARS"
                                      "paymentMethod" "HORIZONTAL"
                                      "allowOffset" false
                                      "interestSettings"
                                      {"interestChargeFrequency" "ANNUALIZED"
                                       "interestApplicationMethod" "REPAYMENT_DUE_DATE"
                                       "accrueLateInterest" true
                                       "interestCalculationMethod" "DECLINING_BALANCE"
                                       "interestBalanceCalculationMethod" "ONLY_PRINCIPAL"
                                       "interestRateSource" "FIXED_INTEREST_RATE"
                                       "interestType" "SIMPLE_INTEREST"
                                       "interestRate" 10.1
                                       "accrueInterestAfterMaturity" false}
                                      "scheduleSettings"
                                      {"repaymentPeriodUnit" "MONTHS"
                                       "periodicPayment" 0
                                       "paymentPlan" []
                                       "hasCustomSchedule" false
                                       "gracePeriod" 0
                                       "repaymentPeriodCount" 1
                                       "scheduleDueDatesMethod" "INTERVAL"
                                       "repaymentScheduleMethod" "DYNAMIC"
                                       "repaymentInstallments" 12
                                       "principalRepaymentInterval" 1
                                       "gracePeriodType" "NONE"}
                                      "fundingSources" []
                                      "daysLate" 567
                                      "accountHolderType" "CLIENT"
                                      "loanAmount" 100000.0
                                      "loanName" "Lease3XXXXXX"
                                      "accountHolderKey" "8a8187136eaec3f5016eb32791322de2"
                                      "accruedPenalty" 0
                                      "lastSetToArrearsDate" "2019-02-02T00:00:00+02:00"}}))
  )


(use 'clojure.test)

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(comment
(run-tests 'http.api.account)

(run-all-tests #"http.*")
)