(ns http.api.mambu.deposit-account
  (:require [http.api.json_helper :as api]
            [http.api.mambu.customer :as cust]))

(defn list-deposits [& opt-overrides]
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
        url "{{*env*}}/deposits"]
    (api/PRINT (api/GET url options))))

(defn get-deposit [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        detailLevel (or (:details-level moreOpts) "FULL")
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" detailLevel}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/deposits/" id)]
    (prn "**get-deposit " moreOpts)
    (api/PRINT (api/GET url options) moreOpts)))

(defn create-deposit [prodid clientIdOrEncId & opt-overrides]
  (let [moreOpts (first opt-overrides)
        clientId (cust/get-customer-encid clientIdOrEncId)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body  {"accountType" "REGULAR_SAVINGS"
                         "name" "Deposit Account"
                         "currencyCode" "EUR"
                         "accountHolderKey" clientId
                         "productTypeKey" prodid
                         "accountHolderType" "CLIENT"}}
        options (merge optdefs moreOpts)]
    (api/PRINT (api/POST "{{*env*}}/deposits" options))))

(defn approve-deposit [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :body {"action" "APPROVE"
                        "notes" "more notes"}
                 :query-params {}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/deposits/" id ":changeState")]
    (api/PRINT (api/POST url options))))

(defn deposit-transaction [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :body {"depositAccountId" id
                        "amount" 10.0}
                 :query-params {}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/deposits/" id "/deposit-transactions")]
    (api/PRINT (api/POST url options))))

(defn withdrawal-transaction [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :body {"depositAccountId" id
                        "amount" 10.0}
                 :query-params {}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/deposits/" id "/withdrawal-transactions")]
    (api/PRINT (api/POST url options))))

(defn transfer-transaction [from-id to-id2 & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :body {"amount" 10.0
                        "transferDetails" {"linkedAccountId" to-id2
                                           "linkedAccountType" "DEPOSIT"
                                           ;"linkedAccountKey" "8a81868e6808ec4501680e7898bf26e5"
                                           }}
                 :query-params {}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/deposits/" from-id "/transfer-transactions")]
    (api/PRINT (api/POST url options))))

(defn delete-deposit [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/deposits/" id)]
    (api/PRINT (api/DELETE url options))))

(defn put-deposit [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body
                 {}}
        url (str "{{*env*}}/deposits/" id)
        options (merge optdefs moreOpts)]
    (api/PRINT (api/PUT url options))))

(defn patch-deposit [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body [{"op" "ADD"
                         "path" "loanName"
                         "value" "Lease3BBBBB"}]}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/deposits/" id)]
    (api/PRINT (api/PATCH url options))))



(defn close-deposit [acc-obj]
  (prn "*******In close-deposit " acc-obj)
  (let [accid (get acc-obj "id")]
    (try
      (delete-deposit accid {:throw-errors true})
      (catch Exception _ 
        (prn "lose-deposit exception")))
    )
  )

; Test in your REPL: Select line to run ctl+alt+c <space>
; Use api/find-path and api/extract-attrs to navigate through results
(comment

  (time (list-deposits {:details-level "BASIC" :limit 1000 :offset 109}))
  (time (list-deposits {:limit 1000}))

  ;; ({"id" "WBOS115"} {"id" "OJBS671"} {"id" "ARNM393"} {"id" "BKDP961"} {"id" "TVTV962"} {"id" "LLQB002"} {"id" "ADNP290"})

  (def NewAccountID "MSYI692")
  (def TOAccountID "AWZC853")
  
  (time (get-deposit NewAccountID))
  (time (get-deposit NewAccountID {:details-level "BASIC"}))

  (time (create-deposit "8a81867d6a01398b016a01a0552b006c" "896933805"))

  (time (approve-deposit NewAccountID))
  (time (approve-deposit TOAccountID))

  (time (deposit-transaction NewAccountID))
  (time (deposit-transaction NewAccountID
                             {:body {"depositAccountId" NewAccountID
                                     "amount" 10.11}}))

  (time (withdrawal-transaction NewAccountID))

  (time (transfer-transaction NewAccountID TOAccountID ))

  (time (close-deposit (get-deposit NewAccountID {:no-print true})))
  (time (delete-deposit NewAccountID))

  (time (patch-deposit NewAccountID {:body [{"op" "ADD"
                                             "path" "loanName"
                                             "value" "LeaseLoan33332"}]}))
  (time (patch-deposit NewAccountID))

  (time (put-deposit NewAccountID {:body {"creationDate" "2020-01-08T09:01:13+02:00"
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
                                          "lastSetToArrearsDate" "2019-02-02T00:00:00+02:00"}})))


(use 'clojure.test)

(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 7 (+ 3 4))))

(comment
(run-tests 'http.api.account)

(run-all-tests #"http.*")
)