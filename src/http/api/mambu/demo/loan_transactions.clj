(ns http.api.mambu.demo.loan_transactions
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))


(defn get-loan-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-schedule-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/schedule")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn disburse-loan-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/disbursement-transactions")
   :method api/POST
   :body {"notes" "Disbursement from clojure"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn getall-transactions-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/transactions" )
   :method api/GET
   :body {"notes" "Adjust transaction from clojure"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn transaction-adjust-api [context]
  {:url (str "{{*env*}}/loans/transactions/" (:loanTransactionId context) ":adjust")
   :method api/POST
   :body {"notes" "Adjust transaction from clojure"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn repayment-loan-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/repayment-transactions")
   :method api/POST
   :body {"amount" (:amount context)
          "externalId" (:externalId context)
          "installmentEncodedKey" (:installmentEncodedKey context)
          "notes" "Repayment from clojure"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(comment
  (api/setenv "env1")
  (steps/apply-api get-loan-api {:loanAccountId "SCGC121"} :loan_get)
  (steps/apply-api get-loan-schedule-api {:loanAccountId "FPGT121"} :loan_get)
  (steps/apply-api disburse-loan-api {:loanAccountId "SCGC121"} :loan_disburse)
  (steps/apply-api repayment-loan-api {:loanAccountId "SCGC121" :amount "5.11" :externalId "MK1"} :loan_repayment) 
  (steps/apply-api repayment-loan-api {:loanAccountId "SCGC121" :amount "8257.02" :installmentEncodedKey "8a8187f467e0b11f0167e0b29bb40006"} :loan_repayment)

;; FPGT121 - A backdated loan (with all instalments late)
;; Paying off a specific instalment
;; I can only get it to work for late instalments and I am getting some strange behaviour
;; You have to payoff the exact amount owed (which is fine) but it then pays part interest for an instalment not identified??
(steps/apply-api repayment-loan-api {:loanAccountId "FPGT121" :amount "8658.36" :installmentEncodedKey "8a8187f37a58f054017a5c2ca6380745"} :loan_repayment)

;; Adjust a loan transaction - Reverse/revert a loan transaction
(steps/apply-api getall-transactions-api {:loanAccountId "FPGT121"} :loan_adjust)
(steps/apply-api transaction-adjust-api {:loanTransactionId "11643"} :loan_adjust)

(+ 8227.17 248.28)
;;
)