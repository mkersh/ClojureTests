;; Testing the custom repayment ordering capability
(ns http.api.mambu.demo.training.repayment-custom
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

;; https://api.mambu.com/#loan-transactions-makerepayment
(defn repayment-api [context0]
  (let [defaults {:recalc-method nil}
        context (merge defaults context0)]
    {:url (str "{{*env*}}/loans/" (:loanid context) "/repayment-transactions")
     :method api/POST
     :body
     {"amount" (:amount context)
      "customPaymentAmounts" [{"amount" (:amount context)
                               "customPaymentAmountType" (:repay-type context)}]
      "prepaymentRecalculationMethod" (:recalc-method context)}
     :headers {"Accept" "application/vnd.mambu.v2+json"
               "Content-Type" "application/json"}}))

(defn get-loan-account-api [context]
  {:url (str "{{*env*}}/loans/" (:loanid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(comment 

(api/setenv "env1") ;; MK sandbox tenant

(def loanID "APRI861")

(steps/apply-api get-loan-account-api {:loanid loanID})

;; Next call will also be paid against the principal, regardless of the reallocation order
;; defined on the product
;; NOTE: It will only work if the product has been setup to allow for "Allow Custom Repayment Allocation"
(steps/apply-api repayment-api {:loanid loanID :amount 100.00 :repay-type "PRINCIPAL" })

;; Just pay off Interest
;; NOTE: You can only pay interest owed. If you exceed this amount then you get an EXCESS_REPAYMENT_ERROR
(steps/apply-api repayment-api {:loanid loanID :amount 14.00 :repay-type "INTEREST"})

;; Just pay off Fees
;; NOTE: You can only pay fees owed. If you exceed this amount then you get an EXCESS_REPAYMENT_ERROR
(steps/apply-api repayment-api {:loanid loanID :amount 33.33 :repay-type "MANUAL_FEE"})

;; Repay principal and define the prepaymentRecalculationMethod
;; Looks like you can't dynamically define the prepaymentRecalculationMethod
(steps/apply-api repayment-api {:loanid loanID :amount 10000.00 :repay-type "PRINCIPAL" :recalc-method "REDUCE_AMOUNT_PER_INSTALLMENT" })
;;
)