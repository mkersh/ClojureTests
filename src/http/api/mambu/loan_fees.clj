(ns http.api.mambu.loan-account
  (:require [http.api.json_helper :as api]
            [http.api.mambu.loan-account :as loan]
            [http.api.mambu.product-type :as pt]
            ))

(defn apply-fee [loanid feeid amount installNumber]
  (let [options {
                 :headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body {"amount" amount
                        "predefinedFeeKey" feeid
                        "installmentNumber" installNumber}}
        url (str "{{*env*}}/loans/" loanid "/fee-transactions")]
    (api/PRINT (api/POST url options))))

(defn enumerate [s]
  (map #(vector %2 %1) s (iterate inc 1)))

(defn apply-VAT [loanid feeid vatPercentage [instalNum instalDetails]]

  (let [prinDue (api/get-attr instalDetails ["principal" "amount" "due"] :DECIMAL)
        interestDue (api/get-attr instalDetails ["interest" "amount" "due"] :DECIMAL)
        vatAmount (api/round-num (/ (* (+ prinDue interestDue) vatPercentage) 100))]
    ;(prn "*****Installment:" instalNum)
    ;(prn "Principal Due" prinDue)
    ;(prn "Iterest Due" interestDue)
    ;(prn "VAT Amount" vatAmount)

    (apply-fee loanid feeid vatAmount instalNum)
    
    ))

(defn apply-VAT-to-instalments
  [loanid feeid vatPercentage]
  (let [schedule-list (enumerate
                       (get
                        (loan/loan-schedule loanid {:no-print true})
                        "installments"))]
    (map #(apply-VAT loanid feeid vatPercentage %1)  schedule-list)))


(comment

(api/setenv "env2")
(def NewAccountID "QRAL117")
(time (loan/get-loan NewAccountID))
(time (loan/loan-schedule NewAccountID))
(def feeID "8a818ece75a3211a0175a84cd03d1307")
(time (apply-fee NewAccountID feeID (format "%.2f" 11.4555555) 36))
(time (apply-VAT-to-instalments NewAccountID feeID 23.0))
(loan/loan-schedule NewAccountID {:no-print true})

;; Test loan (with 1 installment)

(def NewAccountID "FUTG403")
(time (apply-VAT-to-instalments NewAccountID feeID 23.0))

;; find the productTypeKey on the loan account
(time (pt/get-loan-product "8a818ece75a3211a0175a83127834f39"))



(def instalDetails {"encodedKey" "8a818fce75aea51d0175b1221f1d4551", "number" "1", "dueDate" "2020-12-10T01:00:00+01:00", "state" "PENDING", "principal" {"amount" {"expected" 5000.0, "paid" 0, "due" 5000.0}}, "interest" {"amount" {"expected" 27.0, "paid" 0, "due" 27.0}, "tax" {"expected" 0, "paid" 0, "due" 0}}, "fee" {"amount" {"expected" 0, "paid" 0, "due" 0}, "tax" {"expected" 0, "paid" 0, "due" 0}}, "penalty" {"amount" {"expected" 0, "paid" 0, "due" 0}, "tax" {"expected" 0, "paid" 0, "due" 0}}})

(api/get-attr instalDetails ["principal" "amount" "due"] )

)