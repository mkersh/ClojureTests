;; Test https://community.mambu.com/t/patching-deposit-transaction-204-but-not-patched/3348
(ns http.api.mambu.demo.custom-fields-transactions
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

(defn patch-deposit-transaction-api [context]
  {:url (str "{{*env*}}/deposits/transactions/" (:transactionid context))
   :method api/PATCH
   :body [
          {"op" "add"
           "path" (:fieldset context)
           "value"(:value context)}]
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-deposit-transaction-api [context]
  {:url (str "{{*env*}}/deposits/transactions/" (:transactionid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(comment
  ;; Test environment to use
  (api/setenv "env1")

  ;; When getting the transaction you can use either the id or the encodedKey
  (let [tran-details (steps/apply-api get-deposit-transaction-api {:transactionid "936"})
        encodedKey (get-in tran-details [:last-call "encodedKey"])]
    (def transEncodedKey encodedKey)
    tran-details)
  transEncodedKey

  ;; When updating a custom field associated with a transaction you currently need to use the encodeKey
  ;; It should work with the transaction id as well but doesn't do currently 
  (steps/apply-api patch-deposit-transaction-api {:transactionid transEncodedKey
                                                  :fieldset "/_TestTransactionExtra"
                                                  :value {"field1" "Test data 111"}})



  (steps/apply-api patch-deposit-transaction-api {:transactionid transEncodedKey
                                                  :fieldset "_DepositTransactionFieldsExtra"
                                                  :value {"TestField" "Hello World!!!4444"}})


  ;; Setting a field on the default "_Transaction_Details_Transactions" fieldset
  ;; NOTE: You can't patch these fields ONLY custom fields
  (steps/apply-api patch-deposit-transaction-api {:transactionid transEncodedKey
                                                  :fieldset "_Transaction_Details_Transaction"
                                                  :value {"BANK_NUMBER_TRANSACTION_CHANNEL_" "BBB"}})

;;  
  )
