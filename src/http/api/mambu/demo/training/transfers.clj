(ns http.api.mambu.demo.training.transfers
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))


(defn transfer-api [context]
  {:url (str "{{*env*}}/deposits/" (:from-id context) "/transfer-transactions")
   :method api/POST
   :body {"amount" (:amount context)
          "transferDetails" {"linkedAccountId" (:to-id2 context)
                             "linkedAccountType" "DEPOSIT"}}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(comment

(api/setenv "env1") ;; MK sandbox tenant

;; Test the https://api.mambu.com/#deposit-transactions-maketransfer
;; to confirm that you can use to transfer money between accounts belonging to 2 separate customers
;; Answer: You can
(steps/apply-api transfer-api {:from-id "BDKA797" :to-id2 "ZGAJ270" :amount 100.00})

;;
)