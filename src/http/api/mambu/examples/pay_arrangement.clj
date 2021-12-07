(ns http.api.mambu.examples.pay-arrangement
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.string :as str]
            [java-time :as t]))


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
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(comment
(api/setenv "env11")
(def accid "YLKK457")

(let [accid "FNND040" ;; main = YLKK457 pay-arrange = FNND040
      amount 999.99]
  (api/PRINT (:last-call (steps/apply-api repayment-api {:accid accid :amount amount}))))

(api/PRINT (:last-call (steps/apply-api get-account {:accid accid})))

;;;;
)