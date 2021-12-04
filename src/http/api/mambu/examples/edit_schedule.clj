(ns http.api.mambu.examples.edit-schedule
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.string :as str]))


;; See doc https://api.mambu.com/v1/index.html#loan-accounts-update-loan-repayments 
(defn edit-loan-schedule [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/repayments")
   :method api/PATCH
   :query-params {}
   :body {"Repayments" [
    {"dueDate" "2020-02-01T01:00:00+01:00" 
     "feesDue" 0
     "feesUnappliedDue" 0
     "interestDue" 5000.0
     "isPaymentHoliday" false
     "parentAccountKey" "8a818ff17d470d02017d4814072c19fe"
     "penaltyDue" 0
     "principalDue" 100}
  ]}
   :headers {"Content-Type" "application/json"}
   
   
   })

(comment

{;;"dueDate" "2022-01-01T01:00:00+01:00"
 "dueDate" "2022-01-01"
 "encodedKey" "8a818e3f7d56df38017d5801dc552afc"
 "parentAccountKey" "8a818e3f7d56df38017d57ff74dd2a77"}



{"dueDate" "2020-12-01T01:00:00+01:00"
 "encodedKey" "8a818e3f7d56df38017d5801dc462aee"
 "feesDue" 0
 "feesUnappliedDue" 0
 "interestDue" 5000.0
 "isPaymentHoliday" false
 "parentAccountKey" "8a818e3f7d56df38017d57ff74dd2a77"
 "penaltyDue" 0
 "principalDue" 0}


;;;
)

(defn get-loan-schedule [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/schedule")
   :method api/GET
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-account [context]
  {:url (str "{{*env*}}/loans/" (:accid context))
   :method api/GET
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(comment
  (api/setenv "env2")
  (def accid "XVKB253")
  
  (api/PRINT (:last-call (steps/apply-api edit-loan-schedule {:accid accid})))
  (api/PRINT (:last-call (steps/apply-api get-loan-schedule {:accid accid})))
  (api/PRINT (:last-call (steps/apply-api get-account {:accid accid})))
  (* 23 6)
  ;;
  )