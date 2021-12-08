(ns http.api.mambu.examples.edit-schedule
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.string :as str]
            [java-time :as t]
            ))

(defonce NUM_MONTHS (atom 1))
(defn add-month [date1]
  (let [date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (str (t/adjust date1-local t/plus (t/months @NUM_MONTHS)))))

(defn get-loan-schedule [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/schedule")
   :method api/GET
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; See doc https://api.mambu.com/v1/index.html#loan-accounts-update-loan-repayments 
(defn edit-loan-schedule [context]
     {:url (str "{{*env*}}/loans/" (:accid context) "/repayments")
      :method api/PATCH
      :query-params {}
      :body (:body context)
      :headers {"Content-Type" "application/json"}})

(defn distribute-dates-instalments [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        start-date (atom (:start-date context))
        rep-list (mapv
                  (fn [instal-obj]
                    (let [result {"dueDate" @start-date
                     "encodedKey" (get instal-obj "encodedKey")
                     "parentAccountKey" (get instal-obj "parentAccountKey")}
                     _ (reset! start-date (add-month @start-date)) ;; Move date on for next one
                     ]
                     result
                     ))
                  sched-list)
        rep-body {"repayments" rep-list}
        context1 (assoc context :body rep-body)]
    (edit-loan-schedule context1)))

(defn reduce-to-n-instalments [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        last-instal (get sched-list (- (:num-instal context) 1))
        last-instal-date (subs (get last-instal "dueDate") 0 10) ;; strip the timezone away
        sched-list2 (subvec sched-list (:num-instal context))
        rep-list (mapv
                  (fn [instal-obj]
                    {"dueDate" last-instal-date 
                     "encodedKey" (get instal-obj "encodedKey")
                     "parentAccountKey" (get instal-obj "parentAccountKey")})
                  sched-list2)
        rep-body {"repayments" rep-list}
        context1 (assoc context :body rep-body)]
    (edit-loan-schedule context1)
    ))

(defn edit-principal-on-instalment [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        last-instal (get sched-list (- (:num-instal context) 1))
        rep-list  [{"principalDue" (:amount context)
                    "encodedKey" (get last-instal "encodedKey")
                    "parentAccountKey" (get last-instal "parentAccountKey")}]
        rep-body {"repayments" rep-list}
        context1 (assoc context :body rep-body)]
    (edit-loan-schedule context1)
    ))

;; Simple test of the edit-loan-schedule
;; NOTE: You need to change the encodedKey and parentAccountKey to relate to keys on your account
(defn test-edit-loan-schedule [context]
  (let [context1 (assoc context :body
                        {"repayments" [{"dueDate" "2022-02-04"
                                        "encodedKey" "8a818ef67d8012b4017d871724c14276"
                                        "parentAccountKey" "8a818ef67d8012b4017d871446da41af"
                                        "principalDue" 70.77}]})]
    (edit-loan-schedule context1)))





(defn get-account [context]
  {:url (str "{{*env*}}/loans/" (:accid context))
   :method api/GET
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(comment
  (api/setenv "env2")
  (def accid "NMMZ161")
  (reset! NUM_MONTHS 3) ;; used by distribute-dates-instalments

  (api/PRINT (:last-call (steps/apply-api distribute-dates-instalments {:accid accid :start-date "2022-03-07"})))
  (api/PRINT (:last-call (steps/apply-api reduce-to-n-instalments {:accid accid :num-instal 5})))
  (api/PRINT (:last-call (steps/apply-api edit-principal-on-instalment {:accid accid :num-instal 13 :amount 5000.00})))
  
  (api/PRINT (:last-call (steps/apply-api test-edit-loan-schedule {:accid accid})))
  (api/PRINT (:last-call (steps/apply-api get-loan-schedule {:accid accid})))
  (api/PRINT (:last-call (steps/apply-api get-account {:accid accid})))
  


  ;; 
  (api/setenv "env11")
  (def accid "POGP216") ;; LOAN2
  (def accid "YFJW588") ;; LOAN1 - Balloon
  
  (def accid "WOAT914") ;; Capitalised Loan
  
  
  (* 365 (/ (/ 332.87 50000) 27))
  ;;
  )


  (comment

    {;;"dueDate" "2022-01-01T01:00:00+01:00"
     "dueDate" "2022-01-01"
     "encodedKey" "8a818e3f7d56df38017d5801dc552afc"
     "parentAccountKey" "8a818e3f7d56df38017d57ff74dd2a77"}



    {"dueDate" "2020-12-01T01:00:00+01:00"
     "encodedKey" "8a818e3f7d56df38017d5801dc462aee"
     "feesDue" 0
     "feesUnappliedDue" 0
     "interestDue" 24.54
     "isPaymentHoliday" false
     "parentAccountKey" "8a818e3f7d56df38017d57ff74dd2a77"
     "penaltyDue" 0
     "principalDue" 0}


;;;
    )