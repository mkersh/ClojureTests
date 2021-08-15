;;; A higher level set of APIs for Mambu credit-arrangements
;;; The new higher level APIs exposed are:
;;;     (get-ca-schedule)
;;;     (get-ca-transactions)
;;;     
(ns http.api.credit-arrangements (:require [http.api.json_helper :as api]
                                           [http.api.api_pipe :as steps]))


;; https://api.mambu.com/#mambu-api-v2-credit-arrangements
(defn get-ca-details [context]
  {:url (str "{{*env*}}/creditarrangements/" (:caid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-ca-accounts [context]
  {:url (str "{{*env*}}/creditarrangements/" (:caid context) "/accounts")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-schedule-api [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/schedule")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-trans-api [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/transactions")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;;; The following is a more efficient way to search for transactions
(defn get-loan-trans-api2 [context]
  {:url (str "{{*env*}}/loans/transactions:search")
   :method api/POST
   :query-params {"detailsLevel" "FULL"}
   :body {"filterCriteria" [{"field" "parentAccountKey"
                             "operator" "EQUALS"
                             "value" "8a818f1676715ba301767552950e2cbc"}
                            {"field" "creationDate"
                             "operator" "AFTER"
                             "value" "2020-12-20T00:00:35+01:00"}]}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})



(defn get-loan-trans2 []
  (prn "In get-loan-trans2")
  (let [steps {:context {:accid "GHGHG"}
               :steps [{:request get-loan-trans-api2
                        :post-filter (steps/save-last-to-context :loan-trans2)}]}
        context2 (steps/process-collection steps)]
    context2))

(comment

  (get-loan-trans2))

(defn merge-append [context1 context2 item-to-append]
  (let [val1 (get context1 item-to-append)
        val2 (get context2 item-to-append)
        to-insert (conj val1 val2)
        context3 (assoc context2 item-to-append to-insert)]
    (merge context1 context3)))



(defn get-loan-schedule [context loan-obj]
  (prn "In get-loan-schedule")
  (prn (get loan-obj "id"))
  (let [accid (get loan-obj "id")
        steps {:context {:accid accid}
               :steps [{:request get-loan-schedule-api
                        :post-filter (steps/save-last-to-context2 accid :loan-schedule)}]}
        context2 (steps/process-collection steps)]
    (merge-append context context2 :loan-schedule)))

(defn get-all-loan-schedules [context]
  (reduce get-loan-schedule context (get (:ca-accounts context) "loanAccounts")))


(defn get-loan-transacions [context loan-obj]
  (prn "In get-loan-transacions")
  (prn (get loan-obj "id"))
  (let [accid (get loan-obj "id")
        steps {:context {:accid accid}
               :steps [{:request get-loan-trans-api
                        :post-filter (steps/save-last-to-context2 accid :loan-trans)}]}
        context2 (steps/process-collection steps)]
    (merge-append context context2 :loan-trans)))

(defn get-all-loan-transacions [context]
  (reduce get-loan-transacions context (get (:ca-accounts context) "loanAccounts")))

(def get-ca-schedule-step1
  "Get the combined schedule for all the loans underneath a given caid"
  {:context {:custid "286020631"
             :caid "TLL564"
             :loan-schedule []}
   :steps [;; [STEP-1] Get CA Details
           {:request get-ca-details
            :post-filter (steps/save-last-to-context :ca-details)}

           ;; [STEP-2] Get CA Accounts
           {:request get-ca-accounts
            :post-filter (steps/save-last-to-context :ca-accounts)}]})

(defn addAmounts [p1 p2]
  (let [p1-amount (get p1 "amount")
        p2-amount (get p2 "amount")
        p1-expected (or (get p1-amount "expected") 0.00)
        p1-paid (or (get p1-amount "paid") 0.00)
        p1-due (or (get p1-amount "due") 0.00)
        p2-expected (get p2-amount "expected")
        p2-paid (get p2-amount "paid")
        p2-due (get p2-amount "due")]

    {"amount" {"expected" (+ p1-expected p2-expected), "paid" (+ p1-paid p2-paid), "due" (+ p1-due p2-due)}}))

(defn add-date-to-map [accid]
  (fn [oldMap newItem]
    (prn "Add schedule for " accid)
    (prn newItem)
    (let [dueDate (get newItem  "dueDate")
          oldItem (get oldMap dueDate)
          oldPrincipal (:principle oldItem)
          principal (get newItem  "principal")
          new-principal (addAmounts oldPrincipal principal)

          oldInterest (:interest oldItem)
          interest (get newItem  "interest")
          new-interest (addAmounts oldInterest interest)]
      (assoc oldMap dueDate  {:principle new-principal, :interest new-interest}))))

(defn add-schedule-item [shMap install-obj]
  (let [accid (first (keys install-obj))
        inList (first (vals install-obj))
        inList2 (get inList "installments")]
    (reduce (add-date-to-map accid) shMap inList2)))

(defn create-date-map [sh-list]
  (reduce add-schedule-item {} sh-list))

(defn merge-schedules [context]
  (let [schedule-list (:loan-schedule context)
        date-map (create-date-map schedule-list)]
    (assoc context :date-map date-map)))

(defn get-ca-schedule []
  (let [context (steps/process-collection get-ca-schedule-step1)
        context2 (get-all-loan-schedules context)
        parts (:loan-schedule context2)
        context3 (merge-schedules context2)
        shList (into [] (:date-map context3))
        shList-sorted (sort shList)
        ca-schedule {:total shList-sorted, :parts parts}]
    ca-schedule))

(defn get-ca-transactions []
  (let [context (steps/process-collection get-ca-schedule-step1)
        context2 (get-all-loan-transacions context)
        ca-trans (:loan-trans context2)]
    ca-trans))

(comment
  (api/setenv "env2")
  (get-ca-schedule)
  (get-ca-transactions))


(comment

  (def test {"MHVT083"
             {"installments"
              [{"encodedKey" "8a818ecf76760d0c01767620625e0301"
                "number" "1"
                "dueDate" "2020-10-03T02:00:00+02:00"
                "state" "LATE"
                "principal" {"amount" {"expected" 2.86, "paid" 0, "due" 2.86}}
                "interest" {"amount" {"expected" 0.14, "paid" 0, "due" 0.14}, "tax" {"expected" 0, "paid" 0, "due" 0}}
                "fee" {"amount" {"expected" 0, "paid" 0, "due" 0}, "tax" {"expected" 0, "paid" 0, "due" 0}}
                "penalty" {"amount" {"expected" 0, "paid" 0, "due" 0}, "tax" {"expected" 0, "paid" 0, "due" 0}}}
               {"encodedKey" "8a818ecf76760d0c01767621a15d032a"
                "number" "2"
                "dueDate" "2020-11-03T01:00:00+01:00"
                "state" "LATE"
                "principal" {"amount" {"expected" 0.89, "paid" 0, "due" 0.89}}
                "interest" {"amount" {"expected" 2.11, "paid" 0, "due" 2.11}, "tax" {"expected" 0, "paid" 0, "due" 0}}
                "fee" {"amount" {"expected" 0, "paid" 0, "due" 0}, "tax" {"expected" 0, "paid" 0, "due" 0}}
                "penalty" {"amount" {"expected" 0, "paid" 0, "due" 0}, "tax" {"expected" 0, "paid" 0, "due" 0}}}
               {"encodedKey" "8a818ecf76760d0c01767621a15d032b"
                "number" "3"
                "dueDate" "2020-12-03T01:00:00+01:00"
                "state" "LATE"
                "principal" {"amount" {"expected" 0.95, "paid" 0, "due" 0.95}}
                "interest" {"amount" {"expected" 2.05, "paid" 0, "due" 2.05}, "tax" {"expected" 0, "paid" 0, "due" 0}}
                "fee" {"amount" {"expected" 0, "paid" 0, "due" 0}, "tax" {"expected" 0, "paid" 0, "due" 0}}
                "penalty" {"amount" {"expected" 0, "paid" 0, "due" 0}, "tax" {"expected" 0, "paid" 0, "due" 0}}}]}})

  (add-schedule-item {} test))
