;;; Mambu audit-trail examaples
;;; #bookmark= 87ebc69d-f743-44b9-affd-5d188b4a21f3
(ns http.api.mambu.demo.workshop0722.audit-trail
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

;; **************************************************
;; Various APIs to generate events in the audit-trail

(defonce CUSTID (atom "")) ;; remeber the last custid created so we can delete

(defn call-api [api context]
  (:last-call (steps/apply-api api context)))

(defn create-customer-api [context]
    {:url (str "{{*env*}}/clients")
     :method api/POST
     :headers {"Accept" "application/vnd.mambu.v2+json"
               "Content-Type" "application/json"}
     :query-params {}
     :body {"firstName" (:first-name context)
            "lastName" (:last-name context)
            "preferredLanguage" "ENGLISH"
            "addresses" [{"country" "UK"
                          "city" "Liverpool"}]
            "notes" "Some Notes on this person"
            "assignedBranchKey" (:branchid context)}})


(defn create-customer [context]
  (let [resp (:last-call (steps/apply-api create-customer-api context))
        id (get resp "id")
        _ (reset! CUSTID id)]
    resp))

(defn delete-client-api [context]
  {:url (str "{{*env*}}/clients/" (:clientid context))
   :method api/DELETE
   :query-params {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-client-api [context]
  {:url (str "{{*env*}}/clients/" (:clientid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-detail-api [context]
  {:url (str "{{*env*}}/loans/" (:accid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn patch-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/PATCH
   :body (:body context)
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; **************************************************
;; Access to the activity-log API endpoints
;; https://api.mambu.com/v1/index.html#activities-get-all-activities 

(defn get-activity-log-api [context]
  (let [from-date (:from-date context)
        to-date (:to-date context)
        clientid (:clientid context)
        groupid (:groupid context)
        centreid (:centreid context)
        branchid (:branchid context)
        loan-productid (:loan-productid context)
        saving-productid (:saving-productid context)
        loanid (:loanid context)
        savingsid (:savingsid context)
        userid (:userid context)]
    {:url (str "{{*env*}}/activities")
     :method api/GET
     :query-params (-> {}
                       (api/add-param "from" from-date)
                       (api/add-param "to" to-date)
                       (api/add-param "clientID" clientid)
                       (api/add-param "groupID" groupid)
                       (api/add-param "centreID" centreid)
                       (api/add-param "branchID" branchid)
                       (api/add-param "loanProductID" loan-productid)
                       (api/add-param "savingsProductID" saving-productid)
                       (api/add-param "loanAccountID" loanid)
                       (api/add-param "savingsAccountID" savingsid)
                       (api/add-param "userID" userid)
                       )
     :headers {}}))

;; **************************************************
;; Access to the audit-trail API endpoints
;; NOTE: There is only one endpoint but it takes lots of optional parameters
;;

(defn get-events [context]
  (let [size (:size context)]
    {:url (str "{{*env*}}/v1/events")
     :method api/GET
     :query-params {"size" size}
     :headers {}}))

(defn get-events-from-previous-time [context]
  (let [size (:size context)
        occurred_at_lt (:occurred_at_lt context)]
   {:url (str "{{*env*}}/v1/events")
    :method api/GET
    :query-params {"size" size
                   "occurred_at[lt]" occurred_at_lt}
    :headers {}}))

(defn get-events-from-previous-time2 [context]
  (let [size (:size context)
        occurred_at_gt (:occurred_at_gt context)]
    {:url (str "{{*env*}}/v1/events")
     :method api/GET
     :query-params {"size" size
                    "occurred_at[gt]" occurred_at_gt}
     :headers {}}))

(defn get-object-viewed-by-user [context]
  (let [size (:size context)
        event_source (:event_source context)
        occurred_at (:occurred_at context)
        resource (:resource context)
        username (:user context)]
    {:url (str "{{*env*}}/v1/events")
     :method api/GET
     :query-params (-> {}
                       (api/add-param "size" size)
                       (api/add-param "event_source[eq]" event_source)
                       (api/add-param "occurred_at[gte]" occurred_at)
                       (api/add-param "resource[eq]" resource)
                       (api/add-param "username[eq]" username))
                       
     :headers {}}))


(comment

 (api/setenv "env16b") ;; MKH Audit
 (api/setenv "env17") ;; SEUK

;; [1] Get list of most recent events
 (call-api get-events {:size 10})

;; [2] Get list events from a previous point of time (less-than)
 (call-api get-events-from-previous-time {:size 10 :occurred_at_lt "2020-05-01T09:30:59.872Z"})

;; [3] Get list events from a previous point of time (greater-than)
 (call-api get-events-from-previous-time2 {:size 3 :occurred_at_gt "2020-05-01T09:30:59.872Z"})

;; [4] Call an API below and see how it is reported in the audit-trail
;;     Perform one or more of the following API calls and then goto [4.2] below and see them appear in the audit-trail
;;
;; [4.1a] - create a new customer
;;          NOTE: The api-key used needs to be different from the one used for the audit-trail APIs
 (api/setenv-local "env16a" (create-customer {:first-name "Test" :last-name "Cust" :branchid "8a19225181f7aeac0181f7de7c2a3a0d"}))
;; [4.1b] - delete previous created customer
 (api/setenv-local "env16a" (call-api delete-client-api {:clientid @CUSTID}))
;; [4.1c] - get a loan object
 (api/setenv-local "env16a" (call-api get-loan-detail-api {:accid "MZOH217"}))
;; [4.1d] - get a client object
 (api/setenv-local "env16a" (call-api get-client-api {:clientid "426245829"}))
;; [4.1e] - patch a loan-product
 (api/setenv-local "env16a" (let [patch-body [{"op" "REPLACE"
                                               "path" "notes"
                                               "value" "updated notes via patch - MK testing audit-trail"}]]
                              (call-api patch-loan-product-api {:prodid "fees" :body patch-body})))

;; *******************************
;; [4.2] - Now see that the event is recorded in the audit-trail
;;         NOTE: There may be a tiny delay before it appears but it should be there in near-realtime
;;
 (call-api get-events {:size 2})

;; [5] More specific searching of the audit-trail
 (call-api get-object-viewed-by-user
                              {:user "apiMambu1" ;; apiMambu1
                               :size 3 
                               :event_source nil ;; UI, API
                               :resource nil ;; clients, client, loans, ...
                               :occurred_at "2020-05-01T09:30:59.872Z"}) ;; GTE
                              

;; [6] Search the activity-log (our older audit log mechanism)
 (api/setenv-local
  "env16a"
  (call-api get-activity-log-api
            {:from-date "2022-05-12"
             :to-date "2022-07-14"
             :clientid nil
             :groupid nil
             :centreid nil
             :branchid nil
             :loan-productid nil
             :saving-productid nil
             :loanid nil            ;; IDs need to be encodedKey(s)
             :savingsid nil
             :userid nil ;; encKey
             }))



;; 
)