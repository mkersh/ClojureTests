;;; Mambu audit-trail examaples
;;; #bookmark= 87ebc69d-f743-44b9-affd-5d188b4a21f3
(ns http.api.mambu.demo.workshop0722.audit-trail
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

;; **************************************************
;; Various APIs to generate events in the audit-trail

(def create-customer
  (fn [context]
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
            "assignedBranchKey" (:branchid context)}}))

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
;; Access to the audit-trail API endpoints
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


;; event_source [eq] =UI&occurred_at [gte] =2020-04-20T00:00:01.040Z&resource [eq] =client&username [eq] =michaelgbp
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
                       (api/add-param "username[eq]" username)
                       )
     :headers {}}))


(comment

(api/setenv "env16b") ;; MKH Audit
(api/setenv "env15") ;; MH test 

;; [1] Get list of most recent events
(:last-call (steps/apply-api get-events {:size 10}))

;; [2] Get list events from a previous point of time (less-than)
(:last-call (steps/apply-api get-events-from-previous-time {:size 10 :occurred_at_lt "2020-05-01T09:30:59.872Z"}))

;; [3] Get list events from a previous point of time (greater-than)
(:last-call (steps/apply-api get-events-from-previous-time2 {:size 3 :occurred_at_gt "2020-05-01T09:30:59.872Z"}))

;; [4] Call an API and see how it is reported in audit-trail
(api/setenv-local "env16a" (:last-call (steps/apply-api create-customer {:first-name "Test" :last-name "Cust" :branchid "8a19225181f7aeac0181f7de7c2a3a0d"})))
(api/setenv-local "env16a" (:last-call (steps/apply-api get-loan-detail-api {:accid "MZOH217"})))
(api/setenv-local "env16a" (:last-call (steps/apply-api get-client-api {:clientid "426245829"})))
(api/setenv-local "env16a" (let [patch-body [{"op" "REPLACE"
                                             "path" "notes"
                                             "value" "updated notes via patch - MK testing audit-trail"}]]
                             (steps/apply-api patch-loan-product-api {:prodid "fees" :body patch-body})))
;; Now see that the event is recorded in the audit-trail
;; NOTE: There may be a tiny delay before it appears but it should be there in near-realtime
(:last-call (steps/apply-api get-events {:size 1}))

;; [5] More specific searching of the audir-trail
(:last-call (steps/apply-api get-object-viewed-by-user
                             {:user "apiMambu1" 
                              :size 3 
                              :event_source nil ;; UI, API
                              :resource nil ;; clients, client, loans, ...
                              :occurred_at "2020-05-01T09:30:59.872Z" ;; GTE
                              }))



;; 
)