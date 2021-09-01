;;; Examples/tests for how to efficiently replicate data/objects from Mambu to a data-lake/DWH
;;;
;;; For each object-type to replicate we are looking for:
;;; (a) A single API endpoint that allows us to page through all object(s)
;;; (b) Also endpoint should have the ability to sortBy "lastModifiedDate ASC" (oldest to youngest) 
;;;     This will allow us to efficiently continue replication from a previous saved lastModifiedDate
;;;     NOTE: The sortBy should be oldest to youngest. If the other way around (youngest to oldest):
;;;           Updates would be added to the front as you page through causing chaos.
;;; (c) To support replication continuation:
;;;     We will store previous-position as {:page-size <s> :page-num <n> :lastModifiedDate <date>}
;;;     Finding initial start position will involve jumping to page from previous-position
;;;     BUT we will then need to check that we start at the correct :lastModifiedDate
;;;     Previous order may not be exactly the same because some object(s) will have been updated and now be further down the paging order
;;;

(ns mambu.extensions.data_replication.datarep
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [mambu.extensions.data-replication.file-dwh :as dwh]))

;;; -----------------------------------------------------------------
;;;  Functions for recording where you previously finished.
;;;  Enabling us to resume from a last-position
;;;

;;; last-positions-map = Map of last-position(s) key'ed by object-type
;;; last-position = {<:page-num> <:page-size> <lastModifiedDate>}
(defonce last-positions-map (atom {}))
(declare get-obj-fn)

(defn set-last-position
  ([object-type page-num page-size lastModifiedDate]
   (set-last-position object-type {:page-num page-num :page-size page-size :lastModifiedDate lastModifiedDate}))
  ([object-type last-position]
   (swap! last-positions-map
          (fn [current-state]
            (assoc current-state object-type last-position)))))

(defn determine-last-position [object-type context page]
  (let [lastObj (last page)
        last-moddate (get lastObj (get-obj-fn object-type :last-mod-attr))
        page-num (:page-num context)
        page-size (:page-size context)
        last-position {:page-num page-num :page-size page-size :lastModifiedDate last-moddate}]
    last-position))

(defn get-last-position [object-type]
  (get @last-positions-map object-type))

;; See also determine-start-page below
(defn get-start-page [object-type]
  (let [last-position (get-last-position object-type)
        start-page (:page-num last-position)]
        (if start-page start-page 0)))

;; Save the last-postion for object-type to the DWH
(defn save-last-position-DWH
  ([object-type page-num page-size lastModifiedDate]
   (save-last-position-DWH object-type {:page-num page-num :page-size page-size :lastModifiedDate lastModifiedDate}))
  ([object-type last-position]
   (dwh/save-last-position object-type last-position)))

;; Read the last-postion for object-type from the DWH
(defn read-last-position-DWH [object-type]
  (try (let [last-position (dwh/read-last-position object-type)]
    (set-last-position object-type last-position))
    (catch Exception _ nil)))

(comment ;; Tests
  (set-last-position :client 1 30 "252525")
  
  (reset! last-positions-map {})
  (get-last-position :client)
  (save-last-position-DWH :client 1 30 "252525")
  (save-last-position-DWH :client {:page-num 4 :page-size 30 :lastModifiedDate "5656565"})
  (read-last-position-DWH :client)
;;
  )


;;; -----------------------------------------------------------------
;;;  Next functions allow you to create some activity on customers
;;;

(defn patch-customer [apikey id middleName]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"
                           "apikey" apikey}
                 :query-params {}
                 :body [{"op" "ADD"
                         "path" "middleName"
                         "value" middleName}]}
        url (str "https://europeshowcase.sandbox.mambu.com/api/clients/" id)]
    (api/PATCH url options)))

(defn modify-customer [apikey id stem startNum endNum]
  (doall ;; remember to force your way through the LazySeq that the for returns
   (for [i (range startNum endNum)]
     (do
       (prn "Change name to: " (str stem i))
       (patch-customer apikey id (str stem i))))))

;;; END --------------------------------------------------------------

(defn save-object [obj context]
  (prn "In save-object:")
  (let [object-type (:object-type context)
        last-position (get-last-position object-type)
        last-moddate (:lastModifiedDate last-position)
        obj-last-moddate (get obj (get-obj-fn object-type :last-mod-attr))
        _ (prn "obj-date:" obj-last-moddate "last-moddate" last-moddate)]
    (if  (> (compare obj-last-moddate last-moddate) -1) ;; obj modified after or exactly on last-moddate
      ;; NOTE: If the obj-last-moddate = last-moddate we need to be cautious and update again
      ;; There may have been multiple objects updated with exactly the same last-moddate and we may not have
      ;; seen all of these previously
      (dwh/save-object obj context)
      (prn "Skipping object") ;; We have already processed this object
      ))) 

(defn get-all-clients-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/clients")
                      :method api/GET
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)
                                     "sortBy" "lastModifiedDate:ASC"}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

(defn get-all-deposits-accounts-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/deposits:search")
                      :method api/POST
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)}
                      :body {"filterCriteria" [{"field" "lastModifiedDate" ;; need to filter on something
                                                "operator" "AFTER" ;; so use lastModifiedDate
                                                "value" "1900-01-01"
                                                }]
                             "sortingCriteria" {"field" "lastModifiedDate"
                                                "order" "ASC"}}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

(defn get-all-deposit-trans-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/deposits/transactions:search")
                      :method api/POST
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)}
                      :body {"filterCriteria" [{"field" "creationDate" ;; need to filter on something
                                                "operator" "AFTER" ;; so use lastModifiedDate
                                                "value" "1900-01-01"}]
                             "sortingCriteria" {"field" "creationDate"
                                                "order" "ASC"}}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))



(defn get-all-loan-accounts-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/loans:search")
                      :method api/POST
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)}
                      :body {"filterCriteria" [{"field" "lastModifiedDate" ;; need to filter on something
                                                "operator" "AFTER" ;; so use lastModifiedDate
                                                "value" "1900-01-01"}]
                             "sortingCriteria" {"field" "lastModifiedDate"
                                                "order" "ASC"}}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

(defn get-all-loan-trans-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/loans/transactions:search")
                      :method api/POST
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)}
                      :body {"filterCriteria" [{"field" "creationDate" ;; need to filter on something
                                                "operator" "AFTER" ;; so use lastModifiedDate
                                                "value" "1900-01-01"}]
                             "sortingCriteria" {"field" "creationDate"
                                                "order" "ASC"}}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

(defn get-all-JEs-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/gljournalentries:search")
                      :method api/POST
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)}
                      :body {"filterCriteria" [{"field" "creationDate" ;; need to filter on something
                                                "operator" "AFTER" ;; so use lastModifiedDate
                                                "value" "1900-01-01"}]
                             "sortingCriteria" {"field" "creationDate"
                                                "order" "ASC"}}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

(defn get-installments-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/installments")
                      :method api/GET
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)
                                     "sortBy" "lastModifiedDate:ASC"
                                     "dueFrom" "1900-01-01"
                                     "dueTo" "3000-01-01"
                                     }
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

;; fn-type = (:save-page, :read-page)
(defn get-obj-fn [object_type fn-type]
  (let [func-map
        {:client {:read-page get-all-clients-next-page :last-mod-attr "lastModifiedDate"}
         :deposit_account {:read-page get-all-deposits-accounts-next-page :last-mod-attr "lastModifiedDate"}
         :deposit_trans {:read-page get-all-deposit-trans-next-page :last-mod-attr "creationDate"}
         :loan_account {:read-page get-all-loan-accounts-next-page :last-mod-attr "lastModifiedDate"}
         :loan_trans {:read-page get-all-loan-trans-next-page :last-mod-attr "creationDate"}
         :gl_journal_entry {:read-page get-all-JEs-next-page :last-mod-attr "creationDate"}
         :schedule_install {:read-page get-installments-next-page :last-mod-attr "dueDate"}
         }]
    (get-in func-map [object_type fn-type])))


(defn dec-page-num [page-num]
  (if (< page-num 1) 0 (- page-num 1)))


(defn check-previous-pages [context object_type last-moddate page-num]
  (let
   [context1 ((get-obj-fn object_type :read-page) {:page-size (:page-size context), :page-num page-num})
    page (:last-call context1)
    lastObj (last page)
    page-last-moddate (get lastObj (get-obj-fn object_type :last-mod-attr))]
    (cond
      (= page-num 0) 0
      (nil? page-last-moddate) (dec-page-num page-num)
      (< (compare page-last-moddate last-moddate) 1) (+ page-num 1)
      :else (check-previous-pages context object_type last-moddate (dec-page-num page-num)))))

;; Need to check that the previous page before (read-last-position-DWH ..) has been processed
;; If not then recursively check one before that etc.
;; NOTE: Although we have processed a page before the order of items could have changed - So
;; we are not guarenteed to have processed a page. 
(defn determine-start-page [object_type context]
  (read-last-position-DWH object_type)
  (let [last-position (get-last-position object_type)
        last-moddate (:lastModifiedDate last-position)
        last-page (get-start-page object_type)]
    (if last-moddate
      (check-previous-pages  context object_type last-moddate last-page)
      last-page)))

(defn get-obj-page [object_type context]
  (let [context1 ((get-obj-fn object_type :read-page) context)
        page (:last-call context1)
        last-position (determine-last-position object_type context page)]
    ;; Save the object to the DWH
    (prn "Saving page to DWH XX")
    (doall (map #(save-object % {:object-type object_type}) page))
    ;; Only save the details if last page not empty
    (when (last page) (save-last-position-DWH object_type last-position))
    (set-last-position object_type nil) ;; Avoid skipping checks for other pages
    (prn "**END")
    (count page)))

(defn get-all-objects [object_type context]
  (determine-start-page object_type context) ;; Start from where you left off
  (doall ;; force evaluation of the take-while - which is a LazySeq
   (take-while
    #(> % 0) ;; Get another page if the last one had items in it
    (for [i (iterate inc (get-start-page object_type))]
      (do
        (prn "Getting Page: " i)
        (get-obj-page object_type {:page-size (:page-size context), :page-num i})))))
  (prn "Finished - get-all-clients"))

(defn reset-all []
  (dwh/delete-DWH) ;; Recursively delete the entire DWH
  (set-last-position :client nil))

(comment  ;; Testing sandbox area

  (api/setenv "env5") ;; set to use https://markkershaw.mambu.com

  (reset-all) ;; Delete the DWH and reset other things

  (get-last-position :client)
  (read-last-position-DWH :client)
  ;; Get a single page and save to the DWH
  (get-obj-page :client {:page-size 10, :page-num 1})
  (get-obj-page :deposit_account {:page-size 10, :page-num 0})
  ;; transactions do not have a lastModifiedDate??
  ;; Query this because you can modify a transaction with notes/customFields
  (get-obj-page :deposit_trans {:page-size 10, :page-num 0})
  (get-obj-page :loan_account {:page-size 10, :page-num 0})
  (get-obj-page :loan_trans {:page-size 10, :page-num 0})
  (get-obj-page :gl_journal_entry {:page-size 10, :page-num 0})
  (get-obj-page :schedule_install {:page-size 10, :page-num 0})
  

  ;; Get all objects (of a given type) and save to the DWH
  (get-all-objects :client {:page-size 100})
  (get-all-objects :deposit_account {:page-size 100})
  (get-all-objects :loan_account {:page-size 100})
  (get-all-objects :deposit_trans {:page-size 100})
  (get-all-objects :loan_trans {:page-size 100})
  (get-all-objects :gl_journal_entry {:page-size 100})
  (get-all-objects :schedule_install {:page-size 100})



  ;; testing the determine-start-page functions and helpers
  (check-previous-pages {:page-size 100} :client "2021-08-27T14:12:18+02:00" 1)
  (determine-start-page :client {:page-size 100})
  (get-start-page :client)
  (get-last-position :client)
  (< (compare "2021-08-26T14:12:18+02:00" "2021-08-27T14:12:18+02:00") 1)



;;
  )

