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

(defn set-last-position
  ([object-type page-num page-size lastModifiedDate]
   (set-last-position object-type {:page-num page-num :page-size page-size :lastModifiedDate lastModifiedDate}))
  ([object-type last-position]
   (swap! last-positions-map
          (fn [current-state]
            (assoc current-state object-type last-position)))))

(defn determine-last-position [context page]
  (let [lastObj (last page)
        last-moddate (get lastObj "lastModifiedDate")
        page-num (:page-num context)
        page-size (:page-size context)
        last-position {:page-num page-num :page-size page-size :lastModifiedDate last-moddate}]
    last-position))

(defn get-last-position [object-type]
  (get @last-positions-map object-type))

;; NOT good enough yet
;; need to make sure that this page contain our :lastModifiedDate
;; else we need to go back a page
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

(defn save-object [obj context]
  (prn "In save-object:")
  (let [object-type (:object-type context)
        last-position (get-last-position object-type)
        last-moddate (:lastModifiedDate last-position)
        obj-last-moddate (get obj "lastModifiedDate")
        _ (prn "obj-date:" obj-last-moddate "last-moddate" last-moddate)]
    (if  (> (compare obj-last-moddate last-moddate) -1) ;; obj modified after or exactly on last-moddate
      ;; NOTE: If the obj-last-moddate = last-moddate we need to be cautious and update again
      ;; There may have been multiple objects updated with exactly the same last-moddate and we may not have
      ;; seen all of these previously
      (dwh/save-object obj context)
      (prn "Skipping object") ;; We have already processed this object
      ))) 

(defn get-client-page [context]
  (let [context1 (get-all-clients-next-page context)
        page (:last-call context1)
        last-position (determine-last-position context page)]
    ;; Save the object to the DWH
    (prn "Saving page to DWH XX")
    (doall (map #(save-object % {:object-type :client}) page))
    (save-last-position-DWH :client last-position)
    (set-last-position :client nil) ;; Avoid skipping checks for other pages
    (prn "**END")
    (count page)))

;; Need to improve determine-start-page 
;; There is the possibility that we could missing object updates
;; This could happen if updates are made to objects that we have previously processed
;; Need to check that the previous page before (read-last-position-DWH ..) has been processed
;; If not then recursively check one before that etc.
(defn determine-start-page [object_type context]
  
  (let [last-page](get-start-page object_type)))


(defn get-all-objects [object_type context]
  (determine-start-page object_type context) ;; Start from where you left off
  (doall ;; force evaluation of the take-while - which is a LazySeq
   (take-while
    #(> % 0) ;; Get another page if the last one had items in it
    (for [i (iterate inc (get-start-page object_type))]
      (do
        (prn "Getting Page: " i)
        (get-client-page {:page-size (:page-size context), :page-num i})))))
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
  (get-client-page {:page-size 3, :page-num 0})

  ;; Get all the clients and save to the DWH
  (get-all-objects :client {:page-size 1000})
  
;;
  )

