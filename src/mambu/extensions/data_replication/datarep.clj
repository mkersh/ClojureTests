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

(defonce debug-flag (atom false))

(defn debug [ & ks ]
  (when @debug-flag (apply prn ks)))

(defn debug-off [& ks]
  (when (not @debug-flag) (apply println ks)))

(defn setup-debug [debugOn?]
  (reset! debug-flag debugOn?))

;;; -----------------------------------------------------------------
;;;  Functions for recording where you previously finished.
;;;  Enabling us to resume from a last-position
;;;

;;; last-positions-map = Map of last-position(s) key'ed by object-type
;;; last-position = {<:page-num> <:page-size> <lastModifiedDate>}
(defonce last-positions-map (atom {}))
(declare get-obj-fn get-all-objects get-obj-page)

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
        last-moddate (:lastModifiedDate last-position)
        start-page (if last-moddate (:page-num last-position) 0)]
        (if start-page start-page 0)))

;; Save the last-postion for object-type to the DWH
(defn save-last-position-DWH
  ([object-type page-num page-size lastModifiedDate]
   (save-last-position-DWH object-type {:page-num page-num :page-size page-size :lastModifiedDate lastModifiedDate}))
  ([object-type last-position]
   (when (:lastModifiedDate last-position)(dwh/save-last-position object-type last-position))))

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
       (debug "Change name to: " (str stem i))
       (patch-customer apikey id (str stem i))))))

;;; END --------------------------------------------------------------

;;; -----------------------------------------------------------------
;;;  Next functions support keeping a SHA1 cache of objects
;;;  That can be used to determine whether we need to Save or not

(defonce save-object-cache (atom {}))

(defn sha1-str [s]
  (->> (-> "sha1"
           java.security.MessageDigest/getInstance
           (.digest (.getBytes s)))
       (map #(.substring
              (Integer/toString
               (+ (bit-and % 0xff) 0x100) 16) 1))
       (apply str)))


(defn make_sha1 [obj]
  (let [s (dwh/get-object-str obj)
        signature (sha1-str s)]
    signature))

(defn remove-fields-from-obj [obj remove-fields]
  (reduce #(dissoc %1 %2) obj remove-fields))

(defn add-to-cache [object_type obj remove-fields]
  (let [obj1 (remove-fields-from-obj obj remove-fields)
        cache-map @save-object-cache
        cache-map-objtype (or (get cache-map object_type) {})
        sha1Str (make_sha1 obj1)
        cache-map-objtype2 (assoc cache-map-objtype sha1Str true)]
    (reset! save-object-cache (assoc cache-map object_type cache-map-objtype2))))

(defn in-cache? [object_type obj remove-fields]
  (let [obj1 (remove-fields-from-obj obj remove-fields)
        cache-map @save-object-cache
        cache-map-objtype (or (get cache-map object_type) {})
        sha1Str (make_sha1 obj1)]
    (get cache-map-objtype sha1Str)))

(defn save-cache [object_type]
  (let [caching_enabled? (get-obj-fn object_type :use-caching)]
    (when caching_enabled? (dwh/save-cache caching_enabled? (get @save-object-cache caching_enabled?)))))

(defn load-cache [object_type]
  (let [caching_enabled? (get-obj-fn object_type :use-caching)]
    (when caching_enabled?
      (let [cache-map-objtype (try (dwh/read-cache caching_enabled?)
                                   (catch Exception _ {}))]
        (reset! save-object-cache (assoc @save-object-cache caching_enabled? cache-map-objtype))))))

(defn clear-cache []
  (reset! save-object-cache {}))

(comment ;; Some tests
(clear-cache)
(add-to-cache :schedule_install {:f1 "v2" :f2 "v3"} [:f2])
(in-cache? :schedule_install {:f1 "v2" :f2 "v3"} [:f2])
(get @save-object-cache :schedule_install)
(save-cache :schedule_install)
(load-cache :schedule_install)
;;
)

;;; END --------------------------------------------------------------


(defn save-object [obj context]
  (debug "In save-object:")
  (let [object-type (:object-type context)
        context1 (assoc context :get-file-path-fn (get-obj-fn object-type :get-file-path-fn))
        last-position (get-last-position object-type)
        last-moddate (:lastModifiedDate last-position)
        obj-last-moddate (get obj (get-obj-fn object-type :last-mod-attr))
        caching_enabled? (get-obj-fn object-type :use-caching)
        cache-remove-fields (get-obj-fn object-type :cache-remove-fields)
        trigger-other-fn (get-obj-fn object-type :trigger-other)
        _ (debug "obj-date:" obj-last-moddate "last-moddate" last-moddate)]
    (if  (> (compare obj-last-moddate last-moddate) -1) ;; obj modified after or exactly on last-moddate
      ;; NOTE: If the obj-last-moddate = last-moddate we need to be cautious and update again
      ;; There may have been multiple objects updated with exactly the same last-moddate and we may not have
      ;; seen all of these previously
      (if caching_enabled?
        ;; If caching enabled check to see if we have already saved this obj
        ;; Expecting caching_enabled? to contain the object-type to use as the cache
        ;; NOTE: Most of the time this will just be object-type but want it is not for :schedule_install2
        (if (in-cache? caching_enabled? obj cache-remove-fields)
          ;; Already in cache do not save
          (debug "Skipping object - In Cache")
          ;; Else save the object
          (do
            (dwh/save-object obj context1)
            (add-to-cache object-type obj cache-remove-fields)
            (when trigger-other-fn (trigger-other-fn obj))))
        ;; else just save to DWH
        (do
          (dwh/save-object obj context1)
          (when trigger-other-fn (trigger-other-fn obj))))
      ;; else part of compare if
      (debug "Skipping object") ;; We have already processed this object
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

(defn get-all-groups-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/groups")
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



(defn get-loan-account [context]
  (let [api-call (fn [context0]
                     {:url (str "{{*env*}}/loans/" (:accid context))
                      :method api/GET
                      :query-params {"detailsLevel" "FULL"}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}})]
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

(defn get-loan-schedule [context]
  (let [api-call (fn [context0]
                   {:url (str "{{*env*}}/loans/" (:accid context0) "/schedule")
                    :method api/GET
                    :query-params {"detailsLevel" "FULL"}
                    :headers {"Accept" "application/vnd.mambu.v2+json"
                              "Content-Type" "application/json"}})]
    (steps/apply-api api-call context)))

;; Next function is an alternative to get-installments-next-page
;; It uses the passed (:accid context) to return the instalments for that specific loan
;; and returns the results in the same format as get-installments-next-page.
;; This allows us to save in the DWH in the same way
(defn get-schedule-next-page [context]
  (let [page-num (int (:page-num context))
        acc-schedule (if (< page-num 1)(get-loan-schedule context) nil) ;; context needs :accid passed
        instalments (get-in acc-schedule [:last-call "installments"])]
    ;; need to wrap result back into a map with key :last-call because that is what get-obj-page expects
    (assoc {} :last-call instalments)))

(defonce UPDATE-LOAN-ACC-SCHEDULE-PER-ACCOUNT (atom false))
(defn trigger-schedule-update [loan-obj]
  (when @UPDATE-LOAN-ACC-SCHEDULE-PER-ACCOUNT
    (prn "Sync schedule for " (get loan-obj "id"))
    (get-all-objects :schedule_install2 {:accid (get loan-obj "id")})))
(defn set-update-loan-acc-schedule-per-account [val]
  (reset! UPDATE-LOAN-ACC-SCHEDULE-PER-ACCOUNT val))

;; The following only works for bringing down the complete set of installments
;; It does NOT work for incremental updates because there is nothing to sort on
(defn get-installments-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/installments")
                      :method api/GET
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)
                                     "dueFrom" "1900-01-01"
                                     "dueTo" "3000-01-01"
                                     }
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

  (defn get-gl-accounts-next-page [context]
    (let [api-call (fn [context0]
                     (let [page-size (:page-size context0)
                           offset (* (:page-num context0) page-size)]
                       {:url (str "{{*env*}}/glaccounts")
                        :method api/GET
                        :query-params {"detailsLevel" "FULL"
                                       "paginationDetails" "ON"
                                       "offset" offset "limit" (:page-size context0)
                                       "type" (:gl-type context)
                                       "balanceExcluded" true}
                        :headers {"Accept" "application/vnd.mambu.v2+json"
                                  "Content-Type" "application/json"}}))]
      (steps/apply-api api-call context)))

  (defn get-loan-products-next-page [context]
    (let [api-call (fn [context0]
                     (let [page-size (:page-size context0)
                           offset (* (:page-num context0) page-size)]
                       {:url (str "{{*env*}}/loanproducts")
                        :method api/GET
                        :query-params {"detailsLevel" "FULL"
                                       "paginationDetails" "ON"
                                       "offset" offset "limit" (:page-size context0)
                                       "sortBy" "lastModifiedDate:ASC"
                                       }
                        :headers {"Accept" "application/vnd.mambu.v2+json"
                                  "Content-Type" "application/json"}}))]
      (steps/apply-api api-call context)))

(defn get-deposit-products-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/depositproducts")
                      :method api/GET
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)
                                     "sortBy" "lastModifiedDate:ASC"}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

(defn get-branches-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/branches")
                      :method api/GET
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)
                                     "sortBy" "lastModifiedDate:ASC"}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

(defn get-centres-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/centres")
                      :method api/GET
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)
                                     "sortBy" "lastModifiedDate:ASC"}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

(defn get-users-next-page [context]
  (let [api-call (fn [context0]
                   (let [page-size (:page-size context0)
                         offset (* (:page-num context0) page-size)]
                     {:url (str "{{*env*}}/users")
                      :method api/GET
                      :query-params {"detailsLevel" "FULL"
                                     "paginationDetails" "ON"
                                     "offset" offset "limit" (:page-size context0)}
                      :headers {"Accept" "application/vnd.mambu.v2+json"
                                "Content-Type" "application/json"}}))]
    (steps/apply-api api-call context)))

;; Save installments under their parent account sub-folder
;; NOTE: Was using "number" to order but this does not work - read API to see why
;;       Now using dueDate to sort
(defn install-get-file-path [root-dir object-type object]
  (str root-dir (symbol object-type) "/" (get object "parentAccountKey") "/" (subs (get object "dueDate") 0 10) "-"(get object "encodedKey") ".edn"))

;; Here's the one for :schedule_install2
(defn install-get-file-path2 [root-dir _ object]
  (str root-dir (symbol :schedule_install) "/" (get object "parentAccountKey") "/" (subs (get object "dueDate") 0 10) "-" (get object "encodedKey") ".edn"))

(defn get-obj-fn [object_type fn-type]
  (let [func-map
        {:client {:read-page get-all-clients-next-page :last-mod-attr "lastModifiedDate"}
         :group {:read-page get-all-groups-next-page :last-mod-attr "lastModifiedDate"}
         :deposit_account {:read-page get-all-deposits-accounts-next-page :last-mod-attr "lastModifiedDate"}
         :deposit_trans {:read-page get-all-deposit-trans-next-page :last-mod-attr "creationDate"}
         :loan_account {:read-page get-all-loan-accounts-next-page :last-mod-attr "lastModifiedDate"
                        :trigger-other trigger-schedule-update}
         :loan_trans {:read-page get-all-loan-trans-next-page :last-mod-attr "creationDate"}
         :gl_journal_entry {:read-page get-all-JEs-next-page :last-mod-attr "creationDate"}
         :gl_account {:read-page get-gl-accounts-next-page :last-mod-attr "noDate"}
         :schedule_install {:read-page get-installments-next-page
                            :last-mod-attr "noDate" :get-file-path-fn install-get-file-path
                            :use-caching :schedule_install :cache-remove-fields ["number"]}
        :schedule_install2 {:read-page get-schedule-next-page
                           :last-mod-attr "noDate" :get-file-path-fn install-get-file-path2
                           ;; when comparing cache ignore "feeDetails" because :schedule_install does not return this 
                           :use-caching :schedule_install :cache-remove-fields ["number" "feeDetails"]
                           }
        :loan_product {:read-page get-loan-products-next-page :last-mod-attr "lastModifiedDate"}
        :deposit_product {:read-page get-deposit-products-next-page :last-mod-attr "lastModifiedDate"}
        :branch {:read-page get-branches-next-page :last-mod-attr "lastModifiedDate"}
        :centre {:read-page get-centres-next-page :last-mod-attr "lastModifiedDate"}
        :user {:read-page get-users-next-page :last-mod-attr "lastModifiedDate"}
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
      0)))

(defn get-obj-page [object_type context]
  (let [context1 ((get-obj-fn object_type :read-page) context)
        page (:last-call context1)
        last-position (determine-last-position object_type context page)]
    ;; Save the object to the DWH
    (debug "Saving page to DWH")
    (debug-off ".")
    (doall (map #(save-object % {:object-type object_type}) page))
    ;; Only save the details if last page not empty
    (when (last page) (save-last-position-DWH object_type last-position))
    (set-last-position object_type nil) ;; Avoid skipping checks for other pages
    (debug "**END")
    (count page)))

(defn get-all-objects [object_type context]
  (load-cache object_type)
  (determine-start-page object_type context) ;; Start from where you left off
  (doall ;; force evaluation of the take-while - which is a LazySeq
   (take-while
    #(> % 0) ;; Get another page if the last one had items in it
    (for [i (iterate inc (get-start-page object_type))]
      (do
        (debug "Getting Page: " i)
        (get-obj-page object_type  (merge context {:page-size (:page-size context), :page-num i}))))))
  (save-cache object_type)
  (debug "Finished - get-all-objects"))

(defn reset-all []
  (dwh/delete-DWH) ;; Recursively delete the entire DWH
  (set-last-position :client nil))

(defn resync-dwh
  ([] (resync-dwh true))
  ([full-sync-installments]
   ;; define whether to perform per account schedule updates whenever a loan-account has changed
   ;; NOTE: We want to do this when full-sync-installments=false
   (set-update-loan-acc-schedule-per-account (not full-sync-installments))
   (prn "Sync Branches")
   (get-all-objects :branch {:page-size 100})
   (prn "Sync Centres")
   (get-all-objects :centre {:page-size 100})
   (prn "Sync Clients")
   (get-all-objects :client {:page-size 100})
   (prn "Sync Groups")
   (get-all-objects :group {:page-size 100})
   (prn "Sync Deposit Accounts")
   (get-all-objects :deposit_account {:page-size 100})
   (prn "Sync Loan Accounts")
   (get-all-objects :loan_account {:page-size 100})
   (prn "Sync Deposit Transactions")
   (get-all-objects :deposit_trans {:page-size 100})
   (prn "Sync Loan Transactions")
   (get-all-objects :loan_trans {:page-size 100})
   (prn "Sync Journal Entries")
   (get-all-objects :gl_journal_entry {:page-size 100})
   (prn "Sync GL Accounts")
   (get-all-objects :gl_account {:gl-type "ASSET" :page-size 100})
   (get-all-objects :gl_account {:gl-type "LIABILITY" :page-size 100})
   (get-all-objects :gl_account {:gl-type "EQUITY" :page-size 100})
   (get-all-objects :gl_account {:gl-type "INCOME" :page-size 100})
   (get-all-objects :gl_account {:gl-type "EXPENSE" :page-size 100})
   (when full-sync-installments
     (prn "Sync Installments (Full)")
     (get-all-objects :schedule_install {:page-size 1000}))
   (prn "Sync Loan Products")
   (get-all-objects :loan_product {:page-size 100})
   (prn "Sync Deposit Products")
   (get-all-objects :deposit_product {:page-size 100})
   (prn "Sync Users")
   (get-all-objects :user {:page-size 100})))  

(defn SETENV [env]
  (api/setenv env)
  (dwh/set-dwh-root-dir))

(comment  ;; Testing sandbox area

  (SETENV "env5") ;; set to use https://markkershaw.mambu.com
  (setup-debug false) ;; Turn off debug messages
  (setup-debug true) ;; Turn on debug messages

  (reset-all) ;; Delete the DWH and reset other things. NOTE: only deletes for the current (api/get-env-domain)

  ;; ******************************************
  ;; This is the function you will use the most to:
  ;; Resync the DWH will all updates from Mambu

  (time (resync-dwh false)) ;; Bypass installment update, which takes most time. 

  (time (resync-dwh)) ;; Full resync including full installments update. 


  ;; -----------------------------------------------------------------------
  ;; Lower level test calls

  (get-last-position :client)
  (read-last-position-DWH :client)
  ;; Get a single page and save to the DWH
  (get-obj-page :client {:page-size 10, :page-num 1})
  (get-obj-page :group {:page-size 10, :page-num 1})
  (get-obj-page :deposit_account {:page-size 10, :page-num 0})
  ;; transactions do not have a lastModifiedDate??
  ;; Query this because you can modify a transaction with notes/customFields
  (get-obj-page :deposit_trans {:page-size 10, :page-num 0})
  (get-obj-page :loan_account {:page-size 10, :page-num 0})
  (get-obj-page :loan_trans {:page-size 10, :page-num 0})
  (get-obj-page :gl_journal_entry {:page-size 10, :page-num 0})
  (get-obj-page :schedule_install {:page-size 10, :page-num 0})
  (get-obj-page :gl_account {:gl-type "ASSET" :page-size 100, :page-num 0})
  (get-obj-page :loan_product {:page-size 10, :page-num 1})
  (get-obj-page :deposit_product {:page-size 10, :page-num 1})


  ;; Get all objects (of a given type) and save to the DWH
  (get-all-objects :branch {:page-size 100})
  (get-all-objects :centre {:page-size 100})
  (get-all-objects :client {:page-size 100})
  (get-all-objects :group {:page-size 100})
  (get-all-objects :deposit_account {:page-size 100})
  (get-all-objects :loan_account {:page-size 100})
  (get-all-objects :deposit_trans {:page-size 100})
  (get-all-objects :loan_trans {:page-size 100})
  (get-all-objects :gl_journal_entry {:page-size 100})
  (get-all-objects :gl_account {:gl-type "ASSET" :page-size 100})
  (get-all-objects :gl_account {:gl-type "LIABILITY" :page-size 100})
  (get-all-objects :gl_account {:gl-type "EQUITY" :page-size 100})
  (get-all-objects :gl_account {:gl-type "INCOME" :page-size 100})
  (get-all-objects :gl_account {:gl-type "EXPENSE" :page-size 100})
  (get-all-objects :schedule_install {:page-size 1000})
  ;; Per account schedule updates - alternative to slow :schedule_install
  (get-all-objects :schedule_install2 {:accid "8a19a3d779e6f12c0179ec07b9d45e90"})
  (determine-start-page :schedule_install2 {:accid "8a19a3d779e6f12c0179ec07b9d45e90"})
  (get-obj-page :schedule_install2 {:accid "8a19a3d779e6f12c0179ec07b9d45e90" :page-size 10, :page-num 0})

  (get-all-objects :loan_product {:page-size 100})
  (get-all-objects :deposit_product {:page-size 100})
  (get-all-objects :user {:page-size 100})

  (api/PRINT (get-loan-account {:accid "8a19a3d779e6f12c0179ec07b9d45e90"}))
  (api/PRINT (get-loan-schedule {:accid "8a19a3d779e6f12c0179ec07b9d45e90"}))
  (api/PRINT (get-schedule-next-page {:accid "8a19a3d779e6f12c0179ec07b9d45e90" :page-num 1}))



  ;; testing the determine-start-page functions and helpers
  (check-previous-pages {:page-size 100} :client "2021-08-27T14:12:18+02:00" 1)
  (determine-start-page :schedule_install {:page-size 100})
  (get-start-page :schedule_install)

  (get-start-page :client)
  (get-last-position :client)
  (< (compare "2021-08-26T14:12:18+02:00" "2021-08-27T14:12:18+02:00") 1)

;; Searching for stuff in th DWH
  (dwh/find-all-matches-DWH "Demo")

;;
  )

