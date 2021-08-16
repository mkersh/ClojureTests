;;; Examples/tests for how to efficiently replicate data/objects from Mambu to a data-lake/DWH
(ns http.api.mambu.datarepl.datarep
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

(defn getTimer []
(. System (nanoTime)))

(defn showTimeDiff [title startTimer]
  (let [curr-time (getTimer)
        time-diff (/ (- curr-time startTimer) 1000000.0)]
        (prn title time-diff )
        time-diff))

;; Sorting on lastModifiedDate ASC will allow us to have a shortcut to resume where we left off
;; If will not be 100% accurate because the lastModifiedDate on certain records will have changed
;;
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

(defn print-client-page [context]
  (let [startTimer (getTimer)
        context1 (get-all-clients-next-page context)
        timeDiff (showTimeDiff "API call took (ms):" startTimer)
        page (:last-call context1)]
    (api/PRINT (api/extract-attrs ["id" "lastName" "creationDate" "lastModifiedDate"] page))
    (prn "API call took (ms):" timeDiff)
    (showTimeDiff "Time including print (ms):" startTimer)
    (count page)))

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


(comment 
(api/setenv "env2") ;; set to use https://markkershaw.mambu.com

(binding [api/*ENV* "env1"]
  (print-client-page {:page-size 3, :page-num 0}))

;; api/setenv-local is the easier way to set a binding for *ENV*
(api/setenv-local "env5"
    (print-client-page {:page-size 3, :page-num 0})
    (print-client-page {:page-size 3, :page-num 0}))
  
(print-client-page {:page-size 3, :page-num 0})

;; Test getting a page of customer objects
(take-while
 #(> % 0) ;; Get another page if the last one had items in it
 (for [i (range)]
   (do
     (prn "Getting Page: " i)
     (print-client-page {:page-size 3, :page-num i}))))




;; Test that showTimeDiff is accurate
(let [startTimer (getTimer)]
(Thread/sleep 2000)
(showTimeDiff "Sleep Timer (ms):" startTimer)
)

(take 100 (range))
(+ 1 2)
;;
)

