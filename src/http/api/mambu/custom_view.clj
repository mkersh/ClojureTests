(ns http.api.mambu.customer
  (:require [http.api.json_helper :as api]))

 (defn list-views-v1 [& opt-overrides]
   (let [moreOpts (first opt-overrides)
         env (or (:env moreOpts) "env1")
         optdefs {:basic-auth (api/get-auth env)
                  :headers {}
                  :query-params {}}
         options (merge optdefs moreOpts)
         url (str "{{" env "}}/users/apiUser/views")]
     (prn options)
     (api/PRINT (api/GET url options)))) 

;; For valid values of type see: https://support.mambu.com/docs/en/custom-views-api
(defn get-views-v1 [type viewfilter & opt-overrides]
  (let [moreOpts (first opt-overrides)
        env (or (:env moreOpts) "env1")
        ;; FULL_DETAILS, BASIC, SUMMARY
        detailLevel (or (:details-level moreOpts) "FULL_DETAILS") 
        limit (or (:limit moreOpts) 20)
        offset (or (:offset moreOpts) 0)
        optdefs {:basic-auth (api/get-auth env)
                 :headers {}  ;; detailsLevel
                 :query-params {"resultType" detailLevel, "limit" limit, "offset" offset, "viewfilter" viewfilter}}
        options (merge optdefs moreOpts)
        url (str "{{" env "}}/" type)]
    (prn options)
    (api/PRINT (api/GET url options))))

(comment
  (list-views-v1)
  (get-views-v1 "clients" "8a81868e6808ec4501681508bcd530df")
  (get-views-v1 "clients" "8a818e74677a2e9201677ec2baf136ba")
  
  (get-views-v1 "clients" "8a81868e6808ec4501681508bcd530df" {:limit 500 :offset 13})
  
  (get-views-v1 "savings" "8a818684701c9a660170339c93f93004")
  (get-views-v1 "savings" "8a818684701c9a660170339c93f93004" {:limit 500})

  (list-views-v1 {:env "env2"})
  (get-views-v1 "savings" "8a818e2e74c022de0174c0f63db234c8" {:env "env2"})

  )

