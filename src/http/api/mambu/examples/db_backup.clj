;; Example/test using the https://api.mambu.com/#mambu-api-v2-database-backup endpoints
(ns http.api.mambu.examples.db-backup
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

(defn trigger-db-backup-api [context]
  {:url (str "{{*env*}}/database/backup")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body  {"callback" (:callback context)}})

(defn download-db-backup-api [context]
  {:url (str "{{*env*}}/database/backup/LATEST" (:backup-id context))
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+zip"} ;; TIP: Make sure this content-type is correct
   :query-params {}})


(comment
  (api/setenv "env2")

  (api/PRINT (:last-call (steps/apply-api trigger-db-backup-api {:callback "https://mpo-multitenant.mambuonline.com/api/1/json/public/90641/0fc5b1943e1b43cbbd149064692ffa99f5549c38"})))
  (steps/apply-api download-db-backup-api {})

  )
