;; Example/test using the https://api.mambu.com/#mambu-api-v2-database-backup endpoints
(ns http.api.mambu.examples.db-backup
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

;; Mambu API endpoint for triggering a NEW database download
(defn trigger-db-backup-api [context]
  {:url (str "{{*env*}}/database/backup")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   ;; set the callback parameter to the webhook URL to be called when the download has completed
   ;; NOTE: This callback parameter is optional but you are advised to use it
   :body  {"callback" (:callback context)}})

;; Mambu API endpoint for downloading a previously triggered database download
(defn download-db-backup-api [_context]
  ;; TIP: https://api.mambu.com/#database-backup-downloadbackup implies you can provide a {databaseBackupVersion}
  ;;      but if you read the doc close only allowed value (currently) is LATEST
  {:url (str "{{*env*}}/database/backup/LATEST")
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+zip"} ;; TIP: Make sure this content-type is correct, its different from other Mambu V2 API endpoints!!
   :query-params {}})

(comment
  (api/setenv "env2")

  ;; [1] Trigger a new DB download and inform https://mpo-multitenant.mambuonline.com/i485779274/process/90641/diagram/61f250c8c5654b2814002292/archive
  ;; when this has completed.
  ;; My callback is an MPO process
  (api/PRINT (:last-call (steps/apply-api trigger-db-backup-api {:callback "https://mpo-multitenant.mambuonline.com/api/1/json/public/90641/0fc5b1943e1b43cbbd149064692ffa99f5549c38"})))
  
  ;; [2] Once [1] above has completed you can then download the ZIP file using
  (steps/apply-api download-db-backup-api {})

  )
