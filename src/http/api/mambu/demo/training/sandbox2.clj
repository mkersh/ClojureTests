(ns http.api.mambu.demo.training.sandbox2
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

(defn get-all-customers-api [_context]
  {:url (str "{{*env*}}/clients")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(comment
  (api/setenv "env13")
  
  (api/PRINT (:last-call (steps/apply-api get-all-customers-api {})))


;;
)