(ns http.api.mambu.demo.training.customer
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

(defn get-cust-api [context]
  {:url (str "{{*env*}}/clients/" (:custid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-allcusts-api [context]
  {:url (str "{{*env*}}/clients")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(comment 
(api/setenv "env6")

(steps/apply-api get-allcusts-api {})

(steps/apply-api get-cust-api {:custid "335356139"})
;;
)