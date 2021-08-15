(ns http.api.mambu.examples.custom-fields
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))

(defn getall-custom-field-sets [_]
  {:url (str "{{*env*}}/customfieldsets")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn getall-custom-field-sets-v10 [_]
  {:url (str "{{*env*}}/customfieldsets")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Content-Type" "application/json"}})

(defn getall-custom-fields-by-setid [context]
  {:url (str "{{*env*}}/customfieldsets/" (:cfsid context) "/customfields")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-custom-field [context]
  {:url (str "{{*env*}}/customfields/" (:cfid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})



(comment
 (api/setenv "env2")
 (steps/apply-api getall-custom-field-sets {})
 (steps/apply-api getall-custom-field-sets-v10 {})
 (steps/apply-api getall-custom-fields-by-setid {:cfsid "_SOM1"})
 (steps/apply-api get-custom-field {:cfid "FromAccountID"})

)