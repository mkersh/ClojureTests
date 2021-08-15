;;; Examples of using Mambu's config-as-code API endpoints
(ns http.api.mambu.demo.CasC
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.java.io :as io]
            ))

(defn get-customfields-yaml-template-api [context]
  {:url (str "{{*env*}}/configuration/customfields/template.yaml")
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             }})

(defn get-customfields-yaml-api [context]
  {:url (str "{{*env*}}/configuration/customfields.yaml")
  :query-params {"availableFor" (:availableFor context)}
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             "Content-Type" "application/yaml"}})

(defn put-customfields-yaml-api [context]
  {:url (str "{{*env*}}/configuration/customfields.yaml")
   :method api/PUT
   :body (:yaml-str context)
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             "Content-Type" "application/yaml"}})

   

(defn file->bytes [xin]
  (with-open [xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    ;;(.toByteArray xout)
    (.toString xout "UTF-8")))

(defn write-yaml-file [xin fpath1]
  (let
   [yaml-file-stream xin
    yaml-str (file->bytes yaml-file-stream)
    _ (spit fpath1 yaml-str)]
    yaml-str))

(comment
  ;; Test environment to use
  (api/setenv "env1")


(write-yaml-file (:last-call (steps/apply-api get-customfields-yaml-template-api {}))
                 "CasC-customFields-template.yaml")

(write-yaml-file (:last-call (steps/apply-api get-customfields-yaml-api {}))
                 "CasC-customFields-export-all.yaml")

;; Filter for a specific entity DEPOSIT_ACCOUNT LOAN_ACCOUNT CLIENT GROUP
(write-yaml-file (:last-call (steps/apply-api get-customfields-yaml-api {:availableFor "DEPOSIT_ACCOUNT"})) 
                 "CasC-customFields-export-part.yaml")
  
;; Update CustomFields from a YAML fiel
(steps/apply-api put-customfields-yaml-api {:yaml-str (slurp "CasC-customFields-import.yaml")})

(slurp "CasC-customFields-export-all.yaml")
  ;;
)