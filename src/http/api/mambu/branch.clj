(ns http.api.mambu.branch
  (:require [http.api.json_helper :as api]))

(defn list-branches []
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" "FULL"}}
        url "{{*env*}}/branches"]
    (api/PRINT (api/GET url options))))

(defn get-branch [id]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" "FULL"}}
        url (str "{{*env*}}/branches/" id)]
    (api/PRINT (api/GET url options))))

(defn patch-branch [id]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body [{"op" "ADD"
                         "path" "emailAddress"
                         "value" "hh@gmail.com"}]}
        url (str "{{*env*}}/branches/" id)]
    (api/PRINT (api/PATCH url options))))

; Test in your REPL: Select line to run ctl+alt+c <space>
; Use api/find-path and api/extract-attrs to navigate through results
(comment
  (time (list-branches))
  (time (get-branch "LargeAccountTest"))
  
  ;; patching is not allowed yet, which means that you need to use API 1.0 
  ;; to modify branch custom fields
  (time (patch-branch "LargeAccountTest"))
  
  )