;; Examples of how to support AccountAlias(s) in Mambu using Custom Fields

(ns http.api.mambu.demo.custom-fields-account-aliases
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))


(defn get-account-api [context]
  {:url (str "{{*env*}}/deposits/" (:accountId context))
   :method api/GET
   :keepalive 30000 ;; keep-alive for 30 secs, -1 to turn it off
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"
             }})


(defn new-account-alias [type sort-code acc-num]
  {"Type" type
   "AccSortCodeBranch" sort-code
   "AccNum" acc-num})

(defn patch-account-aliases-api [context]
  {:url (str "{{*env*}}/deposits/" (:accountId context))
   :method api/PATCH
   :body [;; Patching the AccountAlias DataFieldSet
          {"op" "replace"
           "path" "_AccountAlias"
           "value" (:value context)}]
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; Remove any nils from a list
(defn remove-nils [lst]
  (filterv (comp not nil?) lst))

;; Searching for an account given an AccountAlias
(defn alias-search-api [context]
  {:url (str "{{*env*}}/deposits:search")
   :method api/POST
   :query-params {"detailsLevel" "FULL"}
   :body {"filterCriteria" (remove-nils
                            [(if (:accountNum context)
                               {"field" "_AccountAlias.AccNum"
                                "operator" "EQUALS"
                                "value" (:accountNum context)}
                               nil)
                             (if (:accountNumPartial context)
                               {"field" "_AccountAlias.AccNum"
                                "operator" "STARTS_WITH"
                                "value" (:accountNumPartial context)}
                               nil)
                             (if (:sortCode context)
                               {"field" "_AccountAlias.AccSortCodeBranch"
                                "operator" "EQUALS"
                                "value" (:sortCode context)}
                               nil)
                             (if (:type context)
                               {"field" "_AccountAlias.Type"
                                "operator" "EQUALS"
                                "value" (:type context)}
                               nil)])}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(comment
  ;; Test environment to use
  (api/setenv "env1")

  (def account-aliases [(new-account-alias "UK" "123456" "12345678")
                        (new-account-alias "UK" "123456" "12345678_3")])


  (time (steps/apply-api get-account-api {:accountId "FMHJ311"}))
  (steps/apply-api patch-account-aliases-api {:accountId "FMHJ311" :value account-aliases})

  ;; alias-search-api provides different ways to search against the AccountAlias
  (steps/apply-api alias-search-api {:accountNum "12345678_3"})
  (steps/apply-api alias-search-api {:accountNumPartial "1234"})
  (steps/apply-api alias-search-api {:type "UK"})
  (steps/apply-api alias-search-api {:sortCode "123456"})
  (steps/apply-api alias-search-api {:type "UK" :sortCode "123456" :accountNum "12345678"})

;;
  )