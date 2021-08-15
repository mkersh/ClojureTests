(ns http.api.mambu.demo.custom-fields-client
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))


(defn get-client-api [context]
  {:url (str "{{*env*}}/clients/" (:clientId context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn new-credit-limit [id limit-amount]
  {"globalLimitStartDate" "2021-06-30"
   "globalLimitID" id
   "globalLimitEndDate" "30-06-2030"
   "globalLimitAllocated" limit-amount})

(defn patch-customer-api [context]
  {:url (str "{{*env*}}/clients/" (:clientId context))
   :method api/PATCH
   :body [{"op" "add"
           "path" "/_MKExtraCustomer/MyCustomerField1"
           "value" "Oh yes"}
           
           ;; Patching a grouped DataFieldSet
           {"op" "replace"
            "path" "/_GlobalCreditLimits"
            "value" (:value context)}
           ]
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; Return a function to use for mapping through a "/_GlobalCreditLimits" array 
(defn reduce-fn [parentLimitID amount]
  (fn [item]
    (if (= (get item "globalLimitID") parentLimitID)
      (let [old-amount (Float/parseFloat (get item "globalLimitAllocated"))
            new-amount (- old-amount amount)]
        (assoc item "globalLimitAllocated" new-amount))
      ;; else
      item)))

(defn reduce-limit-allocated [clientid parentLimitID amount]
  (let [client-details (:client_get (steps/apply-api get-client-api {:clientId clientid} :client_get))
        limits-array (get client-details "_GlobalCreditLimits")
        new-limits-array (map (reduce-fn parentLimitID amount) limits-array)]
    (steps/apply-api patch-customer-api {:clientId "501284815" :value new-limits-array} :client_patch)))

(comment
  (api/setenv "env1")

  (def initialLimits
    [(new-credit-limit "LimitID1" 23300)
     (new-credit-limit "LimitID2" 300)])

  (steps/apply-api get-client-api {:clientId "501284815"} :client_get)
  (steps/apply-api patch-customer-api {:clientId "501284815" :value initialLimits} :client_patch)

  ;; This next function reduces the limit for a customer–
  (reduce-limit-allocated "501284815" "LimitID1" 11000)
;;
  )