(ns mambu.extensions.product-factory.mod-products.update-product
 (:require [http.api.json_helper :as api]
           [http.api.api_pipe :as steps]
           [clojure.data.json :as json])
)

(defn get-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn update-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/PUT
   :body (:body context)
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(defonce prod-details (atom {}))

(comment
  (api/setenv "env2") ;; MK prod  
  
  ;; Get product details
  (reset! prod-details (:last-call (steps/apply-api get-loan-product-api {:prodid "755_ex"})))
  (get @prod-details "state")
  @prod-details

  ;; update product details
  (reset! prod-details (assoc @prod-details "notes" "updated notes"))
  (reset! prod-details (assoc @prod-details "state" "INACTIVE"))
  (reset! prod-details (assoc @prod-details "state" "ACTIVE"))
  (steps/apply-api update-loan-product-api {:prodid "755_ex" :body @prod-details})

  ;;
  )