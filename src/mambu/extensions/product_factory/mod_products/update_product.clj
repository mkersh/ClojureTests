(ns mambu.extensions.product-factory.mod-products.update-product
 (:require [http.api.json_helper :as api]
           [http.api.api_pipe :as steps]
           [clojure.data.json :as json])
)

(defn get-all-loan-products-api [context]
  {:url (str "{{*env*}}/loanproducts")
   :method api/GET
   :query-params {"detailsLevel" "FULL" "limit" 1000}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

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

(defn change-prod-state [prodid state]
  (let [prod-details1 (:last-call (steps/apply-api get-loan-product-api {:prodid prodid}))
        prod-details2 (assoc prod-details1 "state" state)
        ]
    (steps/apply-api update-loan-product-api {:prodid prodid :body prod-details2})))

(defn change-prod-active [prodid]
  (change-prod-state prodid "ACTIVE"))

(defn change-prod-inactive [prodid]
  (change-prod-state prodid "INACTIVE"))

(defn filter-apply-to-all-products [type func-to-apply]
  (let [prod-list (:last-call (steps/apply-api get-all-loan-products-api {}))
        prod-list-filtered (filter (fn [obj] (= (get obj "type") type)) prod-list)]
    (map #(func-to-apply (get % "id")) prod-list-filtered)))

(defonce prod-details (atom {}))

(comment
  (api/setenv "env2") ;; MK prod  
  (api/setenv "env4") ;; EU showcase

  ;; Update state of all "REVOLVING_CREDIT" products
  (filter-apply-to-all-products "REVOLVING_CREDIT" change-prod-inactive)
  (filter-apply-to-all-products "REVOLVING_CREDIT" change-prod-active)

  ;; Get all products (filtered on type)
  (def prod-list0 (filter-apply-to-all-products "REVOLVING_CREDIT" identity))
  prod-list0

  ;; Get all loan-products
  (def prod-list (:last-call (steps/apply-api get-all-loan-products-api {})))
  (count prod-list)
  (map (fn [obj] (get obj "type")) prod-list)
  (map (fn [obj] (get obj "name")) prod-list)

  ;; Get product details
  (reset! prod-details (:last-call (steps/apply-api get-loan-product-api {:prodid "CC-BT1"})))
  (get @prod-details "state")
  (get @prod-details "type")
  @prod-details
  (get-in @prod-details ["scheduleSettings" "billingCycles"])
  (assoc-in @prod-details ["scheduleSettings" "billingCycles"] nil)

  @prod-details

  ;; update product details
  (reset! prod-details (assoc @prod-details "notes" "updated notes"))
  (reset! prod-details (assoc @prod-details "state" "INACTIVE"))
  (reset! prod-details (assoc @prod-details "state" "ACTIVE"))
  (steps/apply-api update-loan-product-api {:prodid "755_ex" :body @prod-details})

  (change-prod-state "755_ex" "INACTIVE")
  (change-prod-state "755_ex" "ACTIVE")
  (change-prod-active "CC-Cash1")
  (change-prod-inactive "RevMort3")


  ;;
  )