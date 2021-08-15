(ns http.api.mambu.demo.copy-products
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.data.json :as json]
            ))


(defn get-deposit-product-api [context]
  {:url (str "{{*env*}}/depositproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn create-deposit-product-api [context]
  {:url (str "{{*env*}}/depositproducts/")
   :method api/POST
   :body (:body context)
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn create-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/")
   :method api/POST
   :body (:body context)
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(defn write-product-file [jsonStr fpath1]
  (spit fpath1 jsonStr))

(defn get-edn-from-file [fpath1]
  (read-string (slurp fpath1)))

(defn call-api-write-to-file [api context]
  (let [context (steps/apply-api api context)
        ednStr (:last-call context)
        jsonStr (json/write-str ednStr)
        ednPath (:ednPath context)
        jsonPath (:jsonPath context)
        _  (write-product-file jsonStr "DEPOSIT-PROD.json")]
    (if ednPath (write-product-file ednStr ednPath) nil)
    (if jsonPath (write-product-file jsonStr jsonPath) nil)
    ednStr))

(defn change-value [obj change-item]
  (let [path-list (:path change-item)
        new-val (:new-val change-item)]
    (prn "changing" path-list new-val)
    (assoc-in obj path-list new-val)))

(defn change-multi-values [obj change-list]
  (reduce change-value obj change-list))

(defn call-api-body-from-file [api ednFpath changes-list]
  (let [body0 (get-edn-from-file ednFpath)
        body (change-multi-values body0 changes-list)]
    (steps/apply-api api {:body body})))


(comment
  (api/setenv "env2") ;; MK prod
  (call-api-write-to-file get-deposit-product-api {:prodid "ccpay2" :ednPath "CCPAY_DATA.clj"})
  (call-api-write-to-file get-loan-product-api {:prodid "CC-Purchase3" :ednPath "CC-Purchase.clj"})
  (call-api-write-to-file get-loan-product-api {:prodid "PayIn3" :ednPath "PayIn3.clj"})
  (call-api-write-to-file get-loan-product-api {:prodid "PromLoan2" :ednPath "BNPL.clj"})
  (call-api-write-to-file get-loan-product-api {:prodid "promo1" :ednPath "PROMO.clj"})

  ;;--------------------------------------------------------------
  ;; Creating the  product set in a new sandbox tenant

  ;; IMPORTANT - Remember to change the environment that will be called
  (api/setenv "env1") ;; MK Sandbox
  ;;(api/setenv "env7") ;; demotenant.staging.mambucloud.com
  (api/setenv "env5") ;; https://europeshowcase.sandbox.mambu.com/

  ;; [0] Change the following
  (def interest-rate-index "8a19aa827aa96168017aab023d185c1d") ;; To a valid interest-rate index on the tenant
  (def zerorate-index "8a19aa827aa96168017aab023d185c1f")

  ;; [1] Create payment/settlement product
  (def product-changes [{:path ["id"] :new-val "pospay1"}
                        {:path ["encodedKey"] :new-val nil}])
  (call-api-body-from-file create-deposit-product-api "CCPAY_DATA.clj" product-changes)

  ;; [2] Create RCA product
  (def product-changes [{:path ["interestSettings" "indexRateSettings" "indexSourceKey"] :new-val interest-rate-index}
                        {:path ["id"] :new-val "RCA-Purchase5"}
                        {:path ["encodedKey"] :new-val nil}])
  (call-api-body-from-file create-loan-product-api "CC-Purchase.clj" product-changes)

  ;; [3] Create PayIn3 product
  (def product-changes [{:path ["interestSettings" "indexRateSettings" "indexSourceKey"] :new-val zerorate-index}
                        {:path ["id"] :new-val "PayIn3"}
                        {:path ["encodedKey"] :new-val nil}])
  (call-api-body-from-file create-loan-product-api "PayIn3.clj" product-changes)

  ;; [4] Create BNPL product
  (def product-changes [{:path ["interestSettings" "indexRateSettings" "indexSourceKey"] :new-val interest-rate-index}
                        {:path ["id"] :new-val "BNPL11"}
                        {:path ["encodedKey"] :new-val nil}])
  (call-api-body-from-file create-loan-product-api "BNPL.clj" product-changes)

  ;; [4] Create PROMO product
  (def product-changes [{:path ["interestSettings" "indexRateSettings" "indexSourceKey"] :new-val interest-rate-index}
                        {:path ["id"] :new-val "promo1"}
                        {:path ["encodedKey"] :new-val nil}])
  (call-api-body-from-file create-loan-product-api "PROMO.clj" product-changes)


  (get-edn-from-file "CCPAY_DATA.clj")



;;
  )