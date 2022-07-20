(ns http.api.mambu.demo.workshop0722.regular-saver.regsave
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.demo.loyalty_points :as lpd]
            [http.api.mambu.demo.credit_card.zap_cust :as zap]
            ))


(defn call-api [api context]
  (:last-call (steps/apply-api api context)))

(defn get-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-deposit-product-api [context]
  {:url (str "{{*env*}}/depositproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn search-savings-api [context]
  {:url (str "{{*env*}}/savings/search")
   :method api/POST
   :body {"filterConstraints" [{"filterSelection" "ACCOUNT_HOLDER_KEY",
                                "filterElement" "EQUALS",
                                "value" "8a19c1c8821634d601821763a51f7e55"}
                               {"filterSelection" "PRODUCT_KEY",
                                "filterElement" "EQUALS",
                                "value" "8a19c1c8821634d6018217502f276430"}]}
   :headers {"Content-Type" "application/json"}})

(defn createSavingsAccount [context]
  {:url (str "{{*env*}}/deposits")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {
          "accountType" "CURRENT_ACCOUNT"
          "name" (:acc-name context)
          "accountHolderKey" (:cust-key context)
          "productTypeKey" (:product-key context)
          "currencyCode" "GBP"
          "accountHolderType" "CLIENT"}})

(defn get-client-api [context]
  {:url (str "{{*env*}}/clients/" (:custid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn zap-cust [context]
 (let  [cust-key (get (call-api get-client-api context) "encodedKey")
        _ (prn "cust-key" cust-key)
        context1 (assoc context :cust-key cust-key)]
  (doall (zap/remove-all-open-dep-accounts context1))
  (steps/apply-api zap/exit-customer2 context1)))

;; A collection of steps     
(defn create-regular-saver-customer-and-account [num]
  {:context {:show-only false ; when true only prints steps, doesn't execute
             :verbose true ;; print out step :label(s)
             :first-name "RegSaver", :last-name (str "Tester " num)
             :branchid "8a19c1c8821634d60182174d21876027"
             ;; **** Debug Only settings next - allows you to jump to individual steps
             :cust-key "8a19c1c8821634d6018217a3282e5798"
             :regsaver-id "OXVX863"
             :bonus-id "GSOT125"
             :custid "861492689"
             }
   :xjump-to-step [:step2 :one-only] ; set ":id :jump-here" in a step 
   :steps [;; [STEP-1] Create Customer
           {:label "[STEP-1] Create Customer"
            :request lpd/create-customer
            :post-filter [;(steps/save-last-to-context :cust-create)
                          (steps/save-part-to-context ["encodedKey"] :cust-key)
                          (steps/save-part-to-context ["id"] :custid)]}
           ;; [STEP-2] Create RegSaver
           {:label "[STEP-2] Create RegSaver"
           :id :step2
            :pre-filter [(steps/save-value-to-context "8a19c1c8821634d6018217502f27642d" :product-key)
                         (steps/save-value-to-context "RegSaver" :acc-name)
                         ]
            :request createSavingsAccount
            :post-filter [(steps/save-part-to-context ["id"] :regsaver-id)]}

            ;; [STEP-3] Create Bonus Account
           {:label "[STEP-3] Create Bonus Account"
            :pre-filter [(steps/save-value-to-context "8a19c1c8821634d6018217502f276430" :product-key)
                         (steps/save-value-to-context "Bonus" :acc-name)]
            :request createSavingsAccount
            :post-filter [(steps/save-part-to-context ["id"] :bonus-id)]}


            ;; [STEP-7] Approve Accounts
           {:label "[STEP-4] Approve Accounts"
            :pre-filter [(steps/save-context-value-to-context :regsaver-id :accid)]
            :request lpd/approveDepositAccount}
           {:pre-filter [(steps/save-context-value-to-context :bonus-id :accid)]
            :request lpd/approveDepositAccount}]})


(comment
(api/setenv "env17")

(steps/process-collection (create-regular-saver-customer-and-account 4)) ;; The number passed is used for the name ""RegSaver Tester n""
(zap-cust {:custid "522739738"})

(call-api get-deposit-product-api {:prodid "RegSave1"})

(call-api search-savings-api {})
(zap/get-all-open-dep-accounts {:cust-key "8a19c1c8821634d601821a434f426212"})

;; Below works
(call-api get-loan-product-api {:prodid "LP2"})

;;
)