(ns mambu.extensions.product-factory.mod-products.update-product
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.data.json :as json]))

(defonce GLOBAL-BRANCH (atom ""))

(defn get-all-loan-products-api [_context]
  {:url (str "{{*env*}}/loanproducts")
   :method api/GET
   :query-params {"detailsLevel" "FULL" "limit" 1000}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-all-deposit-products-api [_context]
  {:url (str "{{*env*}}/depositproducts")
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


(defn patch-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/PATCH
   :body (:body context)
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-deposit-product-api [context]
  {:url (str "{{*env*}}/depositproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn patch-deposit-product-api [context]
  {:url (str "{{*env*}}/depositproducts/" (:prodid context))
   :method api/PATCH
   :body (:body context)
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn change-prod-state [prodid state]
  (let [prod-details1 (:last-call (steps/apply-api get-loan-product-api {:prodid prodid}))
        prod-details2 (assoc prod-details1 "state" state)]
    (steps/apply-api update-loan-product-api {:prodid prodid :body prod-details2})))

(defn change-prod-active [prodid]
  (change-prod-state prodid "ACTIVE"))

(defn change-prod-inactive [prodid]
  (change-prod-state prodid "INACTIVE"))

(defn filter-apply-to-all-products [type func-to-apply]
  (let [prod-list (:last-call (steps/apply-api get-all-loan-products-api {}))
        prod-list-filtered (filter (fn [obj] (= (get obj "type") type)) prod-list)]
    (map #(func-to-apply (get % "id")) prod-list-filtered)))

(defn change-branch-global [prodid get-product-api patch-product-api global-branch]
  (let [obj (:last-call (steps/apply-api get-product-api {:prodid prodid}))
        avail-settings (get obj "availabilitySettings")
        branch-settings (get avail-settings "branchSettings")
        is-global-loan (get branch-settings "forAllBranches")
        avail-branches (get branch-settings "availableProductBranches")]
    (if is-global-loan
      (do (prn (str "Moving global product " prodid))
          (let [avail-branches (if (empty? avail-branches) [global-branch] avail-branches)
                branch-settings2 {"forAllBranches" false,
                                  "availableProductBranches" avail-branches}
                avail-settings2 (assoc avail-settings "branchSettings" branch-settings2)
                patch-body [{"op" "REPLACE"
                             "path" "availabilitySettings"
                             "value" avail-settings2}]]
            ;;(prn patch-body)
            (steps/apply-api patch-product-api {:prodid prodid  :body patch-body})))
      (prn (str "Ignore product " prodid)))))

(defn change-branch-global-deposit [prodid  global-branch]
  (change-branch-global prodid get-deposit-product-api patch-deposit-product-api global-branch))

(defn change-branch-global-loan [prodid  global-branch]
  (change-branch-global prodid get-loan-product-api patch-loan-product-api global-branch))

;; Move any global products to and remove forAllBranches=true setting
(defn removeall-global-products [global-branch]
  (prn "Start removeall-global-products")
  (let [all-deposit-products (:last-call (steps/apply-api get-all-deposit-products-api {}))
        _ (prn "retrieved all deposit-products")
        all-loan-products (:last-call (steps/apply-api get-all-loan-products-api {}))
         _ (prn "retrieved all loan-products")
        ]
    (doall (map (fn [obj]
                  (prn "consider deposit " (get obj "id"))
                  (api/ignore-exceptions (change-branch-global-deposit (get obj "id") global-branch))
                  )
                all-deposit-products))
    (doall (map (fn [obj]
                  (prn "consider loan " (get obj "id"))
                  (api/ignore-exceptions (change-branch-global-loan (get obj "id") global-branch)))
                all-loan-products))))

(defonce prod-details (atom {}))
(defonce patch-body (atom []))

(comment
  (api/setenv "env2") ;; MK prod  
  (api/setenv "env4") ;; EU showcase

  ;; Remove/move all global products to 
  (reset! GLOBAL-BRANCH "8a818e078119bf6301814e5161841737")
  (removeall-global-products @GLOBAL-BRANCH)

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
  (map (fn [obj] (get obj "id")) prod-list)
  ;; print products setup globally
  (map (fn [obj] (get obj "id")) prod-list)

  ;; Test that individual PATCH works
  (reset! GLOBAL-BRANCH "8a818e078119bf6301814e5161841737")
  (change-branch-global-deposit "ZZTest" @GLOBAL-BRANCH)
  (change-branch-global-deposit "FXPOT_AUD" @GLOBAL-BRANCH)
  (change-branch-global-loan "Trust Account" @GLOBAL-BRANCH)
  (steps/apply-api get-loan-product-api {:prodid "Trust Account"})

  (reset! patch-body [{"op" "REPLACE"
                       "path" "notes"
                       "value" "updated notes via patch"}])
  (steps/apply-api patch-loan-product-api {:prodid "TBIO2" :body @patch-body})
  (get-in (steps/apply-api get-loan-product-api {:prodid "TBIO2"}) [:last-call "notes"])
  (steps/apply-api get-loan-product-api {:prodid "8a818e447f3c6cfc017f3f613f9659d2"})
  

  (steps/apply-api patch-deposit-product-api {:prodid "FXPOT_AUD"  :body @patch-body})
  (steps/apply-api get-deposit-product-api {:prodid "FXPOT_AUD"})
  (get-in (steps/apply-api get-deposit-product-api {:prodid "FXPOT_AUD"}) [:last-call "notes"])
  (get-in (steps/apply-api get-deposit-product-api {:prodid "FXPOT_AUD"}) [:last-call "availabilitySettings" "branchSettings"])



  ;; Show all INACTIVE loan products
  (let [prod-list (:last-call (steps/apply-api get-all-loan-products-api {}))
        obj-list (filter (fn [obj] (= (get obj "state") "INACTIVE")) prod-list)]
    (map (fn [obj] (get obj "name")) obj-list))

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

(* 45 (* 2 82.2))

  ;;
  )