;; Workaround for storing loan-product custom-fields 
;; storing these under a branch+centre
(ns http.api.mambu.config-as-code.product-cf
  (:require  [http.api.json_helper :as api]
             [http.api.api_pipe :as steps]
             [http.api.mambu.config_as_code.casc_custom_fields :as cf]
             [http.api.mambu.config-as-code.branch-casc :as br]
             [http.api.mambu.config_as_code.centre_casc :as cc]))


(defn call-api [api context]
  (:last-call (steps/apply-api api context)))

(defn create-product-settings-branch []
  (prn "create-product-settings-branch")
  (br/create-update-branch-safe {:id "8ee9174fc3ea4d60",
                            :name "**product-settings**",
                            :state "ACTIVE",
                            :phoneNumber "",
                            :emailAddress "",
                            :notes nil,
                            :address nil,
                            :holidays nil}))

;; Given a custom-field-set attach it to a branch/centre representing the product-id
(defn add-product-cf [product-id cfs-obj]
  (let [special-branch-exists? (br/does-branch-exist? "8ee9174fc3ea4d60")
        _ (when (not special-branch-exists?) (create-product-settings-branch))
        product-centre-id (str product-id "-settings")
        product-centre-exists? (cc/does-centre-exist? product-centre-id)
        _ (when (not product-centre-exists?) (prn "Create Centre") (cc/create-update-centre-safe {:id product-centre-id,
                                                                       :name product-centre-id,
                                                                       :state "ACTIVE",
                                                                       :notes "",
                                                                       :assignedBranchId "8ee9174fc3ea4d60",
                                                                       :address {}}))
        usage-list [{:id product-centre-id, :required false, :default false}]
        cfs-obj2 (assoc cfs-obj :usage usage-list)]
    (cf/create-update-fieldset-save cfs-obj2)))


(defn loan-product01-custom-fields []
  (let [_ (cf/create-update-fieldset {:id "_4816116c6cf34b9f" :name "loan-product01 custom fields" :type "SINGLE" :availableFor "CENTRE"})
        _ (cf/add-field2 @cf/LAST_FIELD_SET {:id "mk1", :type "FREE_TEXT", :state "ACTIVE", :validationRules {:unique false}, :displaySettings {:displayName nil, :description "", :fieldSize "LONG"}, :viewRights {:roles (), :allUsers true},  :editRights {:roles (), :allUsers false}})]
  @cf/LAST_FIELD_SET
  )
  
  )

(comment
  (cf/get-uuid)
  (api/setenv "env17") ;; SEUKDEMO
  (br/get-all-branches)
  (create-product-settings-branch)
  (br/save-updates!)

  (loan-product01-custom-fields)
  (add-product-cf "loan-product01" (loan-product01-custom-fields))

 ;; 
  )