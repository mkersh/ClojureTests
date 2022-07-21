;;; Custom-field-dataset for capturing interest-payaway-instructions
;;; Using http://localhost:3000/goto-file?&bookmark=f60f1934-2d31-4435-b6d3-246578a56644 - To setup custom-fields
;;;
(ns http.api.mambu.demo.workshop0722.custom_fields.deposit_interest_payaway
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.config_as_code.casc_custom_fields :as cf]))



(defn create-update-fieldset-interest-payaway []
  (cf/get-all-custom-fields) ;; Refresh the local cache of CFs
  (cf/create-update-fieldset {:id "_51adde9252d74825" :name "Interest Payaway Instructions" :type "SINGLE" :availableFor "DEPOSIT_ACCOUNT"})
  (cf/add-field2 {:id "type", :type "SELECTION", :state "ACTIVE", :displaySettings {:displayName "type", :description "", :fieldSize "SHORT"},
                  :selectionOptions [{:availableOptions
                                      [{:selectionId "internal", :value "internal", :score 1}
                                       {:selectionId "external", :value "external", :score 2}]}]})
  (cf/add-field2 {:id "sortcode", :type "FREE_TEXT", :state "ACTIVE", :displaySettings {:displayName "sortcode", :description "6 digit UK sortcode", :fieldSize "SHORT"}})
  (cf/add-field2 {:id "accid", :type "FREE_TEXT", :state "ACTIVE", :displaySettings {:displayName "accid", :description "either internal or external accID (depending on type)", :fieldSize "SHORT"}})

  ;; Save the changes to the tenant
  (cf/save-updates!))


(comment
  ;; [1] Select the Mambu tenant that you want to apply to
  (api/setenv "env16a") ;; MH
  (api/setenv "env17") ;; SEUKDEMO
  (cf/get-uuid) ;; Recommend to use this to generate a unique ID for the datafieldset


  ;; [2] create/update the "Interest Payaway Instructions" datasset
  (create-update-fieldset-interest-payaway)

  ;; Debug functions for examining the dataset
  (cf/get-fieldset @cf/ALL_CUST_FIELDS "_51adde9252d74825")
  @cf/LAST_FIELD_SET
  (cf/prn-fields @cf/LAST_FIELD_SET)
  (cf/get-fs-details @cf/LAST_FIELD_SET)

  ;;
  )

