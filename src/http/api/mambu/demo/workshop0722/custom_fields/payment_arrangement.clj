;;; Custom-field-dataset for capturing payment-arrangement(s)
;;; Using http://localhost:3000/goto-file?&bookmark=f60f1934-2d31-4435-b6d3-246578a56644 - To setup custom-fields
;;; #bookmark= b2d41d5a-425e-444c-a169-99b791e1f39e
(ns http.api.mambu.demo.workshop0722.custom_fields.payment_arrangement
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.config_as_code.casc_custom_fields :as cf]))



(defn create-update-fieldset-payment-arrangement []
  (cf/get-all-custom-fields) ;; Refresh the local cache of CFs
  (cf/create-update-fieldset {:id "_15ace49695324129" :name "Payment Arrangements" :type "GROUPED" :availableFor "LOAN_ACCOUNT"})
  (cf/add-field2 {:id "start-date", :type "DATE", :state "ACTIVE", :displaySettings {:displayName "Start Date", :description "The start date of the temporary arrangement", :fieldSize "SHORT"}})
  (cf/add-field2 {:id "end-date", :type "DATE", :state "ACTIVE", :displaySettings {:displayName "End Date", :description "The end date of the temporary arrangement", :fieldSize "SHORT"}})
  (cf/add-field2 {:id "amount", :type "FREE_TEXT", :state "ACTIVE", :displaySettings {:displayName "Amount", :description "The amount agreed for the temporary arrangement", :fieldSize "SHORT"}})
  (cf/add-field2 {:id "status", :type "SELECTION", :state "ACTIVE", :displaySettings {:displayName "Status", :description "The status of the temporary arrangement", :fieldSize "SHORT"},
                  :selectionOptions [{:availableOptions
                                      [{:selectionId "Active", :value "active", :score 1}
                                       {:selectionId "Cancelled", :value "cancelled", :score 2}
                                       {:selectionId "Broken", :value "broken", :score 3}
                                       ]}]})

  ;; Save the changes to the tenant
  (cf/save-updates!))


(comment
  ;; [1] Select the Mambu tenant that you want to apply to
  (api/setenv "env16a") ;; MH
  (api/setenv "env17") ;; SEUKDEMO
  (cf/get-uuid) ;; Recommend to use this to generate a unique ID for the datafieldset


  ;; [2] create/update the "Interest Payaway Instructions" datasset
  (create-update-fieldset-payment-arrangement)

  ;; Debug functions for examining the dataset
  (cf/get-all-custom-fields)
  (cf/get-fieldset @cf/ALL_CUST_FIELDS "_51adde9252d74825")
  @cf/ALL_CUST_FIELDS
  @cf/LAST_FIELD_SET
  (cf/prn-fields @cf/LAST_FIELD_SET)
  (cf/get-fs-details @cf/LAST_FIELD_SET)

  ;;
  )

