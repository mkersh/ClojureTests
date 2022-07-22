;;; Custom-field-dataset for capturing maturity-instructions(s)
;;; Using http://localhost:3000/goto-file?&bookmark=f60f1934-2d31-4435-b6d3-246578a56644 - To setup custom-fields
;;; #bookmark= b2d41d5a-425e-444c-a169-99b791e1f39e
(ns http.api.mambu.demo.workshop0722.custom_fields.maturity_instructions
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.config_as_code.casc_custom_fields :as cf]))



(defn create-update-fieldset-maturity-insructions []
  (cf/get-all-custom-fields) ;; Refresh the local cache of CFs
  (cf/create-update-fieldset {:id "_Rollover_Instructions" :name "Rollover Instructions" :type "SINGLE" :availableFor "DEPOSIT_ACCOUNT"})
  ;; using cf/add-field to create the fields exactly as defined - normally I would use cf/add-field2
  (cf/add-field {:id "Rollover_Y_N_Deposit_Accounts",
                  :type "SELECTION",
                  :state "ACTIVE",
                  :displaySettings {:displayName "Rollover Y/N", :description "", :fieldSize "SHORT"},
                  :usage [{:id "FDAUTO1", :required false, :default false} {:id "FDAUTO2", :required false, :default false}],
                  :selectionOptions
                  [{:availableOptions [{:selectionId "1190873966", :value "Yes"} {:selectionId "510937446", :value "No"}]}]})

  (cf/add-field {:id "Rollover_Product_Deposit_Account",
                  :type "SELECTION",
                  :state "ACTIVE",
                  :displaySettings {:displayName "Rollover Account Type", :description "", :fieldSize "SHORT"},
                  :usage [{:id "FDAUTO1", :required false, :default false} {:id "FDAUTO2", :required false, :default false}],
                  :selectionOptions
                  [{:availableOptions
                    [{:selectionId "1333498960", :value "Interest Paid at Maturity"}
                     {:selectionId "852614126", :value "Interest Paid Monthly"}]}]})

  (cf/add-field {:id "Rollover_Term_Deposit_Accounts",
                 :type "SELECTION",
                 :state "ACTIVE",
                 :displaySettings {:displayName "Rollover Term", :description "", :fieldSize "SHORT"},
                 :usage [{:id "FDAUTO1", :required false, :default false} {:id "FDAUTO2", :required false, :default false}],
                 :selectionOptions
                 [{:availableOptions
                   [{:selectionId "126235597", :value "1"}
                    {:selectionId "303927213", :value "2"}
                    {:selectionId "1151172406", :value "3"}
                    {:selectionId "1265741853", :value "4"}
                    {:selectionId "1677976503", :value "6"}
                    {:selectionId "824610026", :value "9"}
                    {:selectionId "1080463394", :value "12"}]}]})

  (cf/add-field {:id "Rollover_Amount_Deposit_Accounts",
                 :type "SELECTION",
                 :state "ACTIVE",
                 :displaySettings {:displayName "Rollover Instruction Type", :description "", :fieldSize "SHORT"},
                 :usage [{:id "FDAUTO1", :required false, :default false} {:id "FDAUTO2", :required false, :default false}],
                 :selectionOptions
                 [{:availableOptions
                   [{:selectionId "920000866", :value "Original Principal without Interest", :score 1}
                    {:selectionId "1777494515", :value "Original Principal with Interest", :score 2}
                    {:selectionId "1492259036", :value "Specify Rollover Amount", :score 3}
                    {:selectionId "22822573", :value "Specify Payout Amount", :score 4}]}]})

  (cf/add-field {:id "Specified_Rollover_Amount_Deposi",
                 :type "NUMBER",
                 :state "ACTIVE",
                 :displaySettings {:displayName "Rollover Amount", :description "", :fieldSize "SHORT"},
                 :usage [{:id "FDAUTO1", :required false, :default false} {:id "FDAUTO2", :required false, :default false}],
                 :availableForAll false})

 (cf/add-field  {:id "Specify_Payout_Amount_Deposit_Ac",
                  :type "NUMBER",
                  :state "ACTIVE",
                  :displaySettings {:displayName "Rollover Payout Amount", :description "", :fieldSize "SHORT"},
                 :usage [{:id "FDAUTO1", :required false, :default false} {:id "FDAUTO2", :required false, :default false}]})

 (cf/add-field {:id "Payout_Account_Number_Deposit_Ac",
                 :type "FREE_TEXT",
                 :state "ACTIVE",
                 :validationRules {:unique false},
                 :displaySettings {:displayName "Rollover Payout Account", :description "", :fieldSize "SHORT"},
                 :usage [{:id "FDAUTO1", :required false, :default false} {:id "FDAUTO2", :required false, :default false}]})

  (cf/add-field {:id "Payout_BSB_Number_Deposit_Accoun",
                  :type "FREE_TEXT",
                  :state "ACTIVE",
                  :validationRules {:unique false},
                  :displaySettings {:displayName "Payout SortCode Number", :description "", :fieldSize "SHORT"},
                  :usage [{:id "FDAUTO1", :required false, :default false} {:id "FDAUTO2", :required false, :default false}]})


  ;; Save the changes to the tenant
  (cf/save-updates!))


(comment
  ;; [1] Select the Mambu tenant that you want to apply to
  (api/setenv "env6") ;; EUShowcase
  (api/setenv "env16a") ;; MH
  (api/setenv "env17") ;; SEUKDEMO
  
  (cf/get-uuid) ;; Recommend to use this to generate a unique ID for the datafieldset


  ;; [2] create/update the "Interest Payaway Instructions" datasset
  (create-update-fieldset-maturity-insructions)






  ;; Debug functions for examining the dataset
  (cf/get-all-custom-fields)
  (cf/prn-fieldset-ids @cf/ALL_CUST_FIELDS)
  (cf/get-fieldset @cf/ALL_CUST_FIELDS "_Rollover_Instructions")
  @cf/ALL_CUST_FIELDS
  @cf/LAST_FIELD_SET
  (cf/prn-fields @cf/LAST_FIELD_SET)
  (cf/get-fs-details @cf/LAST_FIELD_SET)

  ;;
  )

