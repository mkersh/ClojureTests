;; Helper functions for Mambu config-as-code
;; #bookmark= f60f1934-2d31-4435-b6d3-246578a56644
(ns http.api.mambu.config-as-code.casc_custom_fields
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.config-as-code.casc-helper :as casc]
            [clj-yaml.core :as yaml]
            [clojure.pprint :as pp]))

(defn call-api [api context]
  (:last-call (steps/apply-api api context)))

;; *****************************************************************
;; Mambu core level APIs for custom-fields config-as-code 
;;

;; Get all custom-fields in a tenant
;; NOTE: If :availableFor passed then you can filter
(defn get-customfields-yaml-api [context]
  {:url (str "{{*env*}}/configuration/customfields.yaml")
   :query-params {"availableFor" (:availableFor context)}
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             "Content-Type" "application/yaml"}})

;; Update all custom-fields in a tenant
(defn put-customfields-yaml-api [context]
  {:url (str "{{*env*}}/configuration/customfields.yaml")
   :method api/PUT
   :body (:yaml-str context)
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             "Content-Type" "application/yaml"}})

;; API to get an example YAML file for custom-fields
(defn get-customfields-yaml-template-api [_context]
  {:url (str "{{*env*}}/configuration/customfields/template.yaml")
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+yaml"}})

;; *****************************************************************
;; Higher level helper functions
;;

(defonce ALL_CUST_FIELDS (atom nil))
(defonce LAST_FIELD_SET (atom {}))

(defn get-all-custom-fields []
  (let [cf-yaml (casc/get-yaml-response (call-api get-customfields-yaml-api {}))
        cf-obj (casc/yaml-str-to-edn cf-yaml)
         _ (reset! ALL_CUST_FIELDS cf-obj)
        ]
    "Loaded all fieldssets into @ALL_CUST_FIELDS"))

(defn get-fieldsets [cf-obj]
  (:customFieldSets cf-obj))

(defn number-of-fieldsets-total [cf-obj]
  (count (get-fieldsets cf-obj)))

(defn prn-fieldset-ids [cf-obj]
  (map #(:id %) (get-fieldsets cf-obj)))

(defn get-field [fs-obj id]
  (let [fields-list (:customFields fs-obj)
        match-list  (filter #(= id (:id %)) fields-list)]
    (if (> (count match-list) 0)
      (first match-list)
      (do (prn (str "ERROR: Could not find a match for field " id)) nil))))

(defn get-fieldset2
  [cf-obj fs-id]
   (let [match-list  (filter #(= fs-id (:id %)) (get-fieldsets cf-obj))]
     (if (> (count match-list) 0)
       (let [res (first match-list)
             _ (reset! LAST_FIELD_SET res)]
         res)
       nil)))

  (defn get-fieldset [cf-obj fs-id]
  (let [res (get-fieldset2 cf-obj fs-id)]
    (when (nil? res) (prn (str "ERROR: Could not find a match for " fs-id)))
    res 
    ))

;; remove :customFields to make the top-level fs attributes more visible
(defn get-fs-details [fs-obj]
  (dissoc fs-obj :customFields))

(defn prn-fields 
([cf-obj fs-id]
 (let [fs-obj (get-fieldset cf-obj fs-id)]
   (prn-fields fs-obj)))
([fs-obj]
  (let [fields-list (:customFields fs-obj)]
    (map #(:id %) fields-list))))


;;************************************************
;; Update functions

(defn add-updated-fs-to-all! [obj-to-update]
  (let [all-obj @ALL_CUST_FIELDS
        fs-list (get-fieldsets all-obj)
        id-to-match (:id obj-to-update)
        _ (prn (str "Looking to replace FS with ID=" id-to-match))
        remove? (:remove obj-to-update)
        fs-list2 (filter #(not (nil? %))
                         (map (fn [obj]
                                (let [id2 (:id obj)
                                      _ (prn (str "considering FS ID=" id2))]
                                  (if (= id2 id-to-match)
                                    (if remove? nil obj-to-update)
                                    obj)))
                              fs-list))
        new-all {:customFieldSets fs-list2}]
    (reset! ALL_CUST_FIELDS new-all)))

(defn update-fieldset [fs-obj update-obj]
  (let [fs-obj (merge fs-obj update-obj)
        _ (reset! LAST_FIELD_SET fs-obj)]
    (add-updated-fs-to-all! fs-obj)
    (get-fs-details (get-fieldset @ALL_CUST_FIELDS (:id fs-obj))))
  )

(defn remove-fieldset [id]
(let [obj (get-fieldset @ALL_CUST_FIELDS id)]
  (assert obj (str "ERROR: remove-fieldset ID does not exist " id))
  (add-updated-fs-to-all! {:id id :remove true}))
)

;; https://support.mambu.com/docs/configuration-as-code-for-custom-fields
(defn create-new-fieldset [update-obj]
  (let [id (:id update-obj)
        name (:name update-obj)
        description (or (:description update-obj) "")
        type (:type update-obj) ;; SINGLE GROUPED
        availableFor (:availableFor update-obj) ;; CLIENT, GROUP, CREDIT_ARRANGEMENT, LOAN_ACCOUNT, GUARANTOR, ASSET, DEPOSIT_ACCOUNT, TRANSACTION_CHANNEL, BRANCH, CENTRE, orUSER.
        check-obj (get-fieldset2 @ALL_CUST_FIELDS id)]
    (assert (= (subs id 0 1) "_") (str "ERROR: create-new-fieldset - :id should begin with _ " id))
    (assert (nil? check-obj) (str "ERROR: create-new-fieldset - fieldset already exists " id))
    (assert (and id name type availableFor) "ERROR: create-new-fieldset - mandatory field missing")
    (let [new-fs {:id id :name name :description description :type type :availableFor availableFor :customFields []}
          all-obj @ALL_CUST_FIELDS
          fs-list (get-fieldsets all-obj)
          fs-list2 (conj fs-list new-fs)
          new-all {:customFieldSets fs-list2}
          _ (reset! ALL_CUST_FIELDS new-all)
          _ (get-fieldset @ALL_CUST_FIELDS id)]
      new-fs)))

;; This next function will update changes to your tenant
(defn save-updates! []
  (let [all-obj @ALL_CUST_FIELDS
        yaml-str (casc/edn-to-yaml-str all-obj)
        tenant-env (api/get-env-domain)]
    (prn (str "SAVING custom-fields to " tenant-env))
    (when all-obj (casc/get-yaml-response (call-api put-customfields-yaml-api {:yaml-str yaml-str})))))


(comment
  ;; Test environment to use
  (api/setenv "env2") ;; MK Prod
  (api/setenv "env16a") ;; MH
  (api/setenv "env17") ;; SEUKDEMO

  ;; [1] Get custom-fields information

  (get-all-custom-fields)
  (save-updates!)

  @ALL_CUST_FIELDS
  (number-of-fieldsets-total @ALL_CUST_FIELDS)
  (prn-fieldset-ids @ALL_CUST_FIELDS)

  ;; [1.1] Get a specific fieldset
  (get-fieldset @ALL_CUST_FIELDS "_Guarantor_Default_Guarantors")
  @LAST_FIELD_SET
  (prn-fields @LAST_FIELD_SET)
  (get-fs-details @LAST_FIELD_SET)

  ;; [1.2] Get a specific field
  (get-field @LAST_FIELD_SET "ROUTING_NUMBER_TRANSACTION_CHANN")

  ;; [2] Updating fieldset
  (get-fieldset @ALL_CUST_FIELDS "_NewTestFieldSet")
  @LAST_FIELD_SET
  (get-fs-details @LAST_FIELD_SET)
  (update-fieldset @LAST_FIELD_SET {:description "XXXX YYYY" :availableFor "CLIENT"})
  (save-updates!)

  ;; [3] Create a new fieldet
  (create-new-fieldset {:id "_NewTestFieldSet" :name "NewTestFieldSet" :type "SINGLE" :availableFor "LOAN_ACCOUNT"})
  (get-fieldset @ALL_CUST_FIELDS "_NewTestFieldSet")
  (update-fieldset @LAST_FIELD_SET {:description "This is a dessc"})
  (save-updates!)

  ;; [4] Remove a fieldset
  (remove-fieldset "_NewTestFieldSet")
  (get-fieldset @ALL_CUST_FIELDS "_NewTestFieldSet")
  (save-updates!)




  ;; ********************************
  ;; Older test functions
  (casc/write-yaml-file (call-api get-customfields-yaml-template-api {})
                        "tmp/CasC-customFields-template.yaml")
  (casc/write-yaml-file (call-api get-customfields-yaml-api {})
                        "tmp/CasC-customFields-export-all.yaml")
  (casc/yaml-file-to-edn "tmp/CasC-customFields-export-all.yaml")
  ;; Filter for a specific entity DEPOSIT_ACCOUNT LOAN_ACCOUNT CLIENT GROUP
  (casc/write-yaml-file (call-api get-customfields-yaml-api {:availableFor "DEPOSIT_ACCOUNT"})
                        "tmp/CasC-customFields-export-part.yaml")
  ;; Update CustomFields from a YAML fiel
  (casc/get-yaml-response (call-api put-customfields-yaml-api {:yaml-str (slurp "tmp/TTT.yaml")}))
  (slurp "tmp/CasC-customFields-import.yaml")
  (slurp "tmp/CasC-customFields-export-all.yaml")

  (spit "tmp/TTT.yaml" (yaml/generate-string (yaml/parse-string (slurp "tmp/CasC-customFields-export-all.yaml"))))

  (pp/pprint (yaml/parse-string (slurp "tmp/CasC-customFields-export-all.yaml")))

  ;;
  )