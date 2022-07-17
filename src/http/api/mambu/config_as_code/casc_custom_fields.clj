;; Helper functions for Mambu config-as-code
;; #bookmark= f60f1934-2d31-4435-b6d3-246578a56644
(ns http.api.mambu.config-as-code.casc_custom_fields
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.config-as-code.casc-helper :as casc]
            [clj-yaml.core :as yaml]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            ))

(import java.util.UUID)

(defn call-api [api context]
  (:last-call (steps/apply-api api context)))

(defn get-uuid []
  (let [uuid1 (UUID/randomUUID)
        uuid2 (str/replace uuid1 #"-" "")]
    (subs uuid2 0 16)))

;; *****************************************************************
;; Mambu core level APIs for custom-fields config-as-code 
;;

(defonce CUSTID (atom nil))

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
;; Mambu core level APIs for attaching custom fields to objects
;;

(defn create-customer-api [context]
  {:url (str "{{*env*}}/clients")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body (merge {"firstName" (:first-name context)
                 "lastName" (:last-name context)
                 "preferredLanguage" "ENGLISH"
                 "addresses" [{"country" "UK"
                               "city" "Liverpool"}]
                 "notes" "Some Notes on this person"
                 "assignedBranchKey" (:branchid context)} (:custom-fields context))
          })

(defn delete-client-api [context]
  {:url (str "{{*env*}}/clients/" (:clientid context))
   :method api/DELETE
   :query-params {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-client-api [context]
  {:url (str "{{*env*}}/clients/" (:clientid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn patch-customer-api [context]
  {:url (str "{{*env*}}/clients/" (:clientId context))
   :method api/PATCH
   :body [{"op" "add"
           "path" "/_MKExtraCustomer/MyCustomerField1"
           "value" "Oh yes"}

           ;; Patching a grouped DataFieldSet
          {"op" "replace"
           "path" "/_GlobalCreditLimits"
           "value" (:value context)}]
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; *****************************************************************
;; Higher level helper functions
;;

(defonce ALL_CUST_FIELDS (atom nil))
(defonce CF-DISPLAY-MAP (atom nil))
(defonce LAST_FIELD_SET (atom {}))
(defonce APPEND_FSNAME_TO_FIELD (atom false))

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

(defn setup-cf-display-map-aux [disp-map fs-obj]
  (reduce (fn [disp-map cf-obj]
            (let [id (:id cf-obj)      
                  display-name (get-in cf-obj [:displaySettings :displayName])
                  unified-display-name (str/trim display-name)
                  disp-obj (get disp-map unified-display-name)
                  disp-obj2 (assoc disp-obj id display-name)]
              (assoc disp-map unified-display-name disp-obj2)))
          disp-map (:customFields fs-obj)))

(defn setup-cf-display-map [cf-obj]
  (reduce
   (fn [disp-map fs-obj] (setup-cf-display-map-aux disp-map fs-obj))
   {} (get-fieldsets cf-obj)))

(comment 
(+ 1 2)
(setup-cf-display-map @ALL_CUST_FIELDS)
)
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

;; Add or update field
(defn add-field [fs-obj field-obj]
  (let [id0 (:id field-obj)
        id (if @APPEND_FSNAME_TO_FIELD (str id0 "_" (subs (:id fs-obj) 1)) id0)
        type (:type field-obj)
        state (:state field-obj)
        validationRules (:validationRules field-obj)
        displaySettings0 (:displaySettings field-obj)
        displaySettings (if (:displayName displaySettings0) displaySettings0 (merge displaySettings0 {:displayName id}))
        usage (:usage field-obj)
        viewRights (:viewRights field-obj)
        editRights (:editRights field-obj)
        availableForAll (:availableForAll field-obj)
        required (:required field-obj)
        default (:default field-obj)]

    (assert (and id type state validationRules displaySettings viewRights editRights (boolean? availableForAll) (boolean? required) (boolean? default)) "ERROR: add-field - mandatory params missing")

    (let [field-obj0 {:id id,
                     :type type,
                     :state state,
                     :validationRules validationRules,
                     :displaySettings displaySettings,
                     :viewRights viewRights,
                     :editRights editRights,
                     :availableForAll availableForAll
                     }
          field-obj (if usage (merge field-obj0 {:usage usage})
                         (merge field-obj0 {:required required :default default}))
          fields-list (:customFields fs-obj)
          fields-list1 (filter (fn [obj] (not (= (:id obj) id))) fields-list)
          _ (when (< (count fields-list1) (count fields-list)) (prn (str "Updating existing field ID = " id)))
          fields-list2 (conj fields-list1 field-obj)
          fs-obj2 (assoc fs-obj :customFields fields-list2)
          _ (reset! LAST_FIELD_SET fs-obj2)]
      (update-fieldset fs-obj2 fs-obj2))))

;; This next function will update changes to your tenant
(defn save-updates! []
  (let [all-obj @ALL_CUST_FIELDS
        yaml-str (casc/edn-to-yaml-str all-obj)
        tenant-env (api/get-env-domain)
        _  (prn (str "SAVING custom-fields to " tenant-env))
        response-obj  (call-api put-customfields-yaml-api {:yaml-str yaml-str})
        _ (prn "response - " response-obj)]
    (when all-obj (casc/get-yaml-response response-obj))))


(comment
  ;; Test environment to use
  (api/setenv "env2") ;; MK Prod
  (api/setenv "env16a") ;; MH
  (api/setenv "env17") ;; SEUKDEMO
  (get-uuid)

  ;; [1] Get custom-fields information

  (get-all-custom-fields)
  (save-updates!)

  @ALL_CUST_FIELDS
  (number-of-fieldsets-total @ALL_CUST_FIELDS)
  (prn-fieldset-ids @ALL_CUST_FIELDS)

  ;; [1.1] Get a specific fieldset
  (get-fieldset @ALL_CUST_FIELDS "_NewClientFieldSet")
  @LAST_FIELD_SET
  (prn-fields @LAST_FIELD_SET)
  (get-fs-details @LAST_FIELD_SET)

  ;; [1.2] Get a specific field
  (get-field @LAST_FIELD_SET "virtualAccountId")

  ;; [2] Updating fieldset
  (get-fieldset @ALL_CUST_FIELDS "_NewTestFieldSet")
  @LAST_FIELD_SET
  (get-fs-details @LAST_FIELD_SET)
  (update-fieldset @LAST_FIELD_SET {:description "XXXX YYYY" :availableFor "LOAN_ACCOUNT"})
  (save-updates!)

  ;; [3] Create a new fieldet
  (create-new-fieldset {:id "_NewTestFieldSet" :name "NewTestFieldSet" :type "SINGLE" :availableFor "LOAN_ACCOUNT"})
  (create-new-fieldset {:id "_NewClientFieldSet" :name "NewClientFieldSet" :type "SINGLE" :availableFor "CLIENT"})
  (create-new-fieldset {:id "_bb1be4204d574d95" :name "NewClientFieldSet3333" :type "SINGLE" :availableFor "CLIENT"})

 
  (get-fieldset @ALL_CUST_FIELDS "_bb1be4204d574d95")
  (get-fs-details @LAST_FIELD_SET)
  (update-fieldset @LAST_FIELD_SET {:description "This is a dessc" :type "SINGLE" :name "NewClientFieldSet222"})
  (save-updates!)

  ;; [4] Remove a fieldset
  ;; TBD - This does not work at the moment
  (remove-fieldset "_bb1be4204d574d95")
  (get-fieldset @ALL_CUST_FIELDS "_NewClientFieldSet")
  (save-updates!)

  ;; [5] Add a field
  (reset! APPEND_FSNAME_TO_FIELD true)
  (reset! APPEND_FSNAME_TO_FIELD false) ;; You need to enure that ID are unique
  (add-field @LAST_FIELD_SET {:id "mk1",
                              :type "FREE_TEXT",
                              :state "ACTIVE",
                              :validationRules {:unique false},
                              :displaySettings {:displayName nil, :description "", :fieldSize "LONG"},
                              ;; :usage [{:id "client", :required false, :default false}
                              ;;         {:id "client-type2", :required false, :default false}]
                              ;; :usage [{:id "LP2", :required false, :default false}]
                              :viewRights {:roles (), :allUsers true},
                              :editRights {:roles (), :allUsers false},
                              :availableForAll false,
                              :required false,
                              :default false})

 
  ;; [6] Test adding some of th new custom-fields to objects
  (reset! CUSTID (get (call-api create-customer-api
                                {:first-name "CF" :last-name "Tester7"
                                 :custom-fields {"_NewClientFieldSet" {"client-field-1" "value222d"}}})
                      "id"))
  
  
  (call-api delete-client-api {:clientid @CUSTID})
  (call-api get-client-api {:clientid @CUSTID})































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