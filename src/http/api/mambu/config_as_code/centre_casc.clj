(ns http.api.mambu.config_as_code.centre_casc
  (:require  [http.api.json_helper :as api]
             [http.api.api_pipe :as steps]
             [http.api.mambu.config-as-code.casc-helper :as casc]))


(defn call-api [api context]
  (:last-call (steps/apply-api api context)))

;; *****************************************************************
;; Mambu core config-as-code endpoints for centrees
;;

(defn get-centre-yaml-api [context]
  {:url (str "{{*env*}}/configuration/centres.yaml")
   :query-params {}
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             "Content-Type" "application/yaml"}})

;; Update all custom-fields in a tenant
(defn put-centre-yaml-api [context]
  {:url (str "{{*env*}}/configuration/centres.yaml")
   :method api/PUT
   :body (:yaml-str context)
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             "Content-Type" "application/yaml"}})


(defonce ALL_CENTRES (atom nil))

(defn get-all-centres []
  (let [centre-yaml (casc/get-yaml-response (call-api get-centre-yaml-api {}))
        br-obj (casc/yaml-str-to-edn centre-yaml)
        _ (reset! ALL_CENTRES br-obj)]
    "Loaded all centrees into @ALL_CENTRES"))

;; This next function will update changes to your tenant
(defn save-updates! []
  (let [all-obj @ALL_CENTRES
        yaml-str (casc/edn-to-yaml-str all-obj)
        tenant-env (api/get-env-domain)
        _  (prn (str "SAVING centres to " tenant-env))
        response-obj  (call-api put-centre-yaml-api {:yaml-str yaml-str})
        _ (prn "response - " response-obj)]
    (when all-obj (casc/get-yaml-response response-obj))))

;; *****************************************************************
;; Mambu core API for branch
;;

(defn get-centre-api [context]
  {:url (str "{{*env*}}/centres/" (:id context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn does-centre-exist? [centreid]
  (let [res-obj (call-api get-centre-api {:id centreid})
        errors (get res-obj "errors")]
    (if errors nil res-obj)))

;; *****************************************************************
;; Higher level functions for manipulating centrees
;;

(defn remove-id-from-list [id obj-list]
  (filter #(not (= id (:id %))) obj-list))


(defn create-update-centre [centre-obj]
  (let [id (:id centre-obj)
        name (:name centre-obj)
        state (:state centre-obj)
        notes (:notes centre-obj)
        assignedBranchId (:assignedBranchId centre-obj)
        address (:address centre-obj)]

    (assert (and id name state) "ERROR: create-new-centre - params missing")

    (let [br-obj {:id id :name name :state state :notes notes :address address :assignedBranchId  assignedBranchId}
          br-list (:centres @ALL_CENTRES)
          br-list2 (remove-id-from-list id br-list)
          br-list3 (conj br-list2 br-obj)
          br-yaml-obj {:centres br-list3}
          _ (reset! ALL_CENTRES br-yaml-obj)]
      br-obj)))

(defn create-update-centre-safe [centre-obj]
  (get-all-centres)
  (create-update-centre centre-obj)
  (save-updates!))

(comment
  (api/setenv "env17") ;; SEUKDEMO
  (get-all-centres)
  (call-api get-centre-api {:id "centre1"})
  (does-centre-exist? "centre1")
  @ALL_CENTRES
  (save-updates!)

  (create-update-centre {:id "centre2",
                         :name "centre2",
                         :state "ACTIVE",
                         :notes "",
                         :assignedBranchId "mk-branch",
                         :address {}})

 ;; 
  )

