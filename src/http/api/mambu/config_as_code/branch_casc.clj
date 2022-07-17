(ns http.api.mambu.config-as-code.branch-casc
  (:require  [http.api.json_helper :as api]
             [http.api.api_pipe :as steps]
             [http.api.mambu.config-as-code.casc-helper :as casc]))


(defn call-api [api context]
  (:last-call (steps/apply-api api context)))

;; *****************************************************************
;; Mambu core config-as-code endpoints for branches
;;

(defn get-branch-yaml-api [context]
  {:url (str "{{*env*}}/configuration/branches.yaml")
   :query-params {}
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             "Content-Type" "application/yaml"}})

;; Update all custom-fields in a tenant
(defn put-branch-yaml-api [context]
  {:url (str "{{*env*}}/configuration/branches.yaml")
   :method api/PUT
   :body (:yaml-str context)
   :headers {"Accept" "application/vnd.mambu.v2+yaml"
             "Content-Type" "application/yaml"}})


(defonce ALL_BRANCHES (atom nil))

(defn get-all-branches []
  (let [branch-yaml (casc/get-yaml-response (call-api get-branch-yaml-api {}))
        br-obj (casc/yaml-str-to-edn branch-yaml)
        _ (reset! ALL_BRANCHES br-obj)]
    "Loaded all branches into @ALL_BRANCHES"))

;; This next function will update changes to your tenant
(defn save-updates! []
  (let [all-obj @ALL_BRANCHES
        yaml-str (casc/edn-to-yaml-str all-obj)
        tenant-env (api/get-env-domain)
        _  (prn (str "SAVING branches to " tenant-env))
        response-obj  (call-api put-branch-yaml-api {:yaml-str yaml-str})
        _ (prn "response - " response-obj)]
    (when all-obj (casc/get-yaml-response response-obj))))

;; *****************************************************************
;; Mambu core API for branch
;;

(defn get-branch-api [context]
  {:url (str "{{*env*}}/branches/" (:id context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; *****************************************************************
;; Higher level functions for manipulating branches
;;

(defn remove-id-from-list [id obj-list]
  (filter #(not (= id (:id %))) obj-list))


(defn create-update-branch [branch-obj]
  (let [id (:id branch-obj)
        name (:name branch-obj)
        state (:state branch-obj)
        phoneNumber (:phoneNumber branch-obj)
        emailAddress (:emailAddress branch-obj)
        notes (:notes branch-obj)
        address (:address branch-obj)
        holidays (:holidays branch-obj)]

    (assert (and id name state) "ERROR: create-new-branch - params missing")

    (let [br-obj {:id id :name name :state state :phoneNumber phoneNumber :emailAddress emailAddress :notes notes :address address :holidays holidays}
          br-list (:branches @ALL_BRANCHES)
          br-list2 (remove-id-from-list id br-list)
          br-list3 (conj br-list2 br-obj)
          br-yaml-obj {:branches br-list3}
          _ (reset! ALL_BRANCHES br-yaml-obj)]
      br-obj)))

(defn create-update-branch-safe [branch-obj]
  (get-all-branches)
  (create-update-branch branch-obj)
  (save-updates!))

(defn does-branch-exist? [branchid]
(let [res-obj (call-api get-branch-api {:id branchid})
      errors (get res-obj "errors")]
      (if errors nil res-obj)
      )
)

(comment
  (api/setenv "env17") ;; SEUKDEMO
  (get-all-branches)
  @ALL_BRANCHES
  (does-branch-exist? "mk-branch")
  (save-updates!)

  (create-update-branch {:id "mk-branch",
                      :name "mk-branch",
                      :state "INACTIVE",
                      :phoneNumber "",
                      :emailAddress "",
                      :notes nil,
                      :address nil,
                      :holidays nil})

 ;; 
  )

