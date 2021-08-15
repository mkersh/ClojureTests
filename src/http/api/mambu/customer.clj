
(ns http.api.mambu.customer
  (:require [http.api.json_helper :as api]))

(defn list-customers [& opt-overrides ]
  (let [moreOpts (first opt-overrides)
        detailLevel (or (:details-level moreOpts) "FULL")
        limitVal (or (:limit moreOpts) 50)
        offset (or (:offset moreOpts) 0)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" (or detailLevel "FULL")
                                "paginationDetails" "ON"
                                "limit" limitVal
                                "offset" offset}}
        options (merge optdefs moreOpts)
        url "{{*env*}}/clients"]
    (prn options)
    (api/PRINT (api/GET url options))))

(defn get-customer [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        detailLevel (or (:details-level moreOpts) "FULL")
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" detailLevel}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/clients/" id)]
    (api/PRINT (api/GET url options) moreOpts)))

(defn create-customer [& opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body  {"firstName" "Dominic"
                         "lastName" "Raab2"
                         "preferredLanguage" "ENGLISH"
                         "addresses" [{"country" "UK"
                                       "city" "Liverpool"}]
                         "notes" "Some Notes on this person"
                         "gender" "MALE"
                         "identificationDocumentTemplateKey" "8a81879867f40eff0167f45206e8002b"}}
        options (merge optdefs moreOpts)]
    (api/PRINT (api/POST "{{*env*}}/clients" options))))


(defn delete-customer [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {}}
        url (str "{{*env*}}/clients/" id)
        options (merge optdefs moreOpts)]
    (api/PRINT (api/DELETE url options))))

(defn put-customer [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body
                 {"creationDate" "2020-08-14T15:02:28+02:00"
                  "approvedDate" "2020-08-14T15:32:10+02:00"
                  "groupLoanCycle" 0
                  "preferredLanguage" "ENGLISH"
                  "lastName" "Raab2XXXXXYYYYY"
                  "id" id
                  "xid" "145566212"  ; Iterestingly you can change the id!!
                  "gender" "MALE"
                  "lastModifiedDate" "2020-08-14T12:14:13+02:00"
                  "firstName" "Jim999bbbbbbb"
                  "encodedKey" "8a81871173ec66260173ed29009d2cee"
                  "loanCycle" 0
                  "state" "INACTIVE"
                  "clientRoleKey" "8a818e74677a2e9201677ec2b4c336aa"
                  "notes" "Modified set of notes<br/>Hello world!!<br/>Another line
                                  <script>alert(1);</script>
                                  <p style= 'color:#FF0000';>Red paragraph text</p>
                                  "
                  "groupKeys" []}}
        url (str "{{*env*}}/clients/" id)
        options (merge optdefs moreOpts)]
    (api/PRINT (api/PUT url options))))

(defn patch-customer [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body [{"op" "ADD"
                         "path" "firstName"
                         "value" "Jim999BBBUUUU"}
                        ;; Next OP is an example of updating a custom field
                        ;; Change the op to remove to delete
                        {"op" "add"
                         "path" "/_MKExtraCustomer/MyCustomerField1"
                         "value" "Oh yes"}
                        {"op" "ADD"
                         "path" "gender"
                         "value" "MALE"}
                        {"op" "ADD"
                         "path" "This attribute doesn't exists"
                         "value" "FEMALEff"}]}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/clients/" id)]
    (api/PRINT (api/PATCH url options))))


(defn get-customer-encid
  "Return the encID associated with the id-or-obj passed in.
   id-or-obj can either be a customer ID, an encoded ID or a customer object."
  [id-or-obj & opt-overrides]
  (cond 
    (map? id-or-obj) (get id-or-obj "encodedKey")
    (= (count (str id-or-obj)) 32) id-or-obj
    :else (get (get-customer id-or-obj (merge (first opt-overrides) {:details-level "BASIC" :no-print true})) "encodedKey")))


; Test in your REPL: Select line to run ctl+alt+c <space>
; Use api/find-path and api/extract-attrs to navigate through results
(comment
  (api/setenv "env1")
  (def NewCustomerID "738211733") 
  (time (list-customers {:query-params {"detailsLevel" "BASIC"}}))
  (time (list-customers {:details-level "BASIC"}))
  (time (list-customers {:details-level "FULL" :limit 6 :offset 0}))
  (time (list-customers))

  (time (get-customer NewCustomerID {:no-print true}))
  (time (get-customer NewCustomerID {:details-level "FULL"}))
  (time (get-customer NewCustomerID {:details-level "BASIC"}))

  (get-customer-encid "756828242")

  (time (create-customer))
  (time (create-customer {:body  {"firstName" "Jane"
                                  "lastName" "Brown2"}}))

  (time (delete-customer NewCustomerID))

  (time (patch-customer NewCustomerID {:body [{"op" "ADD"
                                               "path" "firstName"
                                               "value" "1212121212121212"}]}))
  (time (patch-customer NewCustomerID))

  (time (put-customer "580959603" {:body {"creationDate" "2020-08-14T22:24:47+02:00"
                                          "idDocuments" []
                                          "groupLoanCycle" 0
                                          "preferredLanguage" "ENGLISH"
                                          "lastName" "Brownxx"
                                          "id" "580959603"
                                          "lastModifiedDate" "2020-08-14T22:24:47+02:00"
                                          "firstName" "Charles"
                                          "encodedKey" "8a81871173ec66260173eea5761e1d9e"
                                          "addresses" []
                                          "loanCycle" 0
                                          "state" "INACTIVE"
                                          "clientRoleKey" "8a818e74677a2e9201677ec2b4c336aa"}}))
  (time (put-customer NewCustomerID))

  (api/setenv "env3")
  (api/setenv "env2")
  (def NewCustomerID "795528765")
  (time (get-customer NewCustomerID {:details-level "FULL"}))
  
  
  )
