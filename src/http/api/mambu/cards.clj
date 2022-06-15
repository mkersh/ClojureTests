(ns http.api.mambu.cards
  (:require [http.api.json_helper :as api]))

(defn list-cards [accid]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" "FULL"}}
        url (str "{{*env*}}/deposits/" accid "/cards")]
    (api/PRINT (api/GET url options))))


(defn link-card [accid card-token]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body {"referenceToken" card-token}}
        url (str "{{*env*}}/deposits/" accid "/cards")]
    (api/PRINT (api/POST url options))))

(defn unlink-card [accid card-token]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}}
        url (str "{{*env*}}/deposits/" accid "/cards/" card-token)]
    (api/PRINT (api/DELETE url options))))


(defn create-hold [card-token amount transRef & [no-print & _]]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body {"externalReferenceId" transRef
                        "amount" amount
                        "advice" false
                        "cardAcceptor" {"zip" "zipCode"
                                        "country" "CountryStr"
                                        "city" "CityStr"
                                        "name" "Merchant Name"
                                        "state" "State"
                                        "mcc" 77}
                        "userTransactionTime" "11:10:15"
                        "currencyCode" "EUR"}}
        url (str "{{*env*}}/cards/" card-token "/authorizationholds")]
    (if no-print (api/POST url options) (api/PRINT (api/POST url options)))))

(defn increase-hold [card-token amount transRef]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body {"amount" amount}}
        url (str "{{*env*}}/cards/" card-token "/authorizationholds/" transRef ":increase")]
    (api/PRINT (api/POST url options))))

(defn decrease-hold [card-token amount transRef]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body {"amount" amount}}
        url (str "{{*env*}}/cards/" card-token "/authorizationholds/" transRef ":decrease")]
    (api/PRINT (api/POST url options))))


(defn list-holds [accid & opt-overrides]
  (let [moreOpts (first opt-overrides)
        status (:status moreOpts) 
        options {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" "FULL"
                                "status" status}}
        url (str "{{*env*}}/deposits/" accid "/authorizationholds")]
    (api/PRINT (api/GET url options))))

(defn delete-hold [card-token transRef]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}}
        url (str "{{*env*}}/cards/" card-token "/authorizationholds/" transRef)]
    (api/PRINT (api/DELETE url options))))

(defn get-hold [card-token transRef & opt-overrides]
  (let [moreOpts (first opt-overrides)
        options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}}
        url (str "{{*env*}}/cards/" card-token "/authorizationholds/" transRef)]
    (api/PRINT (api/GET url options) moreOpts)))

(defn get-num [strNum]
  (BigDecimal. strNum))

(defn zero-hold [card-token transRef]
  (let [amount (get-num (get (get-hold card-token transRef {:no-print true}) "amount"))
        near-zero (- amount 0.0000000001)]
    (decrease-hold card-token near-zero transRef)))

(defn settle-transaction [card-token amount transRef]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"}
                 :query-params {}
                 :body {"amount" amount
                        "advice" false
                        "externalAuthorizationReferenceId" transRef
                        "externalReferenceId" transRef
                        "transactionChannelId" "8a818e74677a2e9201677ec2b4c336a6"}}
        url (str "{{*env*}}/cards/" card-token "/financialtransactions")]
    (api/PRINT (api/POST url options))))

(defn create-transaction
  ([card-token amount transRef]
   (create-transaction card-token amount transRef false))
  ([card-token amount transRef advice]
   (let [tref (or transRef (api/uuid))
         options {:headers {"Accept" "application/vnd.mambu.v2+json"
                            "Content-Type" "application/json"}
                  :query-params {}
                  :body {"amount" amount
                         "advice" advice
                         "notes" "Note associated with the transaction"
                         "externalReferenceId" tref
                         "transactionChannelId" "8a818e74677a2e9201677ec2b4c336a6"}}
         url (str "{{*env*}}/cards/" card-token "/financialtransactions")]
     (api/PRINT (api/POST url options)))))

; Test in your REPL: Select line to run ctl+alt+c <space>
; Use api/find-path and api/extract-attrs to navigate through results
(comment
  ;; Setup which Mambu environment you are working against
  (api/setenv "env1")

  (time (list-cards "BUKO329"))
  (time (link-card "BUKO329" "token1"))
  (time (unlink-card "BUKO329" "token1"))



  (api/setenv "env1")
  (api/setenv "env2")
  (api/set-show-only true)
  (time (list-holds "BUKO329"))
  (time (list-holds "BUKO329" {:status "PENDING"}))

  (def transRef (api/uuid))
  (time (create-hold "token1" 169.87 transRef))
  (time (increase-hold "token1" 20.00 transRef))
  (time (decrease-hold "token1" 100.00 transRef))
  (time (delete-hold "token1" transRef))
  (time (zero-hold "token1" transRef))
  (time (get-hold "token1" transRef))

  (time (settle-transaction "token1" 99.13 transRef))

  ;; Create a realtime card financial transactions i.e no previous hold to match
  (time (create-transaction "token1" 377 nil))

  (* 10000 (* 30 (/ 0.05 360)))

  ;; ZMGF768
  (time (link-card "ZMGF768" "token3"))
  (time (unlink-card "ZMGF768" "token3"))
  (def transRef (api/uuid))
  (time (create-hold "token3" 1000 transRef))
  (time (increase-hold "token3" 20.00 transRef))

  ;; LTCA1 - env2
  (time (link-card "LTCA1" "token1506"))
  (time (create-hold "token1506" 500 transRef))
  (time (create-transaction "token1506" 333 nil))


  (defn perf-test [n]
    (let [start-time (System/currentTimeMillis)
          _ (doall (for [_ (range n)]
                     (let [transRef (api/uuid)]
                ;;(prn "create hold")
                       (create-hold "token1" 3.87 transRef true))))
          end-time (System/currentTimeMillis)]
      (prn "Total Time (millisecs): " (- end-time start-time))))

  (perf-test 100)

  (/ 100 15.019)
;
  )