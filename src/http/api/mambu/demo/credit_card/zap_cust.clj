(ns http.api.mambu.demo.credit_card.zap_cust
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps])
)

;; *** [1] Functions to remove deposit accounts

(defn deposit-to-account [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/deposit-transactions")
   :method api/POST
   :query-params {}
   :body {"transactionDetails" {"transactionChannelId" (:deposit-channel context)}
          "amount" (:deposit-amount context)
          "notes" "some notes"
          ;;"paymentOrderId" (:deposit-ID context)
          ;;"externalId" (:deposit-ID context)
          }
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn withdraw-from-account [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/withdrawal-transactions")
   :method api/POST
   :query-params {}
   :body {"transactionDetails" {"transactionChannelId" (:deposit-channel context)}
          "amount" (:deposit-amount context)
          "notes" "some notes"
          ;;"paymentOrderId" (:deposit-ID context)
          ;;"externalId" (:deposit-ID context)
          }
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn delete-account [context]
  {:url (str "{{*env*}}/deposits/" (:accid context))
   :method api/DELETE
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn close-account [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) ":changeState")
   :method api/POST
   :query-params {}
   :body {"action" "CLOSE"
          "notes" "Close FD deposit"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-account [context]
  {:url (str "{{*env*}}/deposits/" (:accid context))
   :method api/GET
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(defn get-all-accounts [context]
  {:url (str "{{*env*}}/deposits/")
   :method api/GET
   :query-params {"accountHolderType" "CLIENT"
                  "accountHolderId" (:cust-key context)}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn is-open-dep? [resList accObj]
  (let [state (get accObj "accountState")
        accid (get accObj "id")
        _ (prn "is-open-dep?" accid)
        ]
    (if (and (not= state "CLOSED") (not= state "WITHDRAWN"))
      (conj resList accid)
      resList)))

(defn get-all-open-dep-accounts [context]
  (let [context1 (steps/apply-api get-all-accounts context)
        _ (prn "get-all-open-dep-accounts 11")
        accList (api/get-attr context1 [:last-call])
        _ (prn "get-all-open-dep-accounts 22" accList)
        res (reduce is-open-dep? [] accList)
        _ (prn "get-all-open-dep-accounts " res)
        ]
    res
    ))

(defn withdraw-and-close [context]
  (let [
        context1 (assoc context :deposit-amount 1.0)
        _  (steps/apply-api deposit-to-account context1)
        accObj (steps/apply-api get-account context)
        amount (api/get-attr accObj [:last-call "balances" "totalBalance"])
        context2 (assoc context :deposit-amount amount)
        context3 (steps/apply-api withdraw-from-account context2)]
    (steps/apply-api close-account context3)))

(defn remove-dep-account [context0]
  (prn (str "Remove accid " (:accid context0)))
  (let [context (assoc context0 :throw-errors true)]
    (try
      (steps/apply-api delete-account context)
      (catch Exception _
        (try
          (steps/apply-api close-account context)
          (catch Exception _ (withdraw-and-close context)))))))

(defn remove-all-open-dep-accounts [context]
  (prn "remove-all-open-dep-accounts")
  (let [accidList (get-all-open-dep-accounts context)]
    (for [accid accidList]
      (let [context1 (assoc context :accid accid)]
        (remove-dep-account context1)))))

;; *** [2] Functions to remove loan accounts

(defonce LOANID (atom nil))

(defn disburse-loan-api [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) "/disbursement-transactions")
   :method api/POST
   :body {"valueDate" (:value-date context)
          "firstRepaymentDate" (:first-date context)
          "amount" 0.01 ;; Need to specify the amount for RCA
          "notes" "Disbursement from clojure"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn approveLoanAccount [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) ":changeState")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"action" "APPROVE"
          "notes" "Approved from the API"}})

(defn writeoffLoanAccount [context]
  {:url (str "{{*env*}}/loans/" (:loanAccountId context) ":writeOff")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {}})

(defn get-all-loans-api [context]
  {:url (str "{{*env*}}/loans")
   :method api/GET
   :query-params {"detailsLevel" "BASIC"
                  "accountHolderType" "CLIENT"
                  "accountHolderId" (:cust-key context)
                  "accountState" (:status context)}

   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(defn zap-a-loan []
  (try (steps/apply-api approveLoanAccount {:loanAccountId @LOANID}) (catch Exception _ nil))
  (try (steps/apply-api disburse-loan-api {:loanAccountId @LOANID}) (catch Exception _ nil))
  (try (steps/apply-api writeoffLoanAccount {:loanAccountId @LOANID}) (catch Exception _ nil)))

(defn zap-all-loans-aux [acc-list]
  (doall
   (map
    (fn [obj]
      (reset! LOANID (get obj "id"))
      (prn "Zapping: " @LOANID)
      (zap-a-loan))
    acc-list)))

(defn zap-all-loans [cust-key]
  (let
   [all-loans   (api/extract-attrs ["id"] (:last-call (steps/apply-api get-all-loans-api {:cust-key cust-key})))
    ;;active-list (api/extract-attrs ["id"] (:last-call (steps/apply-api get-all-loans-api {:cust-key cust-key :status "ACTIVE"})))
    ;;active-in-arrears-list (api/extract-attrs ["id"] (:last-call (steps/apply-api get-all-loans-api {:cust-key cust-key :status "ACTIVE_IN_ARREARS"})))
    ]
    (prn "Zapping all loans")
    (zap-all-loans-aux all-loans)
    ;; (prn "Zapping ACTIVE loans")
    ;; (zap-all-loans-aux active-list)
    ;; (prn "Zapping ACTIVE_IN_ARREARS loans")
    ;; (zap-all-loans-aux active-in-arrears-list)
    ))

;; *** [3] Functions to remove credit-arrangement

(defn delete-ca [context]
  {:url (str "{{*env*}}//creditarrangements/" (:caid context))
   :method api/DELETE
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn close-ca [context]
  {:url (str "{{*env*}}/creditarrangements/" (:caid context) ":changeState")
   :method api/POST
   :query-params {}
   :body {"action" "CLOSE"
          "notes" "Close FD deposit"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn find-CA-accounts [context]
  {:url (str "{{*env*}}/clients/" (:custid context) "/creditarrangements")
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"filterCriteria" [{"field" "parentAccountKey"
                             "operator" "EQUALS"
                             "value" (:acckey context)}]}})

(defn remove-CA-account [context0]
  (let [context (assoc context0 :throw-errors true)
        _ (prn (str "Remove CA " context))]
    (try
      (steps/apply-api delete-ca context)
      (catch Exception _
        (steps/apply-api close-ca context)))))

(defn remove-all-CAs [context]
  (let [accidList (:last-call (steps/apply-api find-CA-accounts context))]
    (for [acc accidList]
      (let [caid (get acc "id")
          context1 (assoc context :caid caid)]
        (remove-CA-account context1)))))

;; *** [4] Functions to remove customer and linked accounts

(defn find-all-customers-with-name [context]
  {:url (str "{{*env*}}/clients:search")
   :method api/POST
   :query-params {}
   :body {"filterCriteria" [{"field" "firstName"
                             "operator" "EQUALS"
                             "value" (:first-name context)}]}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn exit-customer [context]
  {:url (str "{{*env*}}/clients/" (:custid context))
   :method api/PUT
   :query-params {}
   :body { 
        "id" (:custid context)
        "firstName" "**DELETED**"
        "lastName" "**ZAPPED**"
        "encodedKey" (:cust-key context)
       "state" "EXITED"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; Another way of doing the exit using a PATCH
(defn exit-customer2 [context]
  {:url (str "{{*env*}}/clients/" (:custid context))
   :method api/PATCH
   :query-params {}
   :body [{"op" "ADD"
           "path" "state"
           "value" "EXITED"}]
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn zap-cust [context]
  (zap-all-loans (:cust-key context))
  (doall (remove-all-open-dep-accounts context))
  (doall (remove-all-CAs context))
  (steps/apply-api exit-customer context))

;; zap all customners with firstName == (:first-name context)
(defn zap-all-matching-name [context]
  (let [cust-list  (:last-call (steps/apply-api find-all-customers-with-name context))]
    (map (fn [cust-obj]
           (let [cust-key (get cust-obj "encodedKey")
            _ (prn "HERE" cust-obj)
                 custid (get cust-obj "id")
                 fname (get cust-obj "firstName")
                 lname (get cust-obj "lastName")
                 _ (prn (str "Zapping customer " fname " " lname))]
             (zap-cust {:cust-key cust-key :custid custid})))
         cust-list)))

(comment
  (api/setenv "env2")
  (zap-cust {:cust-key "8a818e9c8053196101805cf1f8e2344e" :custid "035625552"})
  (zap-all-loans "8a818e9c8053196101805bf9e0dc259b")
  (remove-all-open-dep-accounts {:cust-key "8a818fde805319bc01805cf2e178332c"})
  (get-all-open-dep-accounts {:cust-key "8a818e7680528bf601805c08e87144bf"})
  (remove-dep-account {:accid "IVSI570"})
 (remove-all-CAs {:custid "106607775"})
  (steps/apply-api find-CA-accounts {:custid "106607775"})
  (steps/apply-api exit-customer2 {:custid "106607775"})
  (steps/apply-api find-all-customers-with-name {:first-name "Apr24"})
  
  (zap-all-matching-name {:first-name "Apr24"})


  (steps/apply-api deposit-to-account {:accid "IXPS551" :deposit-amount 1.0})
  (steps/apply-api delete-account {:accid "IXPS551"})
  (steps/apply-api close-account {:accid "IXPS551"})
  (steps/apply-api delete-ca {:caid "WOY524"})
  (steps/apply-api close-ca {:caid "WOY524"})
  (steps/apply-api find-CA-accounts {:custid "922796880"})
  (steps/apply-api get-all-accounts {:cust-key "8a818e9c8053196101805bf9e0dc259b"})
  (steps/apply-api get-all-loans-api {:cust-key "8a818e9c8053196101805bf9e0dc259b"})

  (for [acc (get-all-open-dep-accounts {:cust-key "8a818e7680528bf601805c08e87144bf"})]
    (prn "acc" acc))

  )