(ns http.api.mambu.demo.credit_card.zap_cust
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps])
)

;; *** [1] Functions to remove deposit accounts

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
        accid (get accObj "id")]
    (if (and (not= state "CLOSED") (not= state "WITHDRAWN"))
      (conj resList accid)
      resList)))

(defn get-all-open-dep-accounts [context]
  (let [context1 (steps/apply-api get-all-accounts context)
        accList (api/get-attr context1 [:last-call])]
    (reduce is-open-dep? [] accList)))

(defn withdraw-and-close [context]
  (let [accObj (steps/apply-api get-account context)
        amount (api/get-attr accObj [:last-call "balances" "totalBalance"])
        context2 (assoc context :deposit-amount amount)
        context3 (steps/apply-api withdraw-from-account context2)]
    (steps/apply-api close-account context3)))

(defn remove-dep-account [context0]
  (let [context (assoc context0 :throw-errors true)]
    (try
      (steps/apply-api delete-account context)
      (catch Exception _
        (try
          (steps/apply-api close-account context)
          (catch Exception _ (withdraw-and-close context)))))))

(defn remove-all-open-dep-accounts [context]
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


(comment 
(api/setenv "env2")
(zap-all-loans "8a818e9c8053196101805bf9e0dc259b")
(remove-all-open-dep-accounts {:cust-key "8a818e9c8053196101805bf9e0dc259b"})


(steps/apply-api get-all-accounts {:cust-key "8a818e9c8053196101805bf9e0dc259b"})
(steps/apply-api get-all-loans-api {:cust-key "8a818e9c8053196101805bf9e0dc259b" })

)