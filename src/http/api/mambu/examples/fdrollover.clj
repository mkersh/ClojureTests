;;; #bookmark= c8a38eb2-5fec-4d04-b994-0ad5c007b38d
;;;
(ns http.api.mambu.examples.fdrollover
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.string :as str]))

(defn getall-custom-field-sets [_]
  {:url (str "{{*env*}}/customfieldsets")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn getall-fd-mature-today [_]
  {:url (str "{{*env*}}/savings/search")
   :method api/POST
   :query-params {}
   :body {"filterConstraints" [{"filterSelection" "MATURITY_DATE"
                                "filterElement" "TODAY"}
                               {"filterElement" "IN"
                                "filterSelection" "ACCOUNT_STATE"
                                "values" ["ACTIVE"]}]}
   :headers {"Content-Type" "application/json"}})

(defn create-fd-account [context]
  {:url (str "{{*env*}}/deposits")
   :method api/POST
   :query-params {}
   :body {"accountType" "FIXED_DEPOSIT"
          "name" "FD Rollover - Monthly"
          "accountHolderKey" (:custid context)
          "productTypeKey" (:prodid context)
          "currencyCode" (:currency context)
          "accountHolderType" "CLIENT"
          "interestSettings"
          {"interestRateSettings"
           {"interestChargeFrequency" "ANNUALIZED"
            "interestChargeFrequencyCount" 1
            "interestRate" 2.54
            "interestRateTerms" "FIXED"
            "interestRateTiers" []}}}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn approve-account [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) ":changeState")
   :method api/POST
   :query-params {}
   :body {"action" "APPROVE"
          "notes" "more notes"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn deposit-into-account [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/deposit-transactions")
   :method api/POST
   :query-params {}
   :body {"transactionDetails" {"transactionChannelId" (:deposit-channel context)}
          "amount" (:deposit-amount context)
          "notes" "some notes"
          "paymentOrderId" (:deposit-ID context)
          "externalId" (:deposit-ID context)
          "valueDate" (:start-date context)}
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

(defn fd-start-maturity [context]
  {:url (str "{{*env*}}/savings/" (:accid context) "/transactions")
   :method api/POST
   :query-params {}
   :body {"type" "START_MATURITY"
          "date" (:maturity-date context)
          "notes" "start maturity for FD"}
   :headers {"Content-Type" "application/json"}})

;;; Next function is pretty useless for most account
;;; Once you've activated an account you can no longer delete
;;; Use :throw-errors so that the status will be thrown
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
                  "accountHolderId" (:custid context)}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn is-open-fd? [resList accObj]
  (let [state (get accObj "accountState")
        accid (get accObj "id")]
    (if (and (not= state "CLOSED") (not= state "WITHDRAWN"))
      (conj resList accid)
      resList)))

(defn get-all-open-fd-accounts [context]
  (let [context1 (steps/apply-api get-all-accounts context)
        accList (api/get-attr context1 [:last-call])]
    (reduce is-open-fd? [] accList)))

(defn withdraw-and-close [context]
  (let [accObj (steps/apply-api get-account context)
        amount (api/get-attr accObj [:last-call "balances" "totalBalance"])
        context2 (assoc context :deposit-amount amount)
        context3 (steps/apply-api withdraw-from-account context2)]
    (steps/apply-api close-account context3)))

(defn remove-fd-account [context0]
  (let [context (assoc context0 :throw-errors true)]
    (try
      (steps/apply-api delete-account context)
      (catch Exception _
        (try
          (steps/apply-api close-account context)
          (catch Exception _ (withdraw-and-close context)))))))

(defn remove-all-open-fd-accounts [context]
  (let [accidList (get-all-open-fd-accounts context)]
    (for [accid accidList]
      (let [context1 (assoc context :accid accid)]
        (remove-fd-account context1)))))

;;; This is the top-level call to go through all the step to
;;; create a FD account, approve it, start maturity
(defn main-create-fd-process [context]
  {:context context
   :Xjump-to-step [:step2 :one-only] ; set ":id :jump-here" in a step 
   :steps [;; [STEP-1] Create FD Account
           {:request create-fd-account
            :post-filter [(steps/save-part-to-context ["id"] :accid)]}
         ;; [STEP-2] Approve Account
           {:request  approve-account}
         ;; [STEP-3] Deposit Money into account
           {:pre-filter [(steps/save-value-to-context (api/uuid) :deposit-ID)]
            :request  deposit-into-account}
         ;; [STEP-4] Start Maturity
           {:request  fd-start-maturity}]})

(defn today-date []
  (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") (new java.util.Date)))

(defn year-ago-date []
  (let [todayStr (today-date)
        parts (str/split todayStr #"-")
        year (first parts)
        month (second parts)
        day (nth parts 2)]
    (str (- (read-string year) 1) "-" month "-" day)))

(defn today-date+1 []
  (let [todayStr (today-date)
        parts (str/split todayStr #"-")
        year (first parts)
        month (second parts)
        day (nth parts 2)]
    (str year "-" month "-" (+ (read-string day) 1))))

(defn update-maturity-instruction [context]
  {:url (str "{{*env*}}/deposits/" (:accid context))
   :method api/PATCH
   :query-params {}
   :body [{"op" (:add_or_remove context)
           "path" (str "/" (:maturity_dataset context) "/" (:maturity_field context))
           "value" (:maturity_value context)}]
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn setup-maturity-instructions [context]
  (let [accid (:accid context)
        maturity_dataset (:maturity_dataset context)
        maturity_instructions (:maturity_instructions context)]
    (for [instr maturity_instructions]
      (let [context1 (assoc instr :maturity_dataset maturity_dataset)
            context2 (assoc context1 :accid accid)
            addOrRemove (if (= (:maturity_value instr) "") "remove" "add")
            context3 (assoc context2 :add_or_remove addOrRemove)
            ]
        (steps/apply-api update-maturity-instruction context3)))))

(defn MPO-process-run-api [_context]
  {:url (:url (api/get-env-item "env19")) ;; Start new MPO TASK
   :method api/POST
   :body {"msg" "started from code script"}
   :headers {"Content-Type" "application/json"}})

(defn run-MPO-Term-Deposit-Rollover-EOD-process []
  (steps/call-api MPO-process-run-api {}))

(defn maturityInstr1 [accid]
  {:accid accid
   :maturity_dataset "_Rollover_Instructions"
   :maturity_instructions
   [{:maturity_field "Rollover_Y_N_Deposit_Accounts"
     :maturity_value "Yes"} ;; Yes | No 
    {:maturity_field "Rollover_Product_Deposit_Account"
     :maturity_value "Interest Paid Monthly"} ;; Interest Paid Monthly | Interest Paid at Maturity
    {:maturity_field "Rollover_Term_Deposit_Accounts"
     :maturity_value "3"} ;; 1 | 2 | 3 | 4 | 6 | 9 | 12
    {:maturity_field "Rollover_Amount_Deposit_Accounts"
     :maturity_value "Specify Rollover Amount"} ;; Original Principal without Interest | Original Principal with Interest | Specify Rollover Amount | Specify Payout Amount ;; "Original Principal with Interest"
    {:maturity_field "Specified_Rollover_Amount_Deposi"
     :maturity_value "123.01"} ;; Number - Rollover Amount
    {:maturity_field "Specify_Payout_Amount_Deposit_Ac"
     :maturity_value "100"} ;; Number - Payout Amount
    {:maturity_field "Payout_Account_Number_Deposit_Ac"
     :maturity_value "IBAN-PayoutAccount"}
    {:maturity_field "Payout_BSB_Number_Deposit_Accoun"
     :maturity_value "PayoutSortCode"}]})

(def fdsetupObj
  {;; API Pipe settings
   :show-only false ; when true only prints steps, doesn't execute
   :throw-errors false
     ;; Step1
   :custid "8a19ada3786e7b0701786edd9b4d0434"
   :prodid "8a19a9a6785b934201786a1c5bd30b8e" ;; monthly interest
   :currency "EUR"
     ;; step2
   :accid "URBG897" ;; This will be overwritten if you use main-create-fd-process
     ;; step3
   :deposit-channel "8a19ca06707c68530170a012971101d0"
   :deposit-amount 359.43
   :deposit-ID (api/uuid) ;; This will be overwritten if you use main-create-fd-process
   :start-date (str (year-ago-date) "T00:00:00+02:00")
     ;; step4
   :maturity-date (str (today-date+1) "T00:00:00+02:00")})
  
  (defonce LAST-FD-ACCID (atom nil))
  (defn create-ready-to-mature-FD []
    (let [res (steps/process-collection (main-create-fd-process fdsetupObj))
          new-fd-accid (get-in res [:last-call "id"])
          _ (reset! LAST-FD-ACCID new-fd-accid)]
      (prn (str "New (ready to mature) FD created " new-fd-accid))))

(api/setenv "env5") ;; EUShowCase

(comment
  
  ;; [1] TidyUp - Remove all FD accounts
  (remove-all-open-fd-accounts fdsetupObj)

  ;; [2] Create - a new FD in one call
  ;; See fdsetupObj for parameters used
  (create-ready-to-mature-FD)

  ;; [3] Setup Maturity Instructions on the FD account
  (setup-maturity-instructions (maturityInstr1 @LAST-FD-ACCID))
  @LAST-FD-ACCID
  (reset! LAST-FD-ACCID "PSRM783")

  ;; [4] run-MPO-Term-Deposit-Rollover-EOD-process
  ;; NOTE: This is hardcoded to 
  (run-MPO-Term-Deposit-Rollover-EOD-process)

 ;;
  )