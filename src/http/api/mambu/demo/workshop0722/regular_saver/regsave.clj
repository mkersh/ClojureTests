;; POC for having a bonus account linked to a RegSaver account:
;; (1) Deposits+Withdrawals reflected in the bonus-account
;;     This reflecton is done in an MPO application 
;; (2) If certain T&Cs are met then the bonus is added at the end of each period
;;
;; #bookmark= d99edf2b-e03d-41c0-be1c-4fa82dfd7f3b
(ns http.api.mambu.demo.workshop0722.regular-saver.regsave
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.demo.loyalty_points :as lpd]
            [http.api.mambu.demo.credit_card.zap_cust :as zap]
            [http.api.mambu.examples.fdrollover :as dateh]
            ))


(defonce REGSAVER-PROD-KEY (atom "8a19c1c8821634d6018217502f27642d"))
(defonce BONUS-PROD-KEY (atom "8a19c1c8821634d6018217502f276430"))
(defonce REGSAVER-ACCID (atom nil))
(defonce BONUS-ACCID (atom nil))
(defonce CUSTID (atom nil))

(defn call-api
  ([api context attr-to-return]
   (let [res (call-api api context)
         val (get res attr-to-return)]
     val))
  ([api context]
  (let  [res (steps/apply-api api context)]
    (:last-call res))
   
   ))

(defn get-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-deposit-product-api [context]
  {:url (str "{{*env*}}/depositproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn search-savings-api [context]
  {:url (str "{{*env*}}/savings/search")
   :method api/POST
   :body {"filterConstraints" [{"filterSelection" "ACCOUNT_HOLDER_KEY",
                                "filterElement" "EQUALS",
                                "value" (:cust-key context)}
                               {"filterSelection" "PRODUCT_KEY",
                                "filterElement" "EQUALS",
                                "value" (:prodid context)}]}
   :headers {"Content-Type" "application/json"}})

(defn createSavingsAccount [context]
  {:url (str "{{*env*}}/deposits")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {
          "accountType" "CURRENT_ACCOUNT"
          "name" (:acc-name context)
          "accountHolderKey" (:cust-key context)
          "productTypeKey" (:product-key context)
          "currencyCode" "GBP"
          "accountHolderType" "CLIENT"}})

(defn get-client-api [context]
  {:url (str "{{*env*}}/clients/" (:custid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn zap-cust [context]
 (let  [cust-key (get (call-api get-client-api context) "encodedKey")
        _ (prn "cust-key" cust-key)
        context1 (assoc context :cust-key cust-key)]
  (doall (zap/remove-all-open-dep-accounts context1))
  (steps/apply-api zap/exit-customer2 context1)))

;; A collection of steps     
(defn create-regular-saver-customer-and-account [num]
  {:context {:show-only false ; when true only prints steps, doesn't execute
             :verbose true ;; print out step :label(s)
             :first-name "RegSaver", :last-name (str "Tester " num)
             :branchid "8a19c1c8821634d60182174d21876027"
             ;; **** Debug Only settings next - allows you to jump to individual steps
             :cust-key "8a19c1c8821634d6018217a3282e5798"
             :regsaver-id "OXVX863"
             :bonus-id "GSOT125"
             :custid "861492689"
             }
   :xjump-to-step [:step2 :one-only] ; set ":id :jump-here" in a step 
   :steps [;; [STEP-1] Create Customer
           {:label "[STEP-1] Create Customer"
            :request lpd/create-customer
            :post-filter [;(steps/save-last-to-context :cust-create)
                          (steps/save-part-to-context ["encodedKey"] :cust-key)
                          (steps/save-part-to-context ["id"] :custid)]}
           ;; [STEP-2] Create RegSaver
           {:label "[STEP-2] Create RegSaver"
           :id :step2
            :pre-filter [(steps/save-value-to-context @REGSAVER-PROD-KEY :product-key)
                         (steps/save-value-to-context "RegSaver" :acc-name)
                         ]
            :request createSavingsAccount
            :post-filter [(steps/save-part-to-context ["id"] :regsaver-id)]}

            ;; [STEP-3] Create Bonus Account
           {:label "[STEP-3] Create Bonus Account"
            :pre-filter [(steps/save-value-to-context @BONUS-PROD-KEY :product-key)
                         (steps/save-value-to-context "Bonus" :acc-name)]
            :request createSavingsAccount
            :post-filter [(steps/save-part-to-context ["id"] :bonus-id)]}


            ;; [STEP-7] Approve Accounts
           {:label "[STEP-4] Approve Accounts"
            :pre-filter [(steps/save-context-value-to-context :regsaver-id :accid)]
            :request lpd/approveDepositAccount}
           {:pre-filter [(steps/save-context-value-to-context :bonus-id :accid)]
            :request lpd/approveDepositAccount}]})

(defn create-new-regsaver-customer [num]
  (let [res-obj (steps/process-collection (create-regular-saver-customer-and-account num))
        regsaver-id (:regsaver-id res-obj)
        bonus-id (:bonus-id res-obj)
        custid (:custid res-obj)
        _ (reset! REGSAVER-ACCID regsaver-id)
        _ (reset! BONUS-ACCID bonus-id)
        _ (reset! CUSTID custid)]
    (prn (str "New Customer + RegSaver accounts (main + bonus) created - custid=" custid " main-account=" regsaver-id " bonus-account="bonus-id))))

(defn deposit-into-account [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/deposit-transactions")
   :method api/POST
   :query-params {}
   :body {"transactionDetails" {"transactionChannelId" (:deposit-channel context)}
          "amount" (:amount context)
          "notes" (:notes context)
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
          "amount" (:amount context)
          "notes" (:notes context)
          "paymentOrderId" (:deposit-ID context)
          "externalId" (:deposit-ID context)
          "valueDate" (:start-date context)}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn deposit-apply-interest-api [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) ":applyInterest")
   :method api/POST
   :body {"interestApplicationDate" (:application-date context),
          "isPaymentHolidaysInterest" (:is-payment-holiday-interest context),
          "notes" (:notes context),
          "paymentHolidaysInterestAmount" (:payment-holiday-interest-amount context)}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(defn get-all-trans-api [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/transactions")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-deposit-account-api [context]
  {:url (str "{{*env*}}/deposits/" (:accid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn check-trans-type [match-type]
  (fn [tran-obj]
    (let [type1 (get tran-obj "type")]
      (= match-type type1))))

(defn get-deposit-trans [trans-list]
  (filter (check-trans-type "DEPOSIT") trans-list))

(defn get-withdrawal-trans [trans-list]
(filter (check-trans-type "WITHDRAWAL") trans-list)
)

;; filter the trans-list to the transactions for the last-period
(defn get-trans-for-last-period [trans-list]
  (loop [trans-list1 trans-list
         res-list []]
    (let [tran-obj (first trans-list1)
          notes (get tran-obj "notes")]
      (if (or (nil? tran-obj) (= notes "LAST-PERIOD-END"))
        res-list
        (recur (rest trans-list1) (conj res-list tran-obj))))))

(defn make-deposits [num context]
  (for [i (range 1 (inc num))]
    (call-api deposit-into-account {:accid (:accid context) :start-date (:start-date context) :amount (+ 100.00 i) :deposit-ID (api/uuid)  :notes (str "bonus deposit " i)})))

(defn make-withdrawal [num context]
(for [i (range 1 (inc num))]
    (call-api withdraw-from-account {:accid (:accid context) :amount (+ 50.00 i) :deposit-ID (api/uuid)  :notes (str "bonus deposit " i)})))

(defonce BONUS-APPLICATION_DATE (atom (str (dateh/today-date) "T13:37:50+01:00")))
(defonce BONUS-DEPOSIT-MIN (atom 11))
(defonce BONUS-WITHDRAW-MAX (atom 1))
(defonce SIM-INTEREST_ACCRUE (atom 20.0)) ;; For demo allow bonus-interest to be simulated
(defonce BONUS-START-NEW-PERIOD (atom true))

(defn check-bonus-rules? [_bonus-accid deposits-count withdrawal-count]
  (and (>= deposits-count @BONUS-DEPOSIT-MIN) (<= withdrawal-count @BONUS-WITHDRAW-MAX)))

(defn apply-bonus [bonus-accid apply-bonus? application-date]
  (let [bonus-acc (call-api get-deposit-account-api {:accid bonus-accid})
        cust-key (get bonus-acc "accountHolderKey")
        search-res (call-api search-savings-api {:cust-key cust-key :prodid @REGSAVER-PROD-KEY})
        regsaver-accid (get-in search-res [0 "id"])
        interest-accrued0 (get-in bonus-acc ["accruedAmounts" "interestAccrued"])]

    ;; If the customer qualifies for a bonus add accrued-interest from the bonus-account into the reg-saver
    (when apply-bonus?
      (let [interest-accrued (if (and (= interest-accrued0 0) @SIM-INTEREST_ACCRUE)
                               (do (prn "Simulating bonus-interest") @SIM-INTEREST_ACCRUE) interest-accrued0)
            _ (prn "adding interest" regsaver-accid interest-accrued)
            _ (call-api deposit-into-account {:accid regsaver-accid :amount interest-accrued :deposit-ID (api/uuid)  :notes "BONUS-INTEREST-PAYMENT"})]
        (prn (str "Your bonus this period is: " interest-accrued))))

    ;; apply the accrued-interest (if there is any) - we will then be starting from 0 for the new period
    (when (> interest-accrued0 0)
      (call-api deposit-apply-interest-api {:accid bonus-accid :application-date application-date :notes "applying interest"}))

    ;; start a new period on the bonu-account
    (when @BONUS-START-NEW-PERIOD (call-api deposit-into-account {:accid bonus-accid :amount 0.01 :deposit-ID (api/uuid)  :notes "LAST-PERIOD-END"}))
      
    true
    ))
   

;; Next function will be triggered when interest-applied transaction happens on an reg-saver account 
;; It will analyse the transactions over the last period and determine is the bonus-interest should be given or not.
(defn check-apply-bonus-interest [bonus-accid application-date]
  (let [trans-all (call-api get-all-trans-api {:accid bonus-accid})
        last-period-trans (get-trans-for-last-period trans-all)
        ;;_ (prn "last-period-trans: " last-period-trans)
        deposit-trans (get-deposit-trans last-period-trans)
        deposits-count (count deposit-trans)
        withdrawal-trans (get-withdrawal-trans last-period-trans)
        withdrawal-count (count withdrawal-trans)]

    (if (check-bonus-rules? bonus-accid deposits-count withdrawal-count)
      (dosync
       (prn (str "Well done you are entitled to bonus!!!" "- You had " deposits-count " deposits and " withdrawal-count " withdrawals"))
       (apply-bonus bonus-accid true application-date))
      (dosync
       (prn (str "No bonus this period - You had " deposits-count " deposits and " withdrawal-count " withdrawals"))
       (apply-bonus bonus-accid false application-date)))))

(api/setenv "env17")
(comment

;; [1] Setup a new RegSaver customer
(create-new-regsaver-customer 9) ;; The number passed is used for the name ""RegSaver Tester n""
;; [1.1] Link to an existing account
(reset! CUSTID 865827243)
(reset! REGSAVER-ACCID "GEXA655")
(reset! BONUS-ACCID "PVSH208")
;; [1.2] Call next function to zap client and accounts
(zap-cust {:custid @CUSTID})
(zap-cust {:custid "841727156"})



;; [2] Apply a number of deposit + withdrawal transaction
;;     NOTE: You can also do this in the Mambu UI and show the MPO process that replicates the transactions to th bonus-account
(make-deposits 11 {:accid @REGSAVER-ACCID}) ;; make deposits on reg-saver account - MPO process will then replicate
(make-withdrawal 1 {:accid @REGSAVER-ACCID})

;; [3] At the end of a period (when interest-applied on main account) run the following function to check
;;     whether the customer has earned their bonus or not
(reset! BONUS-DEPOSIT-MIN 11)
(reset! BONUS-WITHDRAW-MAX 1)
(reset! BONUS-APPLICATION_DATE (str (dateh/today-date) "T13:37:50+01:00"))
(reset! BONUS-START-NEW-PERIOD true)
;; [3.1] Main function to call to check and apply interest
(check-apply-bonus-interest @BONUS-ACCID @BONUS-APPLICATION_DATE)
;; Here's the customer and account details
@REGSAVER-ACCID
@BONUS-ACCID
@CUSTID



;; ************************************************
;; Functions used during the debugging process
;;

;; Mark the end-of-period on the bonus-account. This will happen when check-apply-bonus-interest is called
(call-api deposit-into-account {:accid "UDYA819" :amount 20.0 :deposit-ID (api/uuid)  :notes "LAST-PERIOD-END" })

;; Check that apply-interest works
 (call-api deposit-apply-interest-api {:accid "UDYA819" :application-date "2022-07-20T13:37:50+01:00" :notes "applying interest"})

;; Product information
(call-api get-deposit-product-api {:prodid "RegSave1"} "encodedKey")
(call-api get-deposit-product-api {:prodid "RegSaveBonus1"} "encodedKey")
(reset! REGSAVER-PROD-KEY  "8a19c1c8821634d6018217502f27642d")
(reset! BONUS-PROD-KEY  "8a19c1c8821634d6018217502f276430")


(call-api get-client-api {:custid "420076816"} "encodedKey")
(call-api get-deposit-account-api {:accid "XLBS157"})

(call-api search-savings-api {:cust-key "8a19c1c8821634d601821a6a63d06832" :prodid "8a19c1c8821634d6018217502f27642d" })

(zap/get-all-open-dep-accounts {:cust-key "8a19c1c8821634d601821a6a63d06832" })

;; Below works
(call-api get-loan-product-api {:prodid "LP2"})

;;
)