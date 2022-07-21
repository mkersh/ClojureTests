(ns http.api.mambu.demo.workshop0722.interest_payaway.intpay
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.demo.loyalty_points :as lpd]
            [http.api.mambu.demo.credit_card.zap_cust :as zap]
            [http.api.mambu.examples.fdrollover :as dateh]))

(def call-api steps/call-api) ;; create an alias for this
(def round-num api/round-num)

(defonce LAST-PAYAWAY-TRANS (atom nil))

(defn get-all-trans-api [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/transactions")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

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

(defn transfer-transaction [context]
  {:url (str "{{*env*}}/deposits/" (:from-accid context) "/transfer-transactions")
   :method api/POST
   :query-params {}
   :body {"amount" (:amount context)
          "transferDetails" {"linkedAccountId" (:to-accid context)
                             "linkedAccountType" "DEPOSIT"}
          "notes" (:notes context)
          "paymentOrderId" (:deposit-ID context)
          "externalId" (:deposit-ID context)
          "valueDate" (:start-date context)
          }
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn reverse-transaction-api [context]
  {:url (str "{{*env*}}/deposits/transactions/" (:loanTransactionId context) ":adjust")
   :method api/POST
   :body {"notes" "Reversed PayAway Transaction"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; Get the total interest amount from a list of int-trans
(defn get-total-interest [int-trans]
  (reduce (fn [res tran-obj]
            (let [int-amount (get tran-obj "amount")
                  new-total (+ res int-amount)
                  ;;_ (prn (str "old-total=" res " int-amount= " int-amount " new-total=" new-total))
                  ]
              new-total)) 0 int-trans))

;; filter the trans-list to the transactions since the last-payaway
(defn get-trans-from-last-payaway [trans-list]
  (loop [trans-list1 trans-list
         res-list []]
    (let [tran-obj (first trans-list1)
          adjusted? (get tran-obj "adjustmentTransactionKey")
          notes (get tran-obj "notes")]
      (if (and (or (nil? tran-obj) (= notes "INTEREST-PAYAWAY-TRAN")) (not adjusted?))
        res-list
        (recur (rest trans-list1) (conj res-list tran-obj))))))

(defn check-non-adjusted []
  (fn [tran-obj]
    (let [adj-key (get tran-obj "adjustmentTransactionKey")]
      (nil? adj-key))))

(defn check-trans-type [match-type]
  (fn [tran-obj]
    (let [type1 (get tran-obj "type")]
      (= match-type type1))))

(defn get-interest-applied-trans [trans-list]
  (filter (check-trans-type "INTEREST_APPLIED") trans-list))

(defn get-non-adjusted-trans [trans-list]
  (filter (check-non-adjusted) trans-list))

(defn payaway-interest [total-interest payaway-obj]
  (let [from-acc (:from-acc payaway-obj)
        pay-type (:pay-type payaway-obj) ;; internal, external
        sortcode (:sortcode payaway-obj) ;; only when pay-type=external
        to-acc (:to-accid payaway-obj) ;; either an 8-digit-UK-AccID or a Mambu internal AccID (depending on pay-type)])
        notes "INTEREST-PAYAWAY-TRAN"]
    (if (> total-interest 0)
      (let [_ (prn "Paying this interest away using following instructions: " payaway-obj)
            res (if (= pay-type "external")
                  (do
                    (prn "Payout external")
                    (call-api withdraw-from-account {:accid from-acc :amount total-interest :deposit-ID (api/uuid) :notes notes}))
                  (do
                    (prn "Payout internal")
                    (call-api transfer-transaction {:from-accid from-acc :to-accid to-acc :amount total-interest :deposit-ID (api/uuid)  :notes notes})))
           
            trans-key (get res "id")
            _ (reset! LAST-PAYAWAY-TRANS trans-key) ;; remember the payaway transaction
            ]
        (prn "Transaction ID = " trans-key)
        )
      (prn "No interest to payaway!!"))    
    
    ))

;; This function should get triggered whenever an interest-applied ttransaction is applied to a savings account
(defn apply-interest-payaway [payaway-obj]
  (let [from-acc (:from-acc payaway-obj)
        trans-all (call-api get-all-trans-api {:accid from-acc})
        trans-since-last (get-trans-from-last-payaway trans-all)
        int-applied-list0 (get-interest-applied-trans trans-since-last)
        int-applied-list (get-non-adjusted-trans int-applied-list0)
        _ (prn "int-applied-list count: " (count int-applied-list))
        total-interest (bigdec (round-num (get-total-interest int-applied-list)))]
  
  (prn "Total interest to apply is" total-interest)
 
  (payaway-interest total-interest payaway-obj)
  ))

(api/setenv "env17")
(comment
  ;; [1] apply-interest-payaway
  ;; [1.1] payaway externally
  (apply-interest-payaway {:from-acc "IntPay2" :pay-type "external" :sortcode "123456" :to-accid "87654321"})
  ;; [1.2] payaway locally to another Mambu account
  (apply-interest-payaway {:from-acc "IntPay2" :pay-type "internal" :sortcode "123456" :to-accid "DPJE901"})

  ;; [2] Call the next function if you want to reverse the last apply-interest-payaway
  (call-api reverse-transaction-api {:loanTransactionId @LAST-PAYAWAY-TRANS})
  @LAST-PAYAWAY-TRANS

  ;; Test/debug 
(call-api transfer-transaction {:from-accid "IntPay2" :to-accid "DPJE901":amount 500.22 :notes "Some notes" :deposit-ID (api/uuid)})

  (call-api get-all-trans-api {:accid "IntPay2"})
  (get-interest-applied-trans (call-api get-all-trans-api {:accid "IntPay2"}))



  ;;
  )
