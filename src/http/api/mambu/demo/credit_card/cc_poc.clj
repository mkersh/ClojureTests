(ns http.api.mambu.demo.credit_card.cc_poc
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]))


(defn list-cards-api [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/cards")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn link-card-api [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/cards")
   :method api/POST
   :body   {"referenceToken" (:card-token context)}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn add-card-trans-api [context]
  {:url (str "{{*env*}}/cards/" (:card-token context) "/financialtransactions")
   :method api/POST
   :body   {"amount" (:amount context)
            "advice" false
            "cardAcceptor" (:card-acceptor context)
            "externalReferenceId" (:trans_ref context)
            "transactionChannelId" (:channelid context)}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-all-trans-api [context]
  {:url (str "{{*env*}}/deposits/" (:accid context) "/transactions")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-all-unallocated-trans-api [context]
  {:url (str "{{*env*}}/deposits/transactions:search")
   :method api/POST
   :query-params {"detailsLevel" "FULL", "paginationDetails" "ON", "offset" 0 "limit" 20}
   :body {"filterCriteria" [{"field" "parentAccountKey"
                             "operator" "EQUALS"
                             "value" (:acckey context)}]
          "sortingCriteria" {"field" "id"
                             "order" "DESC"}}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-transaction-api [context]
  {:url (str "{{*env*}}/deposits/transactions:search")
   :method api/POST
   :query-params {"detailsLevel" "FULL"}
   :body {"filterCriteria" [{"field" "encodedKey"
                             "operator" "EQUALS"
                             "value" (:trans_id context)}]}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; Next doesn't work for card transactions - use reverse-card-trans-api instead
(defn revert-transaction-api [context]
  {:url (str "{{*env*}}/deposits/transactions/" (:trans_id context) ":adjust")
   :method api/POST
   :body {"notes" "Revert cards transaction after move"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn reverse-card-trans-api [context]
  {:url (str "{{*env*}}/cards/" (:card-token context) "/financialtransactions/" (:trans_ref context) ":decrease")
   :method api/POST
   :body {"amount" (:amount context)
          "externalReferenceId" (:trans_ref context)}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(defn create-installment-loan-api [context]
  {:url (str "{{*env*}}/loans")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :query-params {}
   :body  {"loanAmount" (:amount context)
           "loanName" (:acc-name context)
           "accountHolderKey" (:cust-key context)
           "productTypeKey" (:prod-key context)
           "accountHolderType" "CLIENT"
           "assignedBranchKey" (:branchid context)
           "interestFromArrearsAccrued" 0.0
           "interestSettings" {"interestSpread" (:interestspread context)}
           "scheduleSettings" {"fixedDaysOfMonth" [(:payment-day context)]
                               "gracePeriod" (:grace_period context)
                               "repaymentInstallments" (:num-installments context)}}})

(defn getCreditArrangement [context]
  {:url (str "{{*env*}}/clients/" (:cust-key context) "/creditarrangements")
   :method api/GET
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn addLoanToCreditLine [context]
  {:url (str "{{*env*}}/linesofcredit/" (:ca-id context) "/loans/" (:accid context))
   :method api/POST
   :headers {"Content-Type" "application/json"}})

(defn approveLoanAccount [context]
  {:url (str "{{*env*}}/loans/" (:accid context) ":changeState")
   :method api/POST
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}
   :body {"action" "APPROVE"
          "notes" "Approved from the API"}})

(defn search-for-loan-api [context]
  {:url (str "{{*env*}}/loans:search")
   :method api/POST
   :query-params {"detailsLevel" "FULL"}
   :body {"filterCriteria" [{"field" "accountHolderKey"
                             "operator" "EQUALS"
                             "value" (:cust-key context)}
                            {"field" "productTypeKey"
                             "operator" "EQUALS"
                             "value" (:prod-key context)}]}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


(defn disburse-rca-api [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/disbursement-transactions")
   :method api/POST
   :body {"amount" (:amount context)
          "bookingDate" (:bookdate context)
          "valueDate" (:bookdate context)
          "transactionDetails" {"transactionChannelKey" (:move_channel context)}
          "_credit-card-fields"
          {"cardAcceptor" (:card_acceptor context)
           "cardToken" (:card-token context)
           "externalReferenceId" (:trans_ref context)}}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn disburse-inst-api [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/disbursement-transactions")
   :method api/POST
   :body {"bookingDate" (:bookdate context)
          "valueDate" (:bookdate context)
          "transactionDetails" {"transactionChannelKey" (:move_channel context)}
          "_credit-card-fields"
          {"cardAcceptor" (:card_acceptor context)
           "cardToken" (:card-token context)
           "externalReferenceId" (:trans_ref context)}}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn add-context-parameter [context nested-fields name]
  (let [val (api/get-attr context nested-fields)]
    (assoc context name val)))

(defn add-context-parameter-filter [context nested-fields name filterFn]
  (let [val (api/get-attr context nested-fields)]
    (assoc context name (filterFn val))))

(defn move-cardtrans-to-rca [context]
;; Use card-acceptor details in :trans_details to determine whether to allocate to cash or purchases RCA bucket
;; TEMP: Will initially just allocate to purchases 
  (let [context1  (steps/apply-api search-for-loan-api context :loan_search)
        context2 (add-context-parameter context1 [:loan_search 0 "id"] :accid)
        _ (prn "*****BEFORE Disburse*** ")
        disburse  (steps/apply-api disburse-rca-api context2 :rca_disburse)]
    disburse))

(defn get-loan-type-settings [context]
  (let [alloc_type (:loan_type context)
        context1 (condp = alloc_type
                   "BNPL" (-> (add-context-parameter context [:bnpl_product :prod-key] :prod-key)
                              (add-context-parameter ,,, [:bnpl_product :acc-name] :acc-name)
                              (add-context-parameter ,,, [:bnpl_product :grace_period] :grace_period)
                              (add-context-parameter ,,, [:bnpl_product :num-installments] :num-installments))
                   "INSTAL" (-> (add-context-parameter context [:install_product :prod-key] :prod-key)
                                (add-context-parameter ,,, [:install_product :acc-name] :acc-name)
                                (add-context-parameter ,,, [:install_product :grace_period] :grace_period)
                                (add-context-parameter ,,, [:install_product :num-installments] :num-installments))
                   "PROM" (-> (add-context-parameter context [:prom_product :prod-key] :prod-key)
                              (add-context-parameter ,,, [:prom_product :acc-name] :acc-name)
                              (add-context-parameter ,,, [:prom_product :grace_period] :grace_period)
                              (add-context-parameter ,,, [:prom_product :num-installments] :num-installments)))]
    context1))

(defn move-cardtrans-to-ind-loan [context]
  (let [_ (prn "*****Start - move-cardtrans-to-ind-loan *** ")
        context1 (get-loan-type-settings context)
        context2 (steps/apply-api create-installment-loan-api context1 :rca_disburse)
        context3 (steps/apply-api getCreditArrangement context2 :ca_details)
        context4 (add-context-parameter context3 [:ca_details 0 "id"] :ca-id)
        context5 (add-context-parameter context4 [:rca_disburse  "id"] :accid)
        context6 (steps/apply-api addLoanToCreditLine context5 :add_to_ca)
        context7 (steps/apply-api approveLoanAccount context6 :approve_loan)
        context8 (steps/apply-api disburse-inst-api context7 :disburse_inst_loan)]
    context8))

(defn move-cardtrans-to-loan [context]

;; 1) - Get card details
;; 2) - Create new loan for transaction
;; 2.1) - Copy in the cardAcceptor as custom fields
;; 3) - reverse the original transaction
;; 3.1) - Need to be careful here to be able to differentiate these internal reversals from actual chargebacks
;; - Might be best to just deposit money back in rather than reverse
;; 
  (let [context1 (steps/apply-api get-transaction-api context :trans_details)
        alloc_type (:loan_type context)
        context2 (steps/apply-api get-transaction-api context1 :trans_details)
        context3 (add-context-parameter context2 [:trans_details 0 "valueDate"] :bookdate)
        context4 (add-context-parameter-filter context3 [:trans_details 0 "amount"] :amount #(- %)) ;; card trans will be negative, change to positive
        context5 (add-context-parameter-filter context4 [:trans_details 0 "cardTransaction" "cardAcceptor"] :card_acceptor api/to-json)
        context6 (add-context-parameter context5 [:trans_details 0 "cardTransaction" "cardToken"] :card-token)
        context7 (add-context-parameter context6 [:trans_details 0 "cardTransaction" "externalReferenceId"] :trans_ref)
        moved (if (= alloc_type "RCA")
                (move-cardtrans-to-rca context7)
                (move-cardtrans-to-ind-loan context7))
        _ (prn "*****BEFORE Reverse Card*** ")]
    (steps/apply-api reverse-card-trans-api moved :card_reverse)))

(defn get-allocated-list [alloc-list tran]
  (prn "***In get-allocated-list" alloc-list tran)
  (let [type (get tran "type")
        card_tran (get tran "cardTransaction")

        card_ref (get card_tran "externalReferenceId")
        _ (prn card_ref)
        _ (prn "**END")]
    (if (= type "CARD_TRANSACTION_REVERSAL")
      (assoc alloc-list card_ref true)
      alloc-list)))

(defn get-unallocated-list-maker [alloc-list]
  (fn [resList tran] (let [type (get tran "type")
                           card_tran (get tran "cardTransaction")
                           card_ref (get card_tran "externalReferenceId")]
                       (if (and (= type "WITHDRAWAL") (not (get alloc-list card_ref)))
                         (conj resList tran)
                         resList))))

(defn take-while-withdrawal [tran]
  (let [type (get tran "type")]
  (if (= type "WITHDRAWAL")
    true false)))


;;; Hitting problems linking a card CARD_TRANSACTION_REVERSAL to a previous WITHDRAWAL
;;; Was hoping to use this to determine which card transacions still need to be moved
;;; Its not working though because there is nothing to tie the two together (there is in UI though!!)
;;; See find-all-unallocated-trans for what I have settled on for MVP
(defn find-all-unallocated-trans2 [context]
  (let [context1 (steps/apply-api get-all-unallocated-trans-api context :trans_details)
        alloc-list (reduce get-allocated-list {} (:trans_details context1))
        unalloc-list (reduce (get-unallocated-list-maker alloc-list) [] (:trans_details context1))
        _ (prn "***RESULTS****:" alloc-list)
        _ (prn "***END****")]
    unalloc-list))

;;; In this versions get-all-unallocated-trans-api returns a list of most recent transactions first
;;; This function takes from the front until finding a transaction whose type is not WITHDRAWAL
;;; This is not ideal and we should find a better approach at some point 
(defn find-all-unallocated-trans [context]
  (let [context1 (steps/apply-api get-all-unallocated-trans-api context :trans_details)
        unalloc-list (take-while take-while-withdrawal (:trans_details context1))]
    unalloc-list))

(comment
  (api/setenv "env2")

  (def move-obj {:move_channel "8a818fc1768a3af801768b8020d14a63"
                 :cust-key "8a818ea47941c3e40179457f60f35789" 
                 ;; 8a818fdd768f6d970176908963cc3afb + 
                 :trans_id "8a818edb79417b5e017945808089284a" ; Card transaction to move
                 :loan_type "PROM" ; RCA | BNPL | INSTAL | PROM
                 :payment-day 1
                 :interestspread 0.0
                 :prod-key "8a818f5f6cbe6621016cbf3cf8675424" ; DEBUG only
                 :rca_cash_product "8a818f5f6cbe6621016cbf3cf8675424"
                 :rca_shop_product "8a818f5f6cbe6621016cbf6d66075e54"
                 :install_product {:prod-key "8a818f657688e84c017689f6a76a2f95" :acc-name "Install Loan" :grace_period 0 :num-installments 24}
                 :bnpl_product {:prod-key "8a818f657688e84c01768ad0da5c54dc" :acc-name "BNPL Loan" :grace_period 0 :num-installments 3}
                 :prom_product {:prod-key "8a818f657688e84c01768ade5b6361c2" :acc-name "Promo Loan" :grace_period 0 :num-installments 12}})

  (move-cardtrans-to-loan move-obj)
  ;;(move-cardtrans-to-rca move-obj)
  (steps/apply-api get-transaction-api move-obj :trans_details)

  (add-context-parameter  move-obj
                          [:rca_shop_product] :prod-key)

  (move-cardtrans-to-loan move-obj)
  (steps/apply-api search-for-loan-api move-obj :loan_search)


  (def loan-obj {:cust-key "8a818eef768ae48801768ee523e939dc"
                 :xacc-name "Installment Loan XXX"
                 :yacc-name "BNPL"
                 :acc-name "Prom Loan"
                 :amount 43.21
                 :branchid nil
                 :interestspread 0.0
                 :payment-day 1
                 :num-installments 4
                 :grace_period 12
                 :xprod-key "8a818f657688e84c017689f6a76a2f95" ; Installment loan
                 :yprod-key "8a818f657688e84c01768ad0da5c54dc" ; BNPL loan
                 :prod-key "8a818f657688e84c01768ade5b6361c2" ; Promotional loan - with 0% grace period
                 })
  (steps/apply-api create-installment-loan-api loan-obj :new_loan)

  (def card-acceptor
    {"city" "London"
     "country" "United Kingdom"
      ;; https://www.web-payment-software.com/online-merchant-accounts/mcc-codes/
     "mcc" 5655
     "name" "Sports Direct"
     "state" "London"
     "street" "Oxford Street"
     "zip" "S10 7EL"})

  (def card-obj {:accid "PMIW402"
                 :acckey "8a818e957941d0e90179427acc492dde"
                 :lastid 716
                 :card-token "cardtoken_0705_1"
                 :amount 17.14
                 :card-acceptor card-acceptor
                 :channelid "8a818e74677a2e9201677ec2b4c336a6"
                 :trans_ref (api/uuid)
                 :trans_id "8a818edb79417b5e017945808089284a"})
  
  ;; Link the "TEMP Card Account" to a card
  (steps/apply-api list-cards-api card-obj :card_list)
  (steps/apply-api link-card-api card-obj :link_card)

  ;; Add a new card transaction
  (steps/apply-api add-card-trans-api card-obj :card_trans)
  (steps/apply-api get-transaction-api card-obj :trans_details)
  (steps/apply-api get-all-trans-api card-obj :trans_details)
  (steps/apply-api get-all-unallocated-trans-api card-obj :trans_details)
  ;; This next one is the one to use. Not get-all-unallocated-trans-api
  (find-all-unallocated-trans card-obj)

  (steps/apply-api get-all-unallocated-trans-api card-obj :trans_details)
  ;;(steps/apply-api revert-transaction-api card-obj :trans_details)
  (steps/apply-api reverse-card-trans-api card-obj :trans_details)

  ;;
)

  
