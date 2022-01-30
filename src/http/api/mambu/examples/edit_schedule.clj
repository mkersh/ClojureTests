(ns http.api.mambu.examples.edit-schedule
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.api.mambu.experiments.loan_schedule :as ext]
            [java-time :as t]
            ))

(defonce NUM_MONTHS (atom 1))
(defn add-month [date1]
  (let [date1-local (t/local-date "yyyy-MM-dd" (subs date1 0 10))]
    (str (t/adjust date1-local t/plus (t/months @NUM_MONTHS)))))

(defn get-loan-schedule [context]
  {:url (str "{{*env*}}/loans/" (:accid context) "/schedule")
   :method api/GET
   :query-params {}
   :body {}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; See doc https://api.mambu.com/v1/index.html#loan-accounts-update-loan-repayments 
(defn edit-loan-schedule [context]
     {:url (str "{{*env*}}/loans/" (:accid context) "/repayments")
      :method api/PATCH
      :query-params {}
      :body (:body context)
      :headers {"Content-Type" "application/json"}})

(defn distribute-dates-instalments [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        start-date (atom (:start-date context))
        rep-list (mapv
                  (fn [instal-obj]
                    (let [result {"dueDate" @start-date
                     "encodedKey" (get instal-obj "encodedKey")
                     "parentAccountKey" (get instal-obj "parentAccountKey")}
                     _ (reset! start-date (add-month @start-date)) ;; Move date on for next one
                     ]
                     result
                     ))
                  sched-list)
        rep-body {"repayments" rep-list}
        context1 (assoc context :body rep-body)]
    (edit-loan-schedule context1)))

;; Function to change all the instalments after :num-instal to have the same dueDate as :num-instal
(defn reduce-to-n-instalments [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        last-instal (get sched-list (- (:num-instal context) 1))
        last-instal-date (subs (get last-instal "dueDate") 0 10) ;; strip the timezone away
        sched-list2 (subvec sched-list (:num-instal context))
        rep-list (mapv
                  (fn [instal-obj]
                    {"dueDate" last-instal-date 
                     "encodedKey" (get instal-obj "encodedKey")
                     "parentAccountKey" (get instal-obj "parentAccountKey")})
                  sched-list2)
        rep-body {"repayments" rep-list}
        context1 (assoc context :body rep-body)]
    (edit-loan-schedule context1)
    ))

(defn edit-principal-on-instalment [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        last-instal (get sched-list (- (:num-instal context) 1))
        rep-list  [{"principalDue" (:amount context)
                    "encodedKey" (get last-instal "encodedKey")
                    "parentAccountKey" (get last-instal "parentAccountKey")}]
        rep-body {"repayments" rep-list}
        context1 (assoc context :body rep-body)]
    (edit-loan-schedule context1)
    ))

;; Similar to edit-principal-on-instalment above but changes principal on multiple instalments 
;; Details of the instalments to change passed in via :instal-list
(defn edit-principal-on-instalments [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        instal-list (:instal-list context)
        rep-list  (mapv (fn [instal-obj]
                          (let [last-instal (get sched-list (- (:num-instal instal-obj) 1))]
                            {"principalDue" (:amount instal-obj)
                             "encodedKey" (get last-instal "encodedKey")
                             "parentAccountKey" (get last-instal "parentAccountKey")}))
                        instal-list)
        rep-body {"repayments" rep-list}
        ;;_ (pp/pprint rep-body)
        context1 (assoc context :body rep-body)]
    (edit-loan-schedule context1)))

;; Function to change all the instalments after :num-instal to be 0 and modify :num-instal to contain a final bullet
(defn reduce-to-n-instalments2 [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        last-install-num (:num-instal context)
        last-install-num-minus1 (- last-install-num 1)
        bullet-amount (reduce (fn [total item]
                                (let [prin-amount (get-in item ["principal" "amount" "due"])]
                                  (+ total prin-amount)))
                              0
                              (subvec sched-list last-install-num-minus1 (count sched-list)))
        changes0 (mapv (fn [i] {:num-instal i :amount 0}) (range (+ last-install-num 1) (count sched-list)))
        changes (into [] (cons {:num-instal last-install-num :amount bullet-amount} changes0))
        context1 (merge {:instal-list changes} context)]
    (edit-principal-on-instalments context1)))

(defn get-product-schedule-preview [context]
  {:url (str "{{*env*}}/loans:previewSchedule")
   :method api/POST
   :query-params {}
   :body {"disbursementDetails" {"expectedDisbursementDate" (:disbursement-date context)
                                 "firstRepaymentDate" (:first-payment-date context)}
          "loanAmount" (:amount context)
          "productTypeKey" (:template-product context)
          "interestSettings" {"interestRate" (:interest-rate context)},
          "scheduleSettings" {"gracePeriod" 0
                              "periodicPayment" (:periodic-amount context)
                              "repaymentInstallments" (:num-instalments context)
                              "repaymentPeriodCount" 1
                              "repaymentPeriodUnit" "MONTHS"}
          }
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})


;; Next function will copy the instalments from a product-schedule-preview to another loan account 
(defn copy-instalments-from-product-preview [context]
  (let [sched-list (get-in (steps/apply-api get-loan-schedule context) [:last-call "installments"])
        other-product-schedule (get-in (steps/apply-api get-product-schedule-preview context) [:last-call "installments"])
        _ (assert (= (count sched-list) (count other-product-schedule)) "ERROR: template-product has different number of instalments")
        changes (mapv (fn [i preview-obj]
                        {:num-instal i :amount (get-in preview-obj ["principal" "amount" "due"])})
                      ;; NOTE: Do not update the final instalment. Let Mambu calculate this
                      (range 1 (count sched-list)) other-product-schedule)
        ;;_ (pp/pprint changes)
        context1 (merge {:instal-list changes} context)
        ;;_ (pp/pprint context1)
        ]
    (edit-principal-on-instalments context1)))

;; Simple test of the edit-loan-schedule
;; NOTE: You need to change the encodedKey and parentAccountKey to relate to keys on your account
(defn test-edit-loan-schedule [context]
  (let [context1 (assoc context :body
                        {"repayments" [{"dueDate" "2022-02-04"
                                        "encodedKey" "8a818ef67d8012b4017d871724c14276"
                                        "parentAccountKey" "8a818ef67d8012b4017d871446da41af"
                                        "principalDue" 70.77}]})]
    (edit-loan-schedule context1)))





(defn get-account [context]
  {:url (str "{{*env*}}/loans/" (:accid context))
   :method api/GET
   :query-params {}
   :body {}
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
           "interestFromArrearsAccrued" 0.0
           "interestSettings" {"interestRate" (:interest-rate context)}
           "scheduleSettings" {"periodicPayment" (:periodic-payment context)
                               "gracePeriod" (:grace_period context)
                               "repaymentInstallments" (:num-installments context)}}})

(defn create-loan-account [accname options]
  (let [res (steps/apply-api create-installment-loan-api
                             {:cust-key (:cust-key options)
                              :prod-key (:prod-key options)
                              :amount (:amount options)
                              :periodic-payment (:periodic-payment options)
                              :acc-name accname
                              :interest-rate (:interest-rate options)
                              :grace_period (:grace_period options)
                              :num-installments (:num-installments options)})
        id (get-in res [:last-call "id"])]
    id))

;; [CONFIG] - Changes needed to get this library working your own tenant
;; Atoms with default values
;; Some of these are specific to a Mambu tenant and will need to be changed if you are trying to get this library 
;; working on your own tenant
(defonce CUSTKEY (atom "8a818f3f7e910785017e925d290745e3")) ;; 313566992
(defonce PRODKEY (atom "8a818ff17d470d02017d4808aaf217e9"))
(defonce PRODKEY_BULLET (atom "8a818e2a7d1e84c5017d1ec09e79013c"))
(defonce ACCID (atom nil)) ;; This atom gets set by the (create-new-loan ...)
(defonce ACCID_BULLET (atom nil)) ;; This atom gets set by the (create-new-bullet-loan ...)
(defonce NUM_INSTAL_AMORT (atom 20))
(defonce NUM_INSTAL_BULLET (atom 10))
(defonce AMOUNT (atom 10000.0))
(defonce PERIODIC_AMOUNT (atom 300.16))
(defonce INTEREST_RATE (atom 5.0))
(defonce VALUE_DATE (atom nil))
(defonce FIRST_DATE (atom nil))
(defn set-dates [year]
  (reset! VALUE_DATE (ext/adjust-timezone2 (str year "-01-26T00:00:50+01:00") "Europe/Berlin")) ;; Change these dates as required
  (reset! FIRST_DATE (ext/adjust-timezone2 (str year "-02-26T13:37:50+01:00") "Europe/Berlin")))
(set-dates 2022) ;; default year

(defn create-new-bullet-loan [acc-nm]
  (reset! ACCID_BULLET (create-loan-account acc-nm
                                     {:cust-key @CUSTKEY
                                      :prod-key @PRODKEY_BULLET
                                      :amount @AMOUNT
                                      :periodic-payment @PERIODIC_AMOUNT
                                      :interest-rate @INTEREST_RATE
                                      :grace_period 0
                                      :num-installments @NUM_INSTAL_AMORT}))
  (steps/apply-api ext/approveLoanAccount {:loanAccountId @ACCID_BULLET})
  (steps/apply-api ext/disburse-loan-api {:loanAccountId @ACCID_BULLET :value-date @VALUE_DATE :first-date @FIRST_DATE}))

(defn create-new-loan [acc-nm]
  (reset! ACCID (create-loan-account acc-nm
                                     {:cust-key @CUSTKEY
                                      :prod-key @PRODKEY
                                      :amount @AMOUNT
                                      :periodic-payment nil
                                      :interest-rate @INTEREST_RATE
                                      :grace_period 0
                                      :num-installments @NUM_INSTAL_AMORT}))
  (steps/apply-api ext/approveLoanAccount {:loanAccountId @ACCID})
  (steps/apply-api ext/disburse-loan-api {:loanAccountId @ACCID :value-date @VALUE_DATE :first-date @FIRST_DATE}))

;; [CONFIG] - This next call defines the Mambu tenant to use
;;    See src/http/ENV-example.clj for details on setting up your src/http/ENV.clj   
(api/setenv "env2")

;; ***[EXAMPLES]********************************************************
;; Examples off how to use this library
;;

(comment
  ;; [0] Next function deletes/zaps all loans for @CUSTKEY
  (ext/zap-all-loans2 @CUSTKEY)

  ;; [1] Create a new Loan account - then jump to [2] below to convert into a bullet loan
  ;;     This creates a standard dynamic-term-equal-installs loan.
  ;;     Other steps below then allow you to edit the schedule of this loan in various ways.
  (create-new-loan "New Simulated Bullet Loan")

  ;; [1b] (optional) Create an account using the product template we use in [3] below
  ;;      NOTE: Don't need to do this but allows you to check that the simulated bullet loan is the same
  (create-new-bullet-loan "New Real Bullet Loan")

  ;; (optional) Execute the following to change atom values used by create-new-loan
  ;;     NOTE: There are default values defined in the  atom defonce definitions above
  (reset! ACCID "XXJG121")
  (reset! ACCID "XXJG121")
  (reset! NUM_INSTAL_AMORT 20)
  (reset! NUM_INSTAL_BULLET 10)
  (reset! AMOUNT 10000.0)
  (reset! PERIODIC_AMOUNT 300.16)
  (reset! INTEREST_RATE 5.0)
  (reset! VALUE_DATE (ext/adjust-timezone2 (str "2022" "-01-26T00:00:50+01:00") "Europe/Berlin")) ;; Change these dates as required
  (reset! FIRST_DATE (ext/adjust-timezone2 (str "2022" "-02-26T13:37:50+01:00") "Europe/Berlin"))
  (reset! NUM_MONTHS 1) ;; used by distribute-dates-instalments

  ;; [2] This next step converts into a bullet loan
  ;;    It will reduce the Term of the loans from @NUM_INSTAL_AMORT to @NUM_INSTAL_BULLET
  ;;    So from a loan created in step [1] that is amortised over @NUM_INSTAL_AMORT periods
  ;;    It will reduce this to @NUM_INSTAL_BULLET, with the final instalment being a bullet (fro remaining amount)
  ;;
  (api/PRINT (:last-call (steps/apply-api reduce-to-n-instalments2 {:accid @ACCID :num-instal @NUM_INSTAL_BULLET})))

  ;; [3] This next one copies the schedule from a balloon-payments product into a dynamic-term loan to simulate the bullet/balloon
  ;;    
  (api/PRINT (:last-call (steps/apply-api copy-instalments-from-product-preview
                                          {:accid @ACCID
                                           :template-product @PRODKEY_BULLET
                                           :disbursement-date @VALUE_DATE
                                           :first-payment-date @FIRST_DATE
                                           :amount @AMOUNT
                                           :interest-rate @INTEREST_RATE
                                           :periodic-amount @PERIODIC_AMOUNT
                                           :num-instalments @NUM_INSTAL_AMORT})))

  ;; [4] Slightly different version of [2] above
  ;;     It changes all the instalments after @NUM_INSTAL_BULLET to have the same repayment-due-date
  ;;     This is another way of simulating a bullet loan (for certain dynamic-term products)
  (api/PRINT (:last-call (steps/apply-api reduce-to-n-instalments {:accid @ACCID :num-instal @NUM_INSTAL_BULLET})))

  ;; [5] Distribute the instalment dates to be different then in the original loan created in [1]
  ;;     More of a helper function to reverse the affects of [4]
  (api/PRINT (:last-call (steps/apply-api distribute-dates-instalments {:accid @ACCID :start-date "2022-02-26"})))

  
  ;; [6] Lower level edit-schedule functions that edit the priciple
  ;;     The higher level steps above ([2], [3]) use these lower level function
  ;;     They are here for testing
  (api/PRINT (:last-call (steps/apply-api edit-principal-on-instalment {:accid @ACCID :num-instal 5 :amount 6000.00})))
  (api/PRINT (:last-call (steps/apply-api edit-principal-on-instalments {:accid @ACCID :instal-list [{:num-instal 4 :amount 0.00}
                                                                                                     {:num-instal 5 :amount 1000.00}]})))

  ;; [**] Other testing steps - used during development
  (let [changes0 (mapv (fn [i] {:num-instal i :amount 0}) (range 6 11))
        changes (into [] (cons {:num-instal 5 :amount 6000.00} changes0))]
    (api/PRINT (:last-call (steps/apply-api edit-principal-on-instalments {:accid @ACCID :instal-list changes}))))


  (let [changes (mapv (fn [i] {:num-instal i :amount 1000}) (range 1 11))]
    (api/PRINT (:last-call (steps/apply-api edit-principal-on-instalments {:accid @ACCID :instal-list changes}))))


  (api/PRINT (:last-call (steps/apply-api test-edit-loan-schedule {:accid @ACCID})))
  (api/PRINT (:last-call (steps/apply-api get-loan-schedule {:accid @ACCID})))
  (api/PRINT (:last-call (steps/apply-api get-account {:accid @ACCID})))


  ;;
  )
