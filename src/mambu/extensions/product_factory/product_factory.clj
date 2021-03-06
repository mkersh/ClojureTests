;;; Support functions to build a product definition using the Mambu product create API endpoints:
;;; - https://api.mambu.com/#loan-products-create
;;; - https://api.mambu.com/#deposit-products-create
;;;
;;; NOTE: Will be concentrating on loan products initially
;;;
;;; One of the challenges that this library will support is controlling and checking what the legal product-feature combinations
;;; are. This is important because the Mambu core endpoints do not currently check that the configuration is legal and if an
;;; illegal combination is passed there will not be an error but the product will not work properly.
;;;
;;; #bookmark= 30c41be8-396e-491a-ac51-cec8b41b6859
(ns mambu.extensions.product_factory.product_factory
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [mambu.extensions.product_factory.templates.pf_temps :as temp]
            ))


(defn create-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/")
   :method api/POST
   :body (:body context)
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn delete-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/DELETE
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

;; if prod-def has config-prop=config-val then check dep-list are valid
;; dep-list is a list of [config-prop config-val-set]
(defn DEP-CHECK [title prod-def config-prop config-val config-actual dep-list]
  (when (= config-actual config-val)
    (doall
     (map (fn [[prop val-set]]
            (let [prop-val (get prod-def prop)
                  valid-val (val-set prop-val)
                  fail-str (str "Error:" title ": Invalide property seting " config-prop " = " config-val
                                " " prop " needs to be one of " val-set " is " prop-val)]
              (assert valid-val fail-str)))
          dep-list)
     )))
(defn nil-or-in-set? [s v]
  (or (nil? v) (get s v)))

(defn nil-or-boolean? [v]
(or (nil? v) (boolean? v))
)

(defn assoc-step [obj stepid prop-keyword val]
  (let [prop-str (subs (str prop-keyword) 1)
        step-prop-keyword (keyword (str stepid "-" prop-str))]
    (assoc obj step-prop-keyword val)))

;; [STEP-0] Identify a product-template on which to base this one
;; e.g (temp/ft-basics)
;; NOTE: This step is optional. If not provided will choose a suitable template for you
;;
(defn product-default-template [prod-def product-temp]
  (assoc-step prod-def "step00" :product-temp product-temp))

;; [STEP-1] Add product name and ID
(defn prod-name-id-desc
  ([prod-def name id desc]
   ;; default active to true
   (prod-name-id-desc prod-def name id desc true))
   ;; main entry point
  ([prod-def name id desc active]
   (let [prod-def2 (assoc-step prod-def "step01" :prod-name name)
         prod-def3 (assoc-step prod-def2 "step01" :prod-desc desc)
         prod-def4 (assoc-step prod-def3 "step01" :active active)]
     (assoc-step prod-def4 "step01" :prod-id id))))

;; [STEP-2] Add the product-type to the prod-def
;; product-type = (:fixed-term|:dynamic-term|:interest-free|:tranched|:revolving-credit)
;;
(defn product-type-def [prod-def product-type]
  (assert (#{:fixed-term :dynamic-term :interest-free :tranched :revolving-credit} product-type) (str "ERROR: Invalid product-type: " product-type))
  (assoc-step prod-def "step02" :product-type product-type))


;; [STEP-3] Add availability details to the prod-def
;; branches-list - List of branchid(s) that the product is available from
;;    NOTE: This is higher level than the Mambu core API that requires encodedKey
(defn prod-avail [prod-def cient-type-avail branches-list]
  (assert (#{:client :group :both} cient-type-avail) (str "ERROR: Invalid cient-type-avail: " cient-type-avail))
  (let [prod-def2 (assoc-step prod-def "step03" :prod-avail-clients cient-type-avail)]
    (assoc-step prod-def2 "step03" :prod-avail-branches branches-list)))

;; [STEP-4] Specify how the (internal) Accid will be generated
;;
(defn prod-accid [prod-def type template-or-start-num]
  (assert (#{:random-pattern :inc-number } type) (str "ERROR: Invalid prod-accid type: " type))
  (let [prod-def2 (assoc-step prod-def "step04" :accid-gen type)]
  (assoc-step prod-def2 "step04" :accid-gen-template template-or-start-num)))

;; [STEP-5] Specify initial state
(defn prod-initial-state [prod-def state]
  (assert (#{:pend-approval :partial-app } state) (str "ERROR: Invalid prod-initial-state state: " state))
  (assoc-step prod-def "step05" :initial-state state))

;; [STEP-6] Specify Amount Constraints
;; This step is optional. If not called then there will be no amount contraint
(defn prod-amount-constrain [prod-def min-amount max-amount def-amount]
  (assoc-step prod-def "step06" :amount-constraint
         {:min-amount min-amount
          :max-amount max-amount
          :def-amount def-amount}))

;; [STEP-7] Managed under a credit arrangement
(defn prod-under-ca-setting [prod-def ca-setting]
  (assert (#{:optional :required :no} ca-setting) (str "ERROR: Invalid prod-under-ca-setting: " ca-setting))
  (assoc-step prod-def "step07" :prod-under-ca-setting
         ca-setting))

;; [STEP-8] Define the instalment calculation type
;; :db - declining balance, principal reduced by equal amounts each instalment
;; :emi2 - refers to DBEI + Optimized Payments
;; :fixed-flat - only availble if product-type-def=:fixed-term|
(defn prod-instalment-calc-type [prod-def calc-type]
  (assert (#{:db :emi :emi2 :fixed-flat} calc-type) (str "ERROR: Invalid prod-instalment-calc-type: " calc-type))
  (DEP-CHECK ":fixed-flat pre-conditions" prod-def :prod-instalment-calc-type :fixed-flat calc-type
             [[:product-type #{:fixed-term}]])
  (assoc-step prod-def "step08" :prod-instalment-calc-type
         calc-type))

(defn prod-instalment-amortize-method [prod-def amort-type]
  (assert (#{:standard :balloon :payment-plan} amort-type) (str "ERROR: Invalid prod-instalment-amortize-method: " amort-type))
  (assoc-step prod-def "step08b" :prod-instalment-amortize-method
              amort-type))

;; [STEP-8b] Interest posting frequency
(defn prod-interest-posting-freq [prod-def freq]
  (assert (#{:on-repayment :on-disbursement } freq) (str "ERROR: Invalid prod-interest-posting-freq: " freq))
  (DEP-CHECK ":fixed-flat pre-conditions" prod-def :prod-interest-posting-freq :on-disbursement freq
             [[:product-type #{:fixed-term}]])
  (assoc-step prod-def "step08b" :prod-interest-posting-freq
         freq))

;; [STEP-8c] First-payment Interest adjust
(defn prod-first-interest-adjust [prod-def val]
  (assert (nil-or-boolean?  val) (str "ERROR: Invalid prod-first-interest-adjust: " val))
  (assoc-step prod-def "step08b" :prod-first-interest-adjust
              val))


;; [STEP-8c] Interest Type Settings
(defn prod-interest-type [prod-def int-type-settings]
  (let [int-rate-source (:int-rate-source int-type-settings)
        int-rate-type (:int-rate-type int-type-settings)
        int-rate-scope (:int-rate-scope int-type-settings)
        index-source (:index-source int-type-settings)
        index-spread-constrain (:index-spread-constrain int-type-settings)
        index-floor (:index-floor int-type-settings)
        index-ceiling (:index-ceiling int-type-settings)
        index-review-frequency-type (:index-review-frequency-type int-type-settings)
        index-review-frequency-val (:index-review-frequency-val int-type-settings)
        day-count-model (:day-count-model int-type-settings)]

    (assert (nil-or-in-set? #{:fixed :index} int-rate-source) (str "ERROR: Invalid prod-interest-type int-rate-source: " int-rate-source))
    (assert (nil-or-in-set? #{:simple :capitalized :compound} int-rate-type) (str "ERROR: Invalid prod-interest-type int-type: " int-rate-type))
    (assert (nil-or-in-set? #{:year :month :4weeks :week :day} int-rate-scope) (str "ERROR: Invalid prod-interest-type int-rate-scope: " int-rate-scope))
    (assert (nil-or-in-set? #{:days :weeks :months} index-review-frequency-type) (str "ERROR: Invalid prod-interest-type index-review-frequency-type: " index-review-frequency-type))
    (assert (nil-or-in-set? #{:30E-360 :actual-365 :actual-360} day-count-model) (str "ERROR: Invalid prod-interest-type day-count-model: " day-count-model))

     (when (= ( :step02-product-type prod-def) :fixed-term) (assert (= day-count-model nil) (str "ERROR: day-count-model " day-count-model " not compatible with product-type :fixed-term")))

   (-> prod-def
       (assoc-step "step08c" :int-rate-source int-rate-source)
       (assoc-step "step08c" :int-rate-type int-rate-type)
       (assoc-step "step08c" :int-rate-scope int-rate-scope)
       (assoc-step "step08c" :index-source index-source)
       (assoc-step "step08c" :index-spread-constrain index-spread-constrain)
       (assoc-step "step08c" :index-floor index-floor)
       (assoc-step "step08c" :index-ceiling index-ceiling)
       (assoc-step "step08c" :index-review-frequency-type index-review-frequency-type)
       (assoc-step "step08c" :index-review-frequency-val index-review-frequency-val)
       (assoc-step "step08c" :day-count-model day-count-model))))

;; [STEP-8c] Interest Type Settings
(defn prod-interest-rate-constrain [prod-def min-amount max-amount def-amount]
  (assoc-step prod-def "step08c" :interest-rate-constraint
         {:min-amount min-amount
          :max-amount max-amount
          :def-amount def-amount}))

;; [STEP-8d] Repayments Interest Calculation
(defn prod-repayment-interest-calc [prod-def freq]
  (assert (#{:repayment-periodicity :actual-days} freq) (str "ERROR: Invalid prod-repayment-interest-calc: " freq))
  (assert (= (:product-type prod-def) :fixed-term) "ERROR: You can only set prod-repayment-interest-calc for :product-type = :fixed-term ")
  (assoc-step prod-def "step08d" :prod-repayment-interest-calc
         freq))


;; [STEP-9] (re)Payment Interval
(defn prod-payment-interval-method [prod-def int-method int-period int-val fixed-days-list]
  (assert (#{:interval :fixed} int-method) (str "ERROR: Invalid prod-payment-interval-method: " int-method))
  (when (= int-method :interval)
    (assert (#{:days :weeks :months :years} int-period) (str "ERROR: Invalid prod-payment-interval-method period: " int-period)))
  (assoc-step prod-def "step09" :prod-payment-interval-method 
         {:int-method int-method
          :int-period int-period
          :int-val int-val
          :fixed-days-list fixed-days-list}))

;; [STEP-9b] Repayment Amount
(defn prod-repayment-amount [prod-def repayment-amount-settings]
  (let [repayment-amount-type (:repayment-amount-type repayment-amount-settings)
        principal-amount-type (:principal-amount-type repayment-amount-settings)
        total-amount-type (:total-amount-type repayment-amount-settings)
        repayment-amount-constraint (:repayment-amount-constraint repayment-amount-settings)]

    (assert (= (:product-type prod-def) :revolving-credit) "ERROR: prod-repayment-amount only available on :revolving-credit")
    (assert (#{:principal :total} repayment-amount-type) (str "ERROR: Invalid prod-repayment-amount repayment-amount-type: " repayment-amount-type))
    (when principal-amount-type (assert (#{:flat :outstanding :last} principal-amount-type) (str "ERROR: Invalid prod-repayment-amount principal-amount-type: " principal-amount-type)))
    (when total-amount-type (assert (#{:flat :total :all} total-amount-type) (str "ERROR: Invalid prod-repayment-amount total-amount-type: " total-amount-type)))

    (-> prod-def
        (assoc-step "step09b"  :repayment-amount-type repayment-amount-type)
        (assoc-step "step09b"  :principal-amount-type principal-amount-type)
        (assoc-step "step09b"  :total-amount-type total-amount-type)
        (assoc-step "step09b"  :repayment-amount-constraint repayment-amount-constraint))
    
    ))


;; [STEP-10] Specify Installments Constraints
;; This step is optional. If not called then there will be no Instalment contraints
(defn prod-installments-constrain [prod-def min-num max-num def-num]
  (assoc-step prod-def "step10" :installments-constraint
         {:min-num min-num
          :max-num max-num
          :def-num def-num}))

;; [STEP-10b] Specify first-payment-date Constraints
;; This step is optional. If not called then there will be no first-payment-date contraints
(defn prod-first-payment-date-constrain [prod-def min-num max-num def-num]
  (assoc-step prod-def "step10b" :first-payment-date-constraint
         {:min-num min-num
          :max-num max-num
          :def-num def-num}))

;; [STEP-11] Collect principal every n Installments
;; Normally should be set to 1 but can be less frequent
(defn prod-principal-collect-frequency [prod-def num]
  (assoc-step prod-def "step11" :prod-principal-collect-frequency num))

;; [STEP-12] Grace period settings
(defn prod-grace-period 
([prod-def grace-type] (prod-grace-period prod-def grace-type nil nil nil))
([prod-def grace-type grace-period-min grace-period-max grace-period-def ]
  (assert (#{:none :principal :pure} grace-type) (str "ERROR: Invalid prod-grace-period: " grace-type))
  (assoc-step prod-def "step12" :prod-grace-period
         {:grace-type grace-type
          :grace-period-constraint {:min grace-period-min :max grace-period-max :def grace-period-def}})))


;; [STEP-13] Repayment Rounding
(defn prod-repayment-rounding
  [prod-def install-round currency-round]
   (assert (#{:no-round :into-last} install-round) (str "ERROR: Invalid prod-repayment-rounding  install-round: " install-round))
   (assert (#{:no-round :round :round-up} currency-round) (str "ERROR: Invalid prod-repayment-rounding  currency-round: " currency-round))
   (assoc-step prod-def "step13" :prod-repayment-rounding
          {:install-round install-round
           :currency-round currency-round}))

;; [STEP-14] Non working days reschedule
(defn prod-non-working-days-reschedule
  [prod-def setting]
  (assert (#{:no :forward :backward :extend} setting) (str "ERROR: Invalid prod-non-working-days-reschedule: " setting))
  (assoc-step prod-def "step14" :prod-non-working-days-reschedule setting))

;; [STEP-15] Schedule Ediing
(defn prod-schedule-edit
  [prod-def edit-obj]
  (let [product-type (:step02-product-type prod-def)]
    (when (not (= product-type :fixed-term))
      (assert (not (:interest? edit-obj)) "Setting only available for :fixed-term")
      (assert (not (:fee? edit-obj)) "Setting only available for :fixed-term")
      (assert (not (:penalty? edit-obj)) "Setting only available for :fixed-term"))
    (when (= product-type :fixed-term)
      (assert (not (:num? edit-obj)) "Setting only available for :dynamic-term")) 
    (when (= product-type :tranched)
      (assert (not (or (:dates? edit-obj) (:principal? edit-obj) (:num? edit-obj) (:holidays? edit-obj)
                  (:interest? edit-obj) (:fee? edit-obj) (:penalty? edit-obj))) "edit schedule not available on :tranched"))
    (when (= product-type :revolving-credit)
      (assert (not (or (:principal? edit-obj) (:holidays? edit-obj)
                        (:interest? edit-obj) (:fee? edit-obj) (:penalty? edit-obj))) "edit schedule not available on :tranched")))
  (assoc-step prod-def "step15" :prod-schedule-edit 
  {:dates? (:dates? edit-obj)
   :principal? (:principal? edit-obj)
   :num? (:num? edit-obj)
   :holidays? (:holidays? edit-obj)
   :interest? (:interest? edit-obj) 
   :fee? (:fee? edit-obj) 
   :penalty? (:penalty? edit-obj)
   }))

;; [STEP-16] Repayment Collection
(defn prod-repayment-collection [prod-def collect-settings]
  (let [payment-allocation-method (:payment-allocation-method collect-settings)
        payment-allocation-order (:payment-allocation-order collect-settings)
        pre-payments-accept (:pre-payments-accept collect-settings)
        pre-payments-apply-interest (:pre-payments-apply-interest collect-settings)
        pre-payments-recalculation (:pre-payments-recalculation collect-settings)
        overdue-payments (:overdue-payments collect-settings)
        pre-payments-future-interest (:pre-payments-future-interest collect-settings)
        allow-custom-repayment-allocation (:allow-custom-repayment-allocation collect-settings)
        amortisation-method (:step08b-prod-instalment-amortize-method prod-def)]

    ;;(assert (= (:product-type prod-def) :revolving-credit) "ERROR: prod-repayment-amount only available on :revolving-credit")
    (assert (nil-or-in-set? #{:horizontal :vertcal} payment-allocation-method) (str "ERROR: Invalid prod-repayment-collection payment-allocation-method: " payment-allocation-method))
    (assert (nil-or-in-set? #{:no :accept} pre-payments-accept) (str "ERROR: Invalid prod-repayment-collection pre-payments-accept: " pre-payments-accept))
    (assert (nil-or-in-set? #{:auto :manual} pre-payments-apply-interest) (str "ERROR: Invalid prod-repayment-collection pre-payments-apply-interest: " pre-payments-apply-interest))
    (assert (nil-or-in-set? #{:none :next-installments :reduce-term :reduce-amount} pre-payments-recalculation) (str "ERROR: Invalid prod-repayment-collection pre-payments-recalculation: " pre-payments-recalculation))
    (assert (nil-or-in-set? #{:increase-installments} overdue-payments) (str "ERROR: Invalid prod-repayment-collection overdue-payments: " overdue-payments))
    (map (fn [order-item] (assert (#{:fee :penalty :interest :principal} order-item) (str "ERROR: Invalid prod-repayment-collection payment-allocation-order: " order-item))) payment-allocation-order)
    (assert (= (count (into #{} payment-allocation-order)) 4) "ERROR :payment-allocation-order needs order of all :fee :penalty :interest :principal defining")
    (assert (nil-or-in-set? #{:none :accept :accept-future} pre-payments-future-interest) (str "ERROR: Invalid prod-repayment-collection pre-payments-future-interest: " pre-payments-future-interest))
    (assert (nil-or-in-set? #{true false} allow-custom-repayment-allocation) (str "ERROR: Invalid prod-repayment-collection allow-custom-repayment-allocation: " allow-custom-repayment-allocation))

    ;; Check any specific rules around settings
    (when (and (= amortisation-method :balloon) (= (:step02-product-type prod-def) :dynamic-term)) (assert (= pre-payments-recalculation :reduce-term) (str "ERROR: pre-payments-recalculation " pre-payments-recalculation " not compatible with amortisation-method = :balloon")))
    (when (and (= amortisation-method :balloon) (= (:step02-product-type prod-def) :fixed-term)) (assert (= pre-payments-accept :accept) (str "ERROR: pre-payments-accept " pre-payments-accept " not compatible with amortisation-method = :balloon")))

    (-> prod-def
        (assoc-step "step16"  :payment-allocation-method payment-allocation-method)
        (assoc-step "step16"  :payment-allocation-order payment-allocation-order)
        (assoc-step "step16"  :pre-payments-accept pre-payments-accept)
        (assoc-step "step16"  :pre-payments-apply-interest pre-payments-apply-interest)
        (assoc-step "step16"  :pre-payments-recalculation pre-payments-recalculation)
        (assoc-step "step16"  :overdue-payments overdue-payments)
        (assoc-step "step16"  :pre-payments-future-interest pre-payments-future-interest)
        (assoc-step "step16"  :allow-custom-repayment-allocation allow-custom-repayment-allocation))))

;; [STEP-17] Arrears Tollerance Period Consraints
(defn prod-arrears-tolerance-period-constrain [prod-def def-num min-num max-num ]
  (assoc-step prod-def "step17" :prod-arrears-tolerance-period-constrain
         {:min min-num
          :max max-num
          :def def-num}))

;; [STEP-18] Arrears Tollerance Consraints
(defn prod-arrears-tolerance-amount-constrain [prod-def def-num min-num max-num ]
  (assoc-step prod-def "step18" :prod-arrears-tolerance-amount-constrain
         {:min min-num
          :max max-num
          :def def-num}))

;; [STEP-19] Arrears Processing
(defn prod-arrears-settings [prod-def collect-settings]
  (let [arrears-days-calculated-from (:arrears-days-calculated-from collect-settings)
        non-working-days (:non-working-days collect-settings)
        arrears-floor (:arrears-floor collect-settings)
        accrue-late-interest (:accrue-late-interest collect-settings)
        ]
    
    ;;(assert (= (:product-type prod-def) :revolving-credit) "ERROR: prod-repayment-amount only available on :revolving-credit")
    (assert (#{:first :oldest} arrears-days-calculated-from) (str "ERROR: Invalid prod-repayment-collection payment-allocation-method: " arrears-days-calculated-from))
    (assert (#{:include :exclude} non-working-days) (str "ERROR: Invalid prod-repayment-collection payment-allocation-method: " non-working-days))

    (-> prod-def
        (assoc-step "step19"  :arrears-days-calculated-from arrears-days-calculated-from)
        (assoc-step "step19"  :non-working-days non-working-days)
        (assoc-step "step19"  :arrears-floor arrears-floor)
        (assoc-step "step19"  :accrue-late-interest accrue-late-interest)
        )))

;; [STEP-20] Penalty Setup
(defn prod-penalty-settings [prod-def penalty-settings]
  (let [penalty-calc-method (:penalty-calc-method penalty-settings)
        penalty-tollerance-period (:penalty-tollerance-period penalty-settings)
        penalty-rate-constraint (:penalty-rate-constraint penalty-settings)]
    (assert (nil-or-in-set? #{:no :overdue-principal :overdue-principal-interest :outstanding-principal} penalty-calc-method) (str "ERROR: Invalid prod-penalty-settings : " penalty-calc-method))
    (-> (assoc-step prod-def "step20"  :penalty-calc-method penalty-calc-method)
        (assoc-step  "step20"  :penalty-tollerance-period penalty-tollerance-period)
        (assoc-step  "step20"  :penalty-rate-constraint penalty-rate-constraint))))

(defonce CREATE-FOR_REAL (atom true))
(defn create-loan-product [body-obj]
  (if @CREATE-FOR_REAL
    (let [_ (prn "create-loan-product - start")
          res (steps/apply-api create-loan-product-api {:body body-obj})
          _ (prn "create-loan-product - end")]
      res)
    body-obj))

(defn get-loan-product-api [context]
  {:url (str "{{*env*}}/loanproducts/" (:prodid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-branch-api [context]
  {:url (str "{{*env*}}/branches/" (:id context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-branch-encid-from-id [branch-id]
  (if-let [val (get-in (steps/apply-api get-branch-api {:id branch-id}) [:last-call "encodedKey"])]
    val
    (assert false (str "ERROR: Unable to find branch with ID " branch-id))))

(defn branch-settings [branch-list]
 (prn "branch-settings" branch-list)
 (if (empty? branch-list)
   {"forAllBranches" true,
    "availableProductBranches" []}
    (let [branch-list2 (mapv get-branch-encid-from-id branch-list)]
      {"forAllBranches" false,
       "availableProductBranches" branch-list2}
      )
  )
)

(defn get-source-index-api [context]
  {:url (str "{{*env*}}/indexratesources/" (:id context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-index-encid-from-id [source-id]
  (if source-id
    (if-let [val (get-in (steps/apply-api get-source-index-api {:id source-id}) [:last-call "encodedKey"])]
      val
      (assert false (str "ERROR: Unable to find SOURCE with ID " source-id)))
    nil))

(defn grace-period-val [grace-obj]
  (let [grace-type-map {:none "NONE" :principal "PAY_INTEREST_ONLY" :pure "INTEREST_FORGIVENESS"}
        grace-type (:grace-type grace-obj)
        grace-type-val (grace-type-map grace-type)
        min (get grace-obj :min 0)
        def (get grace-obj :def min)
        max (get grace-obj :max 9999)]
    (if (= grace-type :none)
      {"gracePeriod" {}, "gracePeriodType" grace-type-val}
      {"gracePeriod" {"defaultValue" def, "minValue" min, "maxValue" max}, "gracePeriodType" grace-type-val}
      )))

(defn schedule-edit-val [edit-obj]
  (let [edit-list (into []
                        (filter
                         (fn [val] val)
                         [(when (:dates? edit-obj) "ADJUST_PAYMENT_DATES")
                          (when (:principal? edit-obj) "ADJUST_PRINCIPAL_PAYMENT_SCHEDULE")
                          (when (:num? edit-obj) "ADJUST_NUMBER_OF_INSTALLMENTS")
                          (when (:holidays? edit-obj) "ADJUST_PAYMENT_HOLIDAYS")
                          (when (:interest? edit-obj) "ADJUST_INTEREST_PAYMENT_SCHEDULE")
                          (when (:fee? edit-obj) "ADJUST_FEE_PAYMENT_SCHEDULE")
                          (when (:penalty? edit-obj) "ADJUST_PENALTY_PAYMENT_SCHEDULE")]))]
    edit-list))

(defn prepayment-recalculation-val [prod-spec _spec-item]
  (fn [body-obj val]
    (let [product-type (:step02-product-type prod-spec)
          instal-calc-type (:step08-prod-instalment-calc-type prod-spec)
          [body-obj2 val-map] (if (and (= product-type :dynamic-term) (= instal-calc-type :db))
                                [body-obj {:none "NO_RECALCULATION" :next-installments "RESCHEDULE_REMAINING_REPAYMENTS" :reduce-term "RECALCULATE_SCHEDULE_KEEP_SAME_PRINCIPAL_AMOUNT" :reduce-amount "RECALCULATE_SCHEDULE_KEEP_SAME_NUMBER_OF_TERMS"}]
                                (if (and (= product-type :dynamic-term) (= instal-calc-type :emi))
                                  (let [body-obj3 (if (= val :none)
                                                    (assoc-in body-obj ["paymentSettings" "prepaymentSettings" "principalPaidInstallmentStatus"] "PARTIALLY_PAID")
                                                    (assoc-in body-obj ["paymentSettings" "prepaymentSettings" "principalPaidInstallmentStatus"] "PAID"))]
                                    [body-obj3 {:none "NO_RECALCULATION" :reduce-term "REDUCE_NUMBER_OF_INSTALLMENTS_NEW" :reduce-amount "REDUCE_AMOUNT_PER_INSTALLMENT"}])
                                  [body-obj {}]))
          _ (assert (nil-or-in-set? val-map val) (str "ERROR: prepayment-recalculation-val - " val))]
      [body-obj2 (get val-map val)])))

(defn arrears-tollerance-val [arrears-obj]
  (let [min (get arrears-obj :min 0)
        def (get arrears-obj :def min)
        max (get arrears-obj :max 9999)]
    {"defaultValue" def, "minValue" min, "maxValue" max}))

(defn payment-frequency-val [body-obj val-obj]
(let [val-map {:interval "INTERVAL" :fixed "FIXED_DAYS_OF_MONTH"}
      interval-type (:int-method val-obj)
      interval-val (:int-val val-obj)
      interval-period (:int-period val-obj)
      period_map {:days "DAYS" :weeks "WEEKS" :months "MONTHS" :years "YEARS"}
      fixed-days (:fixed-days-list val-obj)
      body-obj2 (if (= interval-type :interval)
                  (-> (assoc-in body-obj ["scheduleSettings" "repaymentPeriodUnit"] (get period_map interval-period))
                      (assoc-in ["scheduleSettings" "defaultRepaymentPeriodCount"] interval-val))
                  (assoc-in body-obj ["scheduleSettings" "fixedDaysOfMonth"] fixed-days))
      ]
      [body-obj2 (get val-map interval-type)]
      )
)

(defn instalment-amortize-method [prod-spec]
  (fn [body-obj amort-val]
    (let [product-type (:step02-product-type prod-spec)
          val-map {:standard "STANDARD_PAYMENTS" :balloon "BALLOON_PAYMENTS" :payment-plan "PAYMENT_PLAN"}
          body-obj2 (if (and (= product-type :dynamic-term) (= amort-val :balloon))
                      (-> (assoc-in body-obj ["paymentSettings" "prepaymentSettings" "elementsRecalculationMethod"] "TOTAL_EXPECTED_FIXED")
                          (assoc-in ["paymentSettings" "latePaymentsRecalculationMethod"] "LAST_INSTALLMENT_INCREASE")
                          (assoc-in ["paymentSettings" "prepaymentSettings" "prepaymentRecalculationMethod"] "REDUCE_NUMBER_OF_INSTALLMENTS_NEW"))
                      body-obj)
          body-obj3 (if (and (= product-type :fixed-term) (= amort-val :balloon))
                      (-> (assoc-in body-obj2 ["paymentSettings" "prepaymentSettings" "prepaymentAcceptance"] "ACCEPT_PREPAYMENTS")
                          (assoc-in ["paymentSettings" "latePaymentsRecalculationMethod"] "LAST_INSTALLMENT_INCREASE"))
                      body-obj2)]
      [body-obj3 (get val-map amort-val)]))
    )

;; This is a more complex version of add-to-body
;; Difference: It pase the body-obj to the val-func
;; The val-func can modify this body-obj as well as return a val for a specific attribute
;; val-func needs to return [<new-body-obj> <val>]
(defn add-to-body2
  ([body-obj prod-spec spec-item body-attr val-func]
   (let [val (get prod-spec spec-item)
         [body-obj2 val2] (val-func body-obj val)]
     (if val2
       (if (coll? body-attr)
         (assoc-in body-obj2 body-attr val2)
         (assoc body-obj2 body-attr val2))
       body-obj))))

(defn add-to-body
  ([body-obj prod-spec spec-item body-attr] (add-to-body body-obj prod-spec spec-item body-attr {}))
  ([body-obj prod-spec spec-item body-attr val-func]
   (let [val (get prod-spec spec-item)
         val2 (if (map? val-func)
                (get val-func val val)
                (val-func val))]
     (if (coll? body-attr)
       (assoc-in body-obj body-attr val2)
       (assoc body-obj body-attr val2)))))

(defn create-loan-feature [body-obj prod-spec spec-item]
  (condp = spec-item
    :step00-product-temp nil ;; will be used in merge-body-objs below if passed
    :step01-active (add-to-body body-obj prod-spec spec-item "state" {true "ACTIVE" false "INACTIVE"})
    :step01-prod-desc (add-to-body body-obj prod-spec spec-item "notes")
    :step01-prod-id (add-to-body body-obj prod-spec spec-item "id")
    :step01-prod-name (add-to-body body-obj prod-spec spec-item "name")
    ;; repaymentScheduleMethod" "DYNAMIC"??
    :step02-product-type (add-to-body body-obj prod-spec spec-item "type" {:fixed-term "FIXED_TERM_LOAN" :dynamic-term "DYNAMIC_TERM_LOAN"})
    :step03-prod-avail-branches (add-to-body body-obj prod-spec spec-item ["availabilitySettings" "branchSettings"] branch-settings)
    :step03-prod-avail-clients (add-to-body body-obj prod-spec spec-item ["availabilitySettings" "availableFor"] {:client ["INDIVIDUALS"] :group ["PURE_GROUPS"] :both ["INDIVIDUALS" "PURE_GROUPS"]})
    :step04-accid-gen  (add-to-body body-obj prod-spec spec-item ["newAccountSettings" "idGeneratorType"] {:random-pattern "RANDOM_PATTERN" :inc-number "INCREMENTAL_NUMBER"})
    :step04-accid-gen-template (add-to-body body-obj prod-spec spec-item ["newAccountSettings" "idPattern"])
    :step05-initial-state (add-to-body body-obj prod-spec spec-item ["newAccountSettings" "accountInitialState"] {:pend-approval "PENDING_APPROVAL" :partial-app "PARTIAL_APPLICATION"})
    :step06-amount-constraint body-obj ;; TBD
    :step07-prod-under-ca-setting body-obj ;; TBD
    :step08-prod-instalment-calc-type (-> (add-to-body body-obj prod-spec spec-item ["interestSettings" "interestCalculationMethod"] {:db "DECLINING_BALANCE" :emi "DECLINING_BALANCE_DISCOUNTED" :emi2 "DECLINING_BALANCE_DISCOUNTED" :fixed-flat "FLAT"})
                                          (add-to-body prod-spec spec-item ["paymentSettings" "amortizationMethod"] {:db "STANDARD_PAYMENTS" :emi "STANDARD_PAYMENTS" :emi2 "OPTIMIZED_PAYMENTS" :fixed-flat "STANDARD_PAYMENTS"}))
    :step08b-prod-instalment-amortize-method (add-to-body2 body-obj prod-spec spec-item ["paymentSettings" "amortizationMethod"] (instalment-amortize-method prod-spec)) 
    :step08b-prod-interest-posting-freq (add-to-body body-obj prod-spec spec-item ["interestSettings" "interestApplicationMethod"] {:on-repayment "REPAYMENT_DUE_DATE" :on-disbursement "AFTER_DISBURSEMENT"})
    :step08b-prod-first-interest-adjust (add-to-body body-obj prod-spec spec-item "adjustInterestForFirstInstallment" )
    :step08c-day-count-model (add-to-body body-obj prod-spec spec-item [ "interestSettings" "daysInYear"] {:30E-360 "E30_360" :actual-365 "ACTUAL_365_FIXED" :actual-360 "ACTUAL_360"})
    :step08c-index-ceiling (add-to-body body-obj prod-spec spec-item ["interestSettings" "indexRateSettings" "interestRateCeilingValue"])
    :step08c-index-floor (add-to-body body-obj prod-spec spec-item ["interestSettings" "indexRateSettings" "interestRateFloorValue"])
    :step08c-index-review-frequency-type (add-to-body body-obj prod-spec spec-item ["interestSettings" "indexRateSettings" "interestRateReviewUnit"] {:days "DAYS" :weeks "WEEKS" :months "MONTHS"})
    :step08c-index-review-frequency-val (add-to-body body-obj prod-spec spec-item ["interestSettings" "indexRateSettings" "interestRateReviewCount"] )
    :step08c-index-source (add-to-body body-obj prod-spec spec-item ["interestSettings" "indexRateSettings" "indexSourceKey"], get-index-encid-from-id)
    :step08c-index-spread-constrain body-obj ;; TBD
    :step08c-int-rate-scope (add-to-body body-obj prod-spec spec-item ["interestSettings" "indexRateSettings" "interestChargeFrequency"], {:year "ANNUALIZED" :month "EVERY_MONTH" :4weeks "EVERY_FOUR_WEEKS" :week "EVERY_WEEK" :day "EVERY_DAY"} )
    :step08c-int-rate-source (add-to-body body-obj prod-spec spec-item ["interestSettings"  "indexRateSettings" "interestRateSource"] {:fixed "FIXED_INTEREST_RATE" :index "INDEX_INTEREST_RATE"})
    :step08c-int-rate-type (add-to-body body-obj prod-spec spec-item ["interestSettings"  "interestType" ] {:simple "SIMPLE_INTEREST" :capitalized "CAPITALIZED_INTEREST" :compound "COMPOUNDING_INTEREST"})
    :step09-prod-payment-interval-method (add-to-body2 body-obj prod-spec spec-item ["scheduleSettings"  "scheduleDueDatesMethod"] payment-frequency-val)
    :step10-installments-constraint body-obj ;; TBD
    :step10b-first-payment-date-constraint body-obj ;; TBD
    :step11-prod-principal-collect-frequency body-obj ;; TBD
    :step12-prod-grace-period (add-to-body body-obj prod-spec spec-item "gracePeriodSettings", grace-period-val)
    :step13-prod-repayment-rounding body-obj ;; TBD
    :step14-prod-non-working-days-reschedule (add-to-body body-obj prod-spec spec-item ["scheduleSettings" "repaymentReschedulingMethod"] {:no "NONE" :forward "repaymentReschedulingMethod" :backward "PREVIOUS_WORKING_DAY" :extend "EXTEND_SCHEDULE"})
    :step15-prod-schedule-edit (add-to-body body-obj prod-spec spec-item ["scheduleSettings" "repaymentScheduleEditOptions"], schedule-edit-val )
    :step16-allow-custom-repayment-allocation (add-to-body body-obj prod-spec spec-item "allowCustomRepaymentAllocation")
    :step16-overdue-payments body-obj ;; TBD
    :step16-payment-allocation-method body-obj ;; TBD
    :step16-payment-allocation-order body-obj ;; TBD
    :step16-pre-payments-accept (add-to-body body-obj prod-spec spec-item ["paymentSettings" "prepaymentSettings" "prepaymentAcceptance"] {:no "NO_PREPAYMENTS" :accept "ACCEPT_PREPAYMENTS"})
    :step16-pre-payments-apply-interest (add-to-body body-obj prod-spec spec-item ["paymentSettings" "prepaymentSettings" "applyInterestOnPrepaymentMethod"] {:auto "AUTOMATIC" :manual "MANUAL"})
    :step16-pre-payments-future-interest (add-to-body body-obj prod-spec spec-item ["paymentSettings" "prepaymentSettings" "futurePaymentsAcceptance"] {:none "NO_FUTURE_PAYMENTS" :accept "ACCEPT_OVERPAYMENTS" :accept-future "ACCEPT_FUTURE_PAYMENTS"})
    :step16-pre-payments-recalculation (add-to-body2 body-obj prod-spec spec-item ["paymentSettings" "prepaymentSettings" "prepaymentRecalculationMethod"] (prepayment-recalculation-val prod-spec spec-item) )
    :step17-prod-arrears-tolerance-period-constrain (add-to-body body-obj prod-spec spec-item ["arrearsSettings" "tolerancePeriod"] arrears-tollerance-val)
    :step18-prod-arrears-tolerance-amount-constrain (add-to-body body-obj prod-spec spec-item ["arrearsSettings" "tolerancePercentageOfOutstandingPrincipal"] arrears-tollerance-val)
    :step19-arrears-days-calculated-from (add-to-body body-obj prod-spec spec-item ["arrearsSettings" "dateCalculationMethod"] {:first "ACCOUNT_FIRST_WENT_TO_ARREARS" :oldest "LAST_LATE_REPAYMENT"})
    :step19-non-working-days (add-to-body body-obj prod-spec spec-item ["arrearsSettings" "nonWorkingDaysMethod"] {:include "INCLUDED" :exclude "EXCLUDED"} )
    :step19-arrears-floor (add-to-body body-obj prod-spec spec-item ["arrearsSettings" "toleranceFloorAmount"])
    :step19-accrue-late-interest (add-to-body body-obj prod-spec spec-item ["interestSettings" "accrueLateInterest"] )
    :step20-penalty-calc-method (add-to-body body-obj prod-spec spec-item ["penaltySettings" "loanPenaltyCalculationMethod"] {:no "NONE" :overdue-principal "OVERDUE_BALANCE" :overdue-principal-interest "OVERDUE_BALANCE_AND_INTEREST" :outstanding-principal "OUTSTANDING_PRINCIPAL"})
    :step20-penalty-tollerance-period (add-to-body body-obj prod-spec spec-item ["penaltySettings" "loanPenaltyGracePeriod"])
    :step20-penalty-rate-constraint (add-to-body body-obj prod-spec spec-item ["penaltySettings" "penaltyRate"] arrears-tollerance-val)
    (do (prn "ERROR: Unknown item" spec-item) body-obj)))

;; Was originally using this with (merge-with into2 & maps) from merge-body-objs
;; but it wasn't working
(defn into2 [to from]
  (if (coll? to)
    (into to from)
    from))

;; http://dnaeon.github.io/recursively-merging-maps-in-clojure/
(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (last xs)))]
    (reduce m maps)))

(defn merge-body-objs [prod-spec body-obj]
  (let [prod-temp (:step00-product-temp prod-spec)
        product-type (:step02-product-type prod-spec)
        common-body {"creditArrangementSettings" {"creditArrangementRequirement" "OPTIONAL"}}
        prod-template (or prod-temp (condp = product-type
                        :dynamic-term (temp/dt-basics)
                        :fixed-term (temp/ft-basics)
                        (assert false (str "ERROR: Cannot (currently) create: " product-type))))]
    (deep-merge common-body prod-template body-obj)))

(defn generate-loan-product [prod-spec]
  (let [spec-list0 (sort (keys prod-spec))]
    (loop [spec-list spec-list0
           body-obj {}]
      (let [spec-item (first spec-list)
            _ (prn "build:" spec-item)]
        (if (= spec-item nil)
          (create-loan-product (merge-body-objs prod-spec body-obj))
          (let [body-obj2 (create-loan-feature body-obj prod-spec spec-item)]
            (recur (rest spec-list) body-obj2)))))))

(declare prod-dt-spec1 prod-ft-spec1)
(comment
(/ 260 60.0)
  (temp/dt-basics)
  (api/setenv "env2") ;; MK prod  
  (merge-with into2 {:f1 {:f11 1 :f13 3}} {:f1 {:f12 2} :f3 3})
  (merge-with into2 {:f1 "fff"} {:f1 "222"})
  (deep-merge {:f1 {:f11 {:f111 1} :f13 3}} {:f1 {:f11 {:f112 1} :f12 2 :f13 "xx"} :f3 3})

  (reset! CREATE-FOR_REAL true)
  ;; [1] Create dynamic-term product
  (generate-loan-product prod-dt-spec1)
  (:last-call (steps/apply-api delete-loan-product-api {:prodid "PROD1a"}))
  
  ;; [2] Create fixed-term product
  (generate-loan-product prod-ft-spec1)
  (:last-call (steps/apply-api delete-loan-product-api {:prodid "PROD_FT2b"}))


  ;; TBD - Add support for "Adjust interest to the first repayment"

  (sort prod-dt-spec1)
  (get-index-encid-from-id "459709177")
  (:last-call (steps/apply-api get-loan-product-api {:prodid "PF-DT1"}))
  (:last-call (steps/apply-api get-loan-product-api {:prodid "PF_DTB1"}))
  (:last-call (steps/apply-api get-loan-product-api {:prodid "PF_FTB1"}))
  (:last-call (steps/apply-api get-loan-product-api {:prodid "PROD1a"}))
  (:last-call (steps/apply-api get-loan-product-api {:prodid "PROD_FT2b"}))



  ;; "repaymentScheduleEditOptions" ["ADJUST_PAYMENT_DATES" "ADJUST_PRINCIPAL_PAYMENT_SCHEDULE"],

  (def prod-dt-spec1 (-> {}
                         (product-default-template (temp/dt-basics))
                         (prod-name-id-desc "PROD1a XXX" "PROD1a" "")
                         (product-type-def :dynamic-term) ;; :fixed-term :dynamic-term :interest-free :tranched :revolving-credit
                         (prod-avail :client ["prodfac1"])
                         (prod-accid :random-pattern "@@@@###")
                         (prod-initial-state :pend-approval) ;; Optional step
                         (prod-amount-constrain 0 1000 500)  ;; Optional step
                         (prod-under-ca-setting :no)
                         (prod-instalment-calc-type :emi)
                         ;; if next = :balloon then :pre-payments-recalculation needs to be :reduce-term
                         (prod-instalment-amortize-method :balloon) ;; :standard :balloon :payment-plan
                         ;;(prod-instalment-calc-type :fixed-flat) ;; pre-conditions as to when this val is possible
                         (prod-interest-posting-freq :on-repayment)
                         (prod-first-interest-adjust false)

                         (prod-interest-type
                          {:int-rate-source :fixed
                           :int-rate-type :simple
                           :index-source nil
                           :index-spread-constrain {:min-amount 0 :max-amount 0 :def-amount 0}
                           :index-floor 0
                           :index-ceiling 0
                           :index-review-frequency-type nil  ;;:months
                           :index-review-frequency-val nil
                           :int-rate-scope :year
                           :day-count-model :actual-365})

                         (prod-payment-interval-method :interval :months 1 nil)
                      ;;(prod-payment-interval-method :fixed nil nil [1 3 4])
                         (prod-installments-constrain 0 600 10)

                      ;; (prod-repayment-amount  ;; Only available on :product-type = :revolving-credit
                      ;;  {:repayment-amount-type :principal
                      ;;   :principal-amount-type :flat
                      ;;   :total-amount-type :flat
                      ;;   :repayment-amount-constraint {:min-amount 0 :max-amount 0 :def-amount 0}})

                         (prod-first-payment-date-constrain 0 100 50)
                         (prod-principal-collect-frequency 1)
                         (prod-grace-period :none)
                      ;;(prod-grace-period :pure nil nil nil)
                      ;;(prod-grace-period :principal nil nil nil)
                         (prod-repayment-rounding :no-round :no-round)
                         (prod-non-working-days-reschedule :no) ;; :no :forward :backward :extend


                         (prod-schedule-edit {:dates? false
                                              :principal? false
                                              :num? false
                                              :holidays? false
                                              :interest? false ;; only available for :fixed-term
                                              :fee? false      ;; only available for :fixed-term
                                              :penalty? false  ;; only available for :fixed-term
                                              })


                         (prod-repayment-collection {:payment-allocation-method :horizontal ;; :horizontal :vertcal
                                                     :payment-allocation-order [:fee :penalty :interest :principal] ;; :fee :penalty :interest :principal
                                                     :pre-payments-accept :accept ;; :no :accept
                                                     :pre-payments-apply-interest :auto ;; nil :auto :manual - nil if previous :no
                                                     :pre-payments-recalculation :reduce-term ;; :none :next-installments :reduce-term :reduce-amount
                                                     :overdue-payments :increase-installments ;; :increase-installments
                                                     :pre-payments-future-interest :none ;; :none :accept :accept-future
                                                     :allow-custom-repayment-allocation true})

                         (prod-arrears-tolerance-period-constrain 0 0 77)
                         (prod-arrears-tolerance-amount-constrain nil nil nil)


                         (prod-arrears-settings {:arrears-days-calculated-from  :first
                                                 :non-working-days :include
                                                 :arrears-floor nil
                                                 :accrue-late-interest true})

                         (prod-penalty-settings {:penalty-calc-method :overdue-principal ;; :no :overdue-principal :overdue-principal-interest :outstanding-principal
                                                 :penalty-tollerance-period 0
                                                 :penalty-rate-constraint {:min 0 :max 999 :def 0}})))

  (def prod-ft-spec1 (-> {}
                         (prod-name-id-desc "PROD_FT1" "PROD_FT2b" "")
                         (product-type-def :fixed-term) ;; :fixed-term :dynamic-term :interest-free :tranched :revolving-credit
                         (prod-avail :client ["prodfac1"])
                         (prod-accid :random-pattern "@@@@###")
                         (prod-initial-state :pend-approval) ;; Optional step
                         (prod-amount-constrain 0 1000 500)  ;; Optional step
                         (prod-under-ca-setting :no)
                         (prod-instalment-calc-type :emi)
                         (prod-instalment-amortize-method :standard) ;; :balloon :payment-plan
                         (prod-interest-posting-freq :on-repayment)
                         (prod-first-interest-adjust false)

                         (prod-interest-type
                          {:int-rate-source :fixed
                           :int-rate-type :simple
                           :index-source nil
                           :index-spread-constrain {:min-amount 0 :max-amount 0 :def-amount 0}
                           :index-floor 0
                           :index-ceiling 0
                           :index-review-frequency-type nil  ;;:months
                           :index-review-frequency-val nil
                           :int-rate-scope :year
                           :day-count-model nil ;; must be nil for :fixed-term
                           })
                         
                         ;; This next one works for payment-plan
                        ;;  (prod-interest-type
                        ;;   {:int-rate-source :fixed
                        ;;    :int-rate-type :simple
                        ;;    :index-source nil
                        ;;    :index-spread-constrain nil
                        ;;    :index-floor nil
                        ;;    :index-ceiling nil
                        ;;    :index-review-frequency-type nil  ;;:months
                        ;;    :index-review-frequency-val nil
                        ;;    :int-rate-scope nil
                        ;;    :day-count-model nil ;; must be nil for :fixed-term
                        ;;    })

                         (prod-payment-interval-method :interval :months 1 nil)
                         ;;(prod-payment-interval-method :fixed nil nil [1 3 4])
                         (prod-installments-constrain 0 600 10)

                      ;; (prod-repayment-amount  ;; Only available on :product-type = :revolving-credit
                      ;;  {:repayment-amount-type :principal
                      ;;   :principal-amount-type :flat
                      ;;   :total-amount-type :flat
                      ;;   :repayment-amount-constraint {:min-amount 0 :max-amount 0 :def-amount 0}})

                         (prod-first-payment-date-constrain 0 100 50)
                         (prod-principal-collect-frequency 1)
                         ;;(prod-grace-period :none)
                         ;;(prod-grace-period :pure nil nil nil)
                         ;;(prod-grace-period :principal nil nil nil)
                         ;;(prod-repayment-rounding :no-round :no-round)
                         (prod-non-working-days-reschedule :no) ;; :no :forward :backward :extend


                         (prod-schedule-edit {:dates? true
                                              :principal? true
                                              :num? false      ;; only available for :dynamic-term
                                              :holidays? true
                                              :interest? true ;; only available for :fixed-term
                                              :fee? true      ;; only available for :fixed-term
                                              :penalty? true  ;; only available for :fixed-term
                                              })


                         (prod-repayment-collection {:payment-allocation-method :horizontal ;; :horizontal :vertcal
                                                     :payment-allocation-order [:fee :penalty :interest :principal] ;; :fee :penalty :interest :principal
                                                     :pre-payments-accept :accept ;; :no :accept
                                                     :pre-payments-apply-interest nil ;; nil :auto :manual - nil if previous :no
                                                     :pre-payments-recalculation nil ;; :none :next-installments :reduce-term :reduce-amount
                                                     :overdue-payments :increase-installments ;; :increase-installments
                                                     :pre-payments-future-interest :none ;; :none :accept :accept-future
                                                     :allow-custom-repayment-allocation nil}) ;; nil true false


                         (prod-arrears-tolerance-period-constrain nil nil nil)
                         (prod-arrears-tolerance-amount-constrain nil nil nil)


                         (prod-arrears-settings {:arrears-days-calculated-from  :first
                                                 :non-working-days :include
                                                 :arrears-floor 1000
                                                 :accrue-late-interest true})))


  )