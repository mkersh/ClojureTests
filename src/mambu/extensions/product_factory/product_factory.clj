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
(ns mambu.extensions.product-factory.product-factory)

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

;; [STEP-1] Add product name and ID
(defn prod-name-id-desc
  ([prod-def name id desc]
   ;; default active to true
   (prod-name-id-desc prod-def name id desc true))
   ;; main entry point
  ([prod-def name id desc active]
   (let [prod-def2 (assoc prod-def :prod-name name)
         prod-def3 (assoc prod-def2 :prod-desc desc)
         prod-def4 (assoc prod-def3 :active active)]
     (assoc prod-def4 :prod-id id))))

;; [STEP-2] Add the product-type to the prod-def
;; product-type = (:fixed-term|:dynamic-term|:interest-free|:tranched|:revolving-credit)
;;
(defn product-type-def [prod-def product-type]
  (assert (#{:fixed-term :dynamic-term :interest-free :tranched :revolving-credit} product-type) (str "ERROR: Invalid product-type: " product-type))
  (assoc prod-def :product-type product-type))


;; [STEP-3] Add availability details to the prod-def
;; branches-list - List of branchid(s) that the product is available from
;;    NOTE: This is higher level than the Mambu core API that requires encodedKey
(defn prod-avail [prod-def cient-type-avail branches-list]
  (assert (#{:client :group :both} cient-type-avail) (str "ERROR: Invalid cient-type-avail: " cient-type-avail))
  (let [prod-def2 (assoc prod-def :prod-avail-clients cient-type-avail)]
    (assoc prod-def2 :prod-avail-branches branches-list)))

;; [STEP-4] Specify how the (internal) Accid will be generated
;;
(defn prod-accid [prod-def type template-or-start-num]
  (assert (#{:random-pattern :inc-number } type) (str "ERROR: Invalid prod-accid type: " type))
  (let [prod-def2 (assoc prod-def :accid-gen type)]
  (assoc prod-def2 :accid-gen-template template-or-start-num)))

;; [STEP-5] Specify initial state
(defn prod-initial-state [prod-def state]
  (assert (#{:pend-approval :partial-app } state) (str "ERROR: Invalid prod-initial-state state: " state))
  (assoc prod-def :initial-state state))

;; [STEP-6] Specify Amount Constraints
;; This step is optional. If not called then there will be no amount contraint
(defn prod-amount-constrain [prod-def min-amount max-amount def-amount]
  (assoc prod-def :amount-constraint
         {:min-amount min-amount
          :max-amount max-amount
          :def-amount def-amount}))

;; [STEP-7] Managed under a credit arrangement
(defn prod-under-ca-setting [prod-def ca-setting]
  (assert (#{:optional :required :no} ca-setting) (str "ERROR: Invalid prod-under-ca-setting: " ca-setting))
  (assoc prod-def :prod-under-ca-setting
         ca-setting))

;; [STEP-8] Define the instalment calculation type
;; :db - declining balance, principal reduced by equal amounts each instalment
;; :emi2 - refers to DBEI + Optimized Payments
;; :fixed-flat - only availble if product-type-def=:fixed-term|
(defn prod-instalment-calc-type [prod-def calc-type]
  (assert (#{:db :emi :emi2 :fixed-flat} calc-type) (str "ERROR: Invalid prod-instalment-calc-type: " calc-type))
  (DEP-CHECK ":fixed-flat pre-conditions" prod-def :prod-instalment-calc-type :fixed-flat calc-type
             [[:product-type #{:fixed-term}]])
  (assoc prod-def :prod-instalment-calc-type
         calc-type))

;; [STEP-8b] Interest posting frequency
(defn prod-interest-posting-freq [prod-def freq]
  (assert (#{:on-repayment :on-disbursement } freq) (str "ERROR: Invalid prod-interest-posting-freq: " freq))
  (DEP-CHECK ":fixed-flat pre-conditions" prod-def :prod-interest-posting-freq :on-disbursement freq
             [[:product-type #{:fixed-term}]])
  (assoc prod-def :prod-interest-posting-freq
         freq))

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

    (assert (#{:fixed :index} int-rate-source) (str "ERROR: Invalid prod-interest-type int-rate-source: " int-rate-source))
    (assert (#{:simple :capitalized :compound} int-rate-type) (str "ERROR: Invalid prod-interest-type int-type: " int-rate-type))
    (assert (#{:year :month :4weeks :week :day} int-rate-scope) (str "ERROR: Invalid prod-interest-type int-rate-scope: " int-rate-scope))
    (assert (#{:days :weeks :months} index-review-frequency-type) (str "ERROR: Invalid prod-interest-type index-review-frequency-type: " index-review-frequency-type))
    (assert (#{:30E-360 :actual-365 :actual-360} day-count-model) (str "ERROR: Invalid prod-interest-type day-count-model: " day-count-model))

   (-> prod-def
       (assoc :int-rate-source int-rate-source)
       (assoc :int-rate-type int-rate-type)
       (assoc :int-rate-scope int-rate-scope)
       (assoc :index-source index-source)
       (assoc :index-spread-constrain index-spread-constrain)
       (assoc :index-floor index-floor)
       (assoc :index-ceiling index-ceiling)
       (assoc :index-review-frequency-type index-review-frequency-type)
       (assoc :index-review-frequency-val index-review-frequency-val)
       (assoc :day-count-model day-count-model))))

;; [STEP-8c] Interest Type Settings
(defn prod-interest-rate-constrain [prod-def min-amount max-amount def-amount]
  (assoc prod-def :interest-rate-constraint
         {:min-amount min-amount
          :max-amount max-amount
          :def-amount def-amount}))

;; [STEP-8d] Repayments Interest Calculation
(defn prod-repayment-interest-calc [prod-def freq]
  (assert (#{:repayment-periodicity :actual-days} freq) (str "ERROR: Invalid prod-repayment-interest-calc: " freq))
  (assert (= (:product-type prod-def) :fixed-term) "ERROR: You can only set prod-repayment-interest-calc for :product-type = :fixed-term ")
  (assoc prod-def :prod-repayment-interest-calc
         freq))


;; [STEP-9] (re)Payment Interval
(defn prod-payment-interval-method [prod-def int-method int-period int-val fixed-days-list]
  (assert (#{:interval :fixed} int-method) (str "ERROR: Invalid prod-payment-interval-method: " int-method))
  (when (= int-method :interval)
    (assert (#{:days :weeks :months :years} int-period) (str "ERROR: Invalid prod-payment-interval-method period: " int-period)))
  (assoc prod-def :prod-payment-interval-method 
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
        (assoc  :repayment-amount-type repayment-amount-type)
        (assoc  :principal-amount-type principal-amount-type)
        (assoc  :total-amount-type total-amount-type)
        (assoc  :repayment-amount-constraint repayment-amount-constraint))
    
    ))


;; [STEP-10] Specify Installments Constraints
;; This step is optional. If not called then there will be no Instalment contraints
(defn prod-installments-constrain [prod-def min-num max-num def-num]
  (assoc prod-def :installments-constraint
         {:min-num min-num
          :max-num max-num
          :def-num def-num}))

;; [STEP-10b] Specify first-payment-date Constraints
;; This step is optional. If not called then there will be no first-payment-date contraints
(defn prod-first-payment-date-constrain [prod-def min-num max-num def-num]
  (assoc prod-def :first-payment-date-constraint
         {:min-num min-num
          :max-num max-num
          :def-num def-num}))

;; [STEP-11] Collect principal every n Installments
;; Normally should be set to 1 but can be less frequent
(defn prod-principal-collect-frequency [prod-def num]
  (assoc prod-def :prod-principal-collect-frequency num))

;; [STEP-12] Grace period settings
(defn prod-grace-period 
([prod-def grace-type] (prod-grace-period prod-def grace-type nil nil nil))
([prod-def grace-type grace-period-min grace-period-max grace-period-def ]
  (assert (#{:none :principal :pure} grace-type) (str "ERROR: Invalid prod-grace-period: " grace-type))
  (assoc prod-def :prod-grace-period
         {:grace-type grace-type
          :grace-period-constraint {:min grace-period-min :max grace-period-max :def grace-period-def}})))


;; [STEP-13] Repayment Rounding
(defn prod-repayment-rounding
  [prod-def install-round currency-round]
   (assert (#{:no-round :into-last} install-round) (str "ERROR: Invalid prod-repayment-rounding  install-round: " install-round))
   (assert (#{:no-round :round :round-up} currency-round) (str "ERROR: Invalid prod-repayment-rounding  currency-round: " currency-round))
   (assoc prod-def :prod-repayment-rounding
          {:install-round install-round
           :currency-round currency-round}))

;; [STEP-14] Non working days reschedule
(defn prod-non-working-days-reschedule
  [prod-def setting]
  (assert (#{:no :forward :backward :extend} setting) (str "ERROR: Invalid prod-non-working-days-reschedule: " setting))
  (assoc prod-def :prod-non-working-days-reschedule setting))

;; [STEP-15] Schedule Ediing
(defn prod-schedule-edit
  [prod-def edit-obj]
  (let [product-type (:product-type prod-def)]
    (when (not (= product-type :fixed-term))
      (assert (not (:interest? edit-obj)) "Setting only available for :fixed-term")
      (assert (not (:fee? edit-obj)) "Setting only available for :fixed-term")
      (assert (not (:penalty? edit-obj)) "Setting only available for :fixed-term"))
    (when (= product-type :tranched)
      (assert (not (or (:dates? edit-obj) (:principal? edit-obj) (:num? edit-obj) (:holidays? edit-obj)
                  (:interest? edit-obj) (:fee? edit-obj) (:penalty? edit-obj))) "edit schedule not available on :tranched"))
    (when (= product-type :revolving-credit)
      (assert (not (or (:principal? edit-obj) (:holidays? edit-obj)
                        (:interest? edit-obj) (:fee? edit-obj) (:penalty? edit-obj))) "edit schedule not available on :tranched")))
  (assoc prod-def :prod-schedule-edit 
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
        allow-custom-repayment-allocation (:allow-custom-repayment-allocation collect-settings)]

    ;;(assert (= (:product-type prod-def) :revolving-credit) "ERROR: prod-repayment-amount only available on :revolving-credit")
    (assert (#{:horizontal :vertcal} payment-allocation-method) (str "ERROR: Invalid prod-repayment-collection payment-allocation-method: " payment-allocation-method))
    (assert (#{:no :accept} pre-payments-accept) (str "ERROR: Invalid prod-repayment-collection pre-payments-accept: " pre-payments-accept))
    (assert (#{:auto :manual} pre-payments-apply-interest) (str "ERROR: Invalid prod-repayment-collection pre-payments-apply-interest: " pre-payments-apply-interest))
    (assert (#{:none :next-installments :reduce-term :reduce-amount} pre-payments-recalculation) (str "ERROR: Invalid prod-repayment-collection pre-payments-recalculation: " pre-payments-recalculation))
    (assert (#{:increase-installments} overdue-payments) (str "ERROR: Invalid prod-repayment-collection overdue-payments: " overdue-payments))
    (map (fn [order-item] (assert (#{:fee :penalty :interest :principal} order-item) (str "ERROR: Invalid prod-repayment-collection payment-allocation-order: " order-item))) payment-allocation-order)
    (assert (= (count (into #{} payment-allocation-order)) 4) "ERROR :payment-allocation-order needs order of all :fee :penalty :interest :principal defining")
    (assert (#{:none :accept} pre-payments-future-interest) (str "ERROR: Invalid prod-repayment-collection pre-payments-future-interest: " pre-payments-future-interest))
    (assert (#{true false} allow-custom-repayment-allocation) (str "ERROR: Invalid prod-repayment-collection allow-custom-repayment-allocation: " allow-custom-repayment-allocation))

    (-> prod-def
        (assoc  :payment-allocation-method payment-allocation-method)
        (assoc  :payment-allocation-order payment-allocation-order)
        (assoc  :pre-payments-accept pre-payments-accept)
        (assoc  :pre-payments-apply-interest pre-payments-apply-interest)
        (assoc  :pre-payments-recalculation pre-payments-recalculation)
        (assoc  :overdue-payments overdue-payments)
        (assoc  :pre-payments-future-interest pre-payments-future-interest)
        (assoc  :allow-custom-repayment-allocation allow-custom-repayment-allocation))))

;; [STEP-17] Arrears Tollerance Period Consraints
(defn prod-arrears-tolerance-period-constrain [prod-def min-num max-num def-num]
  (assoc prod-def :prod-arrears-tolerance-period-constrain
         {:min-num min-num
          :max-num max-num
          :def-num def-num}))

;; [STEP-18] Arrears Tollerance Consraints
(defn prod-arrears-tolerance-amount-constrain [prod-def min-num max-num def-num]
  (assoc prod-def :prod-arrears-tolerance-amount-constrain
         {:min-num min-num
          :max-num max-num
          :def-num def-num}))

;; [STEP-19] Arrears Processing
(defn prod-arrears-settings [prod-def collect-settings]
  (let [arrears-days-calculated-from (:arrears-days-calculated-from collect-settings)
        non-working-days (:non-working-days collect-settings)
        arrears-floor (:arrears-floor collect-settings)]
    
    ;;(assert (= (:product-type prod-def) :revolving-credit) "ERROR: prod-repayment-amount only available on :revolving-credit")
    (assert (#{:first :oldest} arrears-days-calculated-from) (str "ERROR: Invalid prod-repayment-collection payment-allocation-method: " arrears-days-calculated-from))
    (assert (#{:include :exclude} non-working-days) (str "ERROR: Invalid prod-repayment-collection payment-allocation-method: " non-working-days))

    (-> prod-def
        (assoc  :arrears-days-calculated-from arrears-days-calculated-from)
        (assoc  :non-working-days non-working-days)
        (assoc  :pre-payments-accept arrears-floor))))

(comment

(-> {}
    (prod-name-id-desc "Product name XXX" "PROD1a" "")
    (product-type-def :dynamic-term)
    ;;(product-type-def :fixed-term)
    (prod-avail :client [])
    (prod-accid :random-pattern "@@@@###")
    (prod-initial-state :pend-approval) ;; Optional step
    (prod-amount-constrain 0 1000 500)  ;; Optional step
    (prod-under-ca-setting :no)
    (prod-instalment-calc-type :emi2)
    ;;(prod-instalment-calc-type :fixed-flat) ;; pre-conditions as to when this val is possible
    (prod-interest-posting-freq :on-repayment)
    (prod-interest-type
     {:int-rate-source :fixed
      :int-rate-type :simple
      :index-source nil
      :index-spread-constrain {:min-amount 0 :max-amount 0 :def-amount 0}
      :index-floor 0
      :index-ceiling 0
      :index-review-frequency-type :months
      :index-review-frequency-val 0
      :int-rate-scope :year
      :day-count-model :actual-365})
    (prod-payment-interval-method :interval :months 1 nil)
    (prod-payment-interval-method :fixed nil nil [1 3 4])
    (prod-installments-constrain 0 600 10)
    ;; (prod-repayment-amount  ;; Only available on :product-type = :revolving-credit
    ;;  {:repayment-amount-type :principal
    ;;   :principal-amount-type :flat
    ;;   :total-amount-type :flat
    ;;   :repayment-amount-constraint {:min-amount 0 :max-amount 0 :def-amount 0}})
    (prod-first-payment-date-constrain 0 100 50)
    (prod-principal-collect-frequency 1)
    (prod-grace-period :none)
    (prod-grace-period :pure nil nil nil)
    (prod-grace-period :principal nil nil nil)
    (prod-repayment-rounding :no-round :no-round)
    (prod-non-working-days-reschedule :no)
    (prod-schedule-edit {:dates? false
                         :principal? false
                         :num? false
                         :holidays? false
                         :interest? false ;; only available for :fixed-term
                         :fee? false      ;; only available for :fixed-term
                         :penalty? false  ;; only available for :fixed-term
                         })
    (prod-repayment-collection {:payment-allocation-method :horizontal
                                :payment-allocation-order [:fee :penalty :interest :principal]
                                :pre-payments-accept :accept
                                :pre-payments-apply-interest :auto
                                :pre-payments-recalculation :next-installments
                                :overdue-payments :increase-installments
                                :pre-payments-future-interest :accept
                                :allow-custom-repayment-allocation true})

    (prod-arrears-tolerance-period-constrain 0 0 0)
    (prod-arrears-tolerance-amount-constrain 0 0 0)
    (prod-arrears-settings {:arrears-days-calculated-from  :first
                            :non-working-days :include
                            :arrears-floor 1000}))

(* 12 60)


)