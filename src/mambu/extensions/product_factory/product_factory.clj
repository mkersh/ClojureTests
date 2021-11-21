;;; Support functions to build a product definition that can be into the Mambu product create API endpoints:
;;; - https://api.mambu.com/#loan-products-create
;;; - https://api.mambu.com/#deposit-products-create
;;;
;;; NOTE: Will be concentrating on loan products initially
;;;
;;; One of the challenges that this library will support is controlling and checking what the legal product-feature combinations
;;; are. This is important because the Mambu core endpoints do not currently check that the configuration is legal and if an
;;; illegal combination is passed there will not be an error but the product will not work properly.
;;;
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
;; prod-type = (:fixed-term|:dynamic-term|:interest-free|:tranched|:revolving-credit)
;;
(defn prod-type-def [prod-def prod-type]
  (assert (#{:fixed-term :dynamic-term :interest-free :tranched :revolving-credit} prod-type) (str "ERROR: Invalid prod-type: " prod-type))
  (assoc prod-def :prod-type prod-type))


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
;; :fixed-flat - only availble if prod-type-def=:fixed-term|
(defn prod-instalment-calc-type [prod-def calc-type]
  (assert (#{:db :emi :emi2 :fixed-flat} calc-type) (str "ERROR: Invalid prod-instalment-calc-type: " calc-type))
  (DEP-CHECK ":fixed-flat pre-conditions" prod-def :prod-instalment-calc-type :fixed-flat calc-type
             [[:prod-type #{:fixed-term}]])
  (assoc prod-def :prod-instalment-calc-type
         calc-type))

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


(comment

(-> {}
    (prod-name-id-desc "Product name XXX" "PROD1a" "")
    (prod-type-def :dynamic-term)
    ;;(prod-type-def :fixed-term)
    (prod-avail :client [])
    (prod-accid :random-pattern "@@@@###")
    (prod-initial-state :pend-approval) ;; Optional step
    (prod-amount-constrain 0 1000 500)  ;; Optional step
    (prod-under-ca-setting :no)
    (prod-instalment-calc-type :emi2)
    ;;(prod-instalment-calc-type :fixed-flat) ;; pre-conditions as to when this val is possible
    (prod-payment-interval-method :interval :months 1 nil)
    (prod-payment-interval-method :fixed nil nil [1 3 4])
    
    )

;;
)