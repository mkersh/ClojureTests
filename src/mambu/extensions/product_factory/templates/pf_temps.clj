;; template settings for Mambu product setup
;; See product_factory.clj - http://localhost:3000/goto-file?&bookmark=30c41be8-396e-491a-ac51-cec8b41b6859
(ns mambu.extensions.product_factory.templates.pf_temps)

;; Basic settings for a fixed-term based product
(defn ft-basics []
  {"securitySettings" {"isGuarantorsEnabled" false, "isCollateralEnabled" false},
   "creationDate" "2022-07-11T09:43:56+02:00",
   "taxSettings" {"taxesOnInterestEnabled" false, "taxesOnFeesEnabled" false, "taxesOnPenaltyEnabled" false},
   "feesSettings" {"allowArbitraryFees" false, "fees" []},
   "loanAmountSettings" {"loanAmount" {}, "trancheSettings" {"maxNumberOfTranches" 1}},
   "fundingSettings" {"enabled" false},
   "currency" {"code" "EUR"},
   "accountLinkSettings" {"enabled" false, "linkedAccountOptions" [], "settlementMethod" "FULL_DUE_AMOUNTS"},
   "allowCustomRepaymentAllocation" false,
   "id" "PF_FTB1",
   "lastModifiedDate" "2022-07-11T09:43:56+02:00",
   "creditArrangementSettings" {"creditArrangementRequirement" "OPTIONAL"},
   "name" "PF_FTB1",
   "arrearsSettings"
   {
    "toleranceCalculationMethod" "ARREARS_TOLERANCE_PERIOD",
    "dateCalculationMethod" "ACCOUNT_FIRST_WENT_TO_ARREARS",
    "nonWorkingDaysMethod" "EXCLUDED",
    "tolerancePeriod" {},
    "tolerancePercentageOfOutstandingPrincipal" {}},
   "adjustInterestForFirstInstallment" false,
   "newAccountSettings"
   {"idGeneratorType" "RANDOM_PATTERN", "idPattern" "@@@@###", "accountInitialState" "PENDING_APPROVAL"},
   "internalControls" {"lockSettings" {}, "fourEyesPrinciple" {"activeForLoanApproval" false}},
   
   "gracePeriodSettings" {"gracePeriod" {}, "gracePeriodType" "NONE"},
   "templates" [],
   "type" "FIXED_TERM_LOAN",
   "penaltySettings" {"penaltyRate" {}, "loanPenaltyCalculationMethod" "NONE"},
   "state" "ACTIVE",
   "category" "PERSONAL_LENDING",
   "notes" "",
   "interestSettings"
   {"daysInYear" "E30_360",
    "interestApplicationMethod" "AFTER_DISBURSEMENT",
    "accrueLateInterest" true,
    "interestRateSettings" [],
    "interestCalculationMethod" "FLAT",
    "indexRateSettings"
    {"interestChargeFrequencyCount" 1,
     "interestChargeFrequency" "ANNUALIZED",
     "interestRateTerms" "FIXED",
     "allowNegativeInterestRate" false,
     
     "interestRateSource" "FIXED_INTEREST_RATE",
     "interestRate" {},
     "interestRateTiers" [],
     "accrueInterestAfterMaturity" false},
    "scheduleInterestDaysCountMethod" "REPAYMENT_PERIODICITY",
    "interestBalanceCalculationMethod" "ONLY_PRINCIPAL",
    "interestType" "SIMPLE_INTEREST"},
   "scheduleSettings"
   {"repaymentScheduleMethod" "FIXED",
    "scheduleDueDatesMethod" "INTERVAL",
    "fixedDaysOfMonth" [],
    "roundingSettings"
    {"roundingRepaymentScheduleMethod" "ROUND_REMAINDER_INTO_LAST_REPAYMENT",
     "repaymentCurrencyRounding" "NO_ROUNDING",
     "repaymentElementsRoundingMethod" "NO_ROUNDING"},
    "numInstallments" {},
    "firstRepaymentDueDateOffset" {},
    "repaymentScheduleEditOptions" [],
    "repaymentReschedulingMethod" "NEXT_WORKING_DAY"},
   "accountingSettings"
   {"accountingMethod" "NONE",
    "interestAccruedAccountingMethod" "NONE",
    "interestAccrualCalculation" "NONE",
    "accountingRules" []},
   "availabilitySettings"
   {"branchSettings" {"forAllBranches" true, "availableProductBranches" []}, "availableFor" ["INDIVIDUALS"]},
   "paymentSettings"
   {"paymentMethod" "HORIZONTAL",
    "amortizationMethod" "STANDARD_PAYMENTS",
    "prepaymentSettings" {"prepaymentAcceptance" "ACCEPT_PREPAYMENTS", "futurePaymentsAcceptance" "ACCEPT_OVERPAYMENTS"},
    "latePaymentsRecalculationMethod" "OVERDUE_INSTALLMENTS_INCREASE",
    "repaymentAllocationOrder" ["FEE" "PENALTY" "INTEREST" "PRINCIPAL"],
    "principalPaymentSettings" {"amount" {}, "percentage" {}, "defaultPrincipalRepaymentInterval" 1}}})

;; Basic settings for a dynamic-term based product
(defn dt-basics []
  {"securitySettings" {"isGuarantorsEnabled" false, "isCollateralEnabled" false},
   "creationDate" "2022-07-10T18:59:04+02:00",
   "taxSettings" {"taxesOnInterestEnabled" false, "taxesOnFeesEnabled" false, "taxesOnPenaltyEnabled" false},
   "feesSettings" {"allowArbitraryFees" false, "fees" []},
   "loanAmountSettings" {"loanAmount" {}, "trancheSettings" {"maxNumberOfTranches" 1}},
   "fundingSettings" {"enabled" false},
   "currency" {"code" "EUR"},
   "accountLinkSettings" {"enabled" false, "linkedAccountOptions" [], "settlementMethod" "FULL_DUE_AMOUNTS"},
   "allowCustomRepaymentAllocation" false,
   "id" "PF_DTB1b",
   "lastModifiedDate" "2022-07-10T18:59:31+02:00",
   "creditArrangementSettings" {"creditArrangementRequirement" "OPTIONAL"},
   "name" "PF_DTB1b",
   "arrearsSettings"
   { 
    "toleranceCalculationMethod" "ARREARS_TOLERANCE_PERIOD",
    "dateCalculationMethod" "ACCOUNT_FIRST_WENT_TO_ARREARS",
    "nonWorkingDaysMethod" "EXCLUDED",
    "tolerancePeriod" {},
    "tolerancePercentageOfOutstandingPrincipal" {}},
   "adjustInterestForFirstInstallment" false,
   "newAccountSettings"
   {"idGeneratorType" "RANDOM_PATTERN", "idPattern" "@@@@###", "accountInitialState" "PENDING_APPROVAL"},
   "internalControls" {"lockSettings" {}, "fourEyesPrinciple" {"activeForLoanApproval" false}},
   "gracePeriodSettings" {"gracePeriod" {}, "gracePeriodType" "NONE"},
   "templates" [],
   "type" "DYNAMIC_TERM_LOAN",
   "penaltySettings" {"penaltyRate" {}, "loanPenaltyCalculationMethod" "NONE"},
   "state" "ACTIVE",
   "category" "PERSONAL_LENDING",
   "notes" "",
   "interestSettings"
   {"daysInYear" "E30_360",
    "interestApplicationMethod" "REPAYMENT_DUE_DATE",
    "accrueLateInterest" true,
    "interestRateSettings" [],
    "interestCalculationMethod" "DECLINING_BALANCE",
    "indexRateSettings"
    {"interestChargeFrequencyCount" 1,
     "interestChargeFrequency" "ANNUALIZED",
     "interestRateTerms" "FIXED",
     "allowNegativeInterestRate" false,
     
     "interestRateSource" "FIXED_INTEREST_RATE",
     "interestRate" {},
     "interestRateTiers" [],
     "accrueInterestAfterMaturity" false},
    "scheduleInterestDaysCountMethod" "ACTUAL_DAYS_COUNT",
    "interestBalanceCalculationMethod" "ONLY_PRINCIPAL",
    "interestType" "SIMPLE_INTEREST"},



   "scheduleSettings"
   {"repaymentScheduleMethod" "DYNAMIC",
    "scheduleDueDatesMethod" "INTERVAL",
    "fixedDaysOfMonth" [],
    "roundingSettings"
    {"roundingRepaymentScheduleMethod" "ROUND_REMAINDER_INTO_LAST_REPAYMENT",
     "repaymentCurrencyRounding" "NO_ROUNDING",
     "repaymentElementsRoundingMethod" "NO_ROUNDING"},
    "numInstallments" {},
    "firstRepaymentDueDateOffset" {},
    "repaymentScheduleEditOptions" [],
    "repaymentReschedulingMethod" "NEXT_WORKING_DAY"},
   "accountingSettings"
   {"accountingMethod" "NONE",
    "interestAccruedAccountingMethod" "NONE",
    "interestAccrualCalculation" "NONE",
    "accountingRules" []},
   "availabilitySettings"
   {"branchSettings" {"forAllBranches" true, "availableProductBranches" []}, "availableFor" ["INDIVIDUALS"]},
   "paymentSettings"
   {"paymentMethod" "HORIZONTAL",
    "amortizationMethod" "STANDARD_PAYMENTS",
    "prepaymentSettings"
    {"prepaymentRecalculationMethod" "NO_RECALCULATION",
     "prepaymentAcceptance" "ACCEPT_PREPAYMENTS",
     "futurePaymentsAcceptance" "NO_FUTURE_PAYMENTS",
     "applyInterestOnPrepaymentMethod" "AUTOMATIC",
     "principalPaidInstallmentStatus" "PARTIALLY_PAID"},
    "latePaymentsRecalculationMethod" "OVERDUE_INSTALLMENTS_INCREASE",
    "repaymentAllocationOrder" ["FEE" "PENALTY" "INTEREST" "PRINCIPAL"],
    "principalPaymentSettings" {"amount" {}, "percentage" {}, "defaultPrincipalRepaymentInterval" 1}}})


(comment
  (dt-basics))
  (ft-basics)

  ;;
      