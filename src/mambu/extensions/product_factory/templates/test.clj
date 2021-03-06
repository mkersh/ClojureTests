{"securitySettings" {"isGuarantorsEnabled" false, "isCollateralEnabled" false},
 "creationDate" "2022-07-11T15:04:03+02:00",
 "taxSettings" {"taxesOnInterestEnabled" false, "taxesOnFeesEnabled" false, "taxesOnPenaltyEnabled" false},
 "feesSettings" {"allowArbitraryFees" false, "fees" []},
 "loanAmountSettings" {"loanAmount" {}, "trancheSettings" {"maxNumberOfTranches" 1}},
 "fundingSettings" {"enabled" false},
 "currency" {"code" "EUR"},
 "accountLinkSettings" {"enabled" false, "linkedAccountOptions" [], "settlementMethod" "FULL_DUE_AMOUNTS"},
 "allowCustomRepaymentAllocation" true,
 "id" "PROD1a",
 "lastModifiedDate" "2022-07-11T15:06:08+02:00",
 "creditArrangementSettings" {"creditArrangementRequirement" "OPTIONAL"},
 "name" "PROD1a XXX",
 "arrearsSettings"
 {"encodedKey" "8a818e9881da3d4b0181ed5c0fca01f8",
  "toleranceCalculationMethod" "ARREARS_TOLERANCE_PERIOD",
  "dateCalculationMethod" "ACCOUNT_FIRST_WENT_TO_ARREARS",
  "nonWorkingDaysMethod" "INCLUDED",
  "toleranceFloorAmount" 1000.0,
  "tolerancePeriod" {"defaultValue" 0, "minValue" 0, "maxValue" 77},
  "tolerancePercentageOfOutstandingPrincipal" {"minValue" 0, "maxValue" 0, "defaultValue" 0}},
 "adjustInterestForFirstInstallment" false,
 "newAccountSettings"
 {"idGeneratorType" "RANDOM_PATTERN", "idPattern" "@@@@###", "accountInitialState" "PENDING_APPROVAL"},
 "internalControls" {"lockSettings" {}, "fourEyesPrinciple" {"activeForLoanApproval" false}},
 "encodedKey" "8a818e9881da3d4b0181ecef7d277a2a",
 "gracePeriodSettings" {"gracePeriod" {}, "gracePeriodType" "NONE"},
 "templates" [],
 "type" "DYNAMIC_TERM_LOAN",
 "penaltySettings" {"penaltyRate" {}, "loanPenaltyCalculationMethod" "NONE"},
 "state" "ACTIVE",
 "category" "PERSONAL_LENDING",
 "notes" "",
 "interestSettings"
 {"daysInYear" "ACTUAL_365_FIXED",
  "interestApplicationMethod" "REPAYMENT_DUE_DATE",
  "accrueLateInterest" true,
  "interestRateSettings" [],
  "interestCalculationMethod" "DECLINING_BALANCE_DISCOUNTED",
  "indexRateSettings"
  {"interestChargeFrequencyCount" 1,
   "interestChargeFrequency" "ANNUALIZED",
   "interestRateTerms" "FIXED",
   "allowNegativeInterestRate" false,
   "encodedKey" "8a818e9881da3d4b0181ecef7d277a2c",
   "interestRateSource" "FIXED_INTEREST_RATE",
   "interestRate" {},
   "interestRateTiers" [],
   "accrueInterestAfterMaturity" false},
  "scheduleInterestDaysCountMethod" "ACTUAL_DAYS_COUNT",
  "interestBalanceCalculationMethod" "ONLY_PRINCIPAL",
  "interestType" "SIMPLE_INTEREST"},
 "scheduleSettings"
 {"firstRepaymentDueDateOffset" {},
  "repaymentPeriodUnit" "MONTHS",
  "fixedDaysOfMonth" [],
  "roundingSettings"
  {"roundingRepaymentScheduleMethod" "ROUND_REMAINDER_INTO_LAST_REPAYMENT",
   "repaymentCurrencyRounding" "NO_ROUNDING",
   "repaymentElementsRoundingMethod" "NO_ROUNDING"},
  "repaymentReschedulingMethod" "NONE",
  "repaymentScheduleEditOptions" [],
  "scheduleDueDatesMethod" "INTERVAL",
  "repaymentScheduleMethod" "DYNAMIC",
  "numInstallments" {},
  "defaultRepaymentPeriodCount" 1},
 "accountingSettings"
 {"accountingMethod" "NONE",
  "interestAccruedAccountingMethod" "NONE",
  "interestAccrualCalculation" "NONE",
  "accountingRules" []},
 "availabilitySettings"
 {"branchSettings" {"forAllBranches" false, "availableProductBranches" ["8a818f3a81da42c10181de262e9d4089"]},
  "availableFor" ["INDIVIDUALS"]},
 "paymentSettings"
 {"paymentMethod" "HORIZONTAL",
  "amortizationMethod" "BALLOON_PAYMENTS",
  "prepaymentSettings"
  {"prepaymentRecalculationMethod" "REDUCE_NUMBER_OF_INSTALLMENTS_NEW",
   "elementsRecalculationMethod" "TOTAL_EXPECTED_FIXED",
   "prepaymentAcceptance" "ACCEPT_PREPAYMENTS",
   "futurePaymentsAcceptance" "NO_FUTURE_PAYMENTS",
   "applyInterestOnPrepaymentMethod" "AUTOMATIC",
   "principalPaidInstallmentStatus" "PAID"},
  "latePaymentsRecalculationMethod" "LAST_INSTALLMENT_INCREASE",
  "repaymentAllocationOrder" ["FEE" "PENALTY" "INTEREST" "PRINCIPAL"],
  "principalPaymentSettings" {"amount" {}, "percentage" {}, "defaultPrincipalRepaymentInterval" 1}}}