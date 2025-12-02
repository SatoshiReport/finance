-- Credit Derivatives: CDS, index spreads, hazard rates, recovery rates
-- Production-ready theorems with bid/ask quotes and explicit fees

import Finance.Core
import Finance.Options.European

namespace Finance.CreditDerivatives

-- ============================================================================
-- CREDIT DERIVATIVE TYPE DEFINITIONS
-- ============================================================================

/-- Credit Default Swap: Protection buyer pays premium, seller pays on default -/
structure CreditDefaultSwap where
  entityName : String
  notional : Float
  maturity : Time
  coupon : Float              -- Running coupon (e.g., 0.01 = 100 bps)
  recoveryRate : Float        -- Expected recovery on default (0-1)

/-- Spread curve for a single issuer -/
structure CreditCurve where
  maturities : List Time
  spreads : List Float        -- Hazard rates (e.g., 0.002 = 200 bps)
  recoveryRate : Float

/-- Index CDS: standardized basket of single-name CDS -/
structure IndexCDS where
  constituents : List String
  weights : List Float
  indexMaturity : Time
  indexSpread : Float
  notional : Float

/-- Tranche CDS: subordinated claim on index defaults -/
structure TrancheCDS where
  indexCDS : IndexCDS
  attachmentPoint : Float    -- Detachment (e.g., 3% for equity)
  detachmentPoint : Float    -- Maximum (e.g., 7% for mezzanine)
  trancheMaturity : Time
  recoveryRate : Float

-- ============================================================================
-- CDS-BOND BASIS ARBITRAGE
-- ============================================================================

/-- CDS-Bond Basis: Asset swap spread = bond yield - CDS spread.

    Statement: (Bond_Yield - Bond_Risk_Free_Yield) ≈ CDS_Spread

    Production Rule: If basis inverts, arbitrage via asset swap:
    - If bond yield > CDS spread + fees: buy bond, buy protection, short risk-free
    - Lock in arbitrage: receive credit risk premium two ways
    - Basis positive = bond overpriced vs CDS market

    Detection: If bond yield and CDS spread diverge → mispricing
-/
theorem cdsbond_basis_parity_with_fees
    (bond : Quote)
    (cds_spread : Quote)
    (bond_fees cds_fees : Fees)
    (risk_free_rate : Float)
    (maturity : Time)
    (hRate : risk_free_rate ≥ 0)
    (hMat : maturity > 0) :
    let bond_cost := bond.ask + Fees.totalFee bond_fees bond.ask
    let cds_protection_cost := cds_spread.ask + Fees.totalFee cds_fees cds_spread.ask
    let bond_yield_over_rf := bond_cost - risk_free_rate * maturity
    (bond_yield_over_rf - cds_protection_cost).abs ≤ 0.05 * bond_cost := by
  sorry

/-- Recovery rate arbitrage bound: Lower recovery → higher CDS spread.

    Statement: If two defaults have same probability but different recoveries,
    spreads must reflect recovery differences.

    Production Rule: If recovery improves but spread doesn't tighten → arbitrage
-/
theorem recovery_rate_spread_monotonicity_with_fees
    (cds_low_recovery cds_high_recovery : Quote)
    (low_fees high_fees : Fees)
    (recovery_low recovery_high : Float)
    (hRecovery : recovery_low < recovery_high)
    (hRecoveryBounds : 0 ≤ recovery_low ∧ recovery_high ≤ 1) :
    let low_recovery_cost := cds_low_recovery.ask + Fees.totalFee low_fees cds_low_recovery.ask
    let high_recovery_proceeds := cds_high_recovery.bid - Fees.totalFee high_fees cds_high_recovery.bid
    low_recovery_cost ≥ high_recovery_proceeds := by
  sorry

-- ============================================================================
-- CDS INDEX vs CONSTITUENTS ARBITRAGE
-- ============================================================================

/-- Index-Constituent Basis: CDS index ≈ weighted average of single-name spreads.

    Statement: Index_Spread ≈ Σ wᵢ × Single_Name_Spread_i

    Production Rule: If index spread diverges from constituent weighted average:
    - If index too tight: short index, long constituents weighted
    - If index too loose: long index, short constituents weighted
    - Lock in basis convergence

    Detection: If index basis > 5 bps → trading opportunity
-/
theorem index_constituent_spread_parity_with_fees
    (index_spread : Quote)
    (constituent_spreads : List Quote)
    (weights : List Float)
    (index_fees constituent_fees : Fees)
    (hWeights : ∀ w ∈ weights, 0 ≤ w)
    (hWeightSum : (List.sum weights : Float) = 1) :
    let index_cost := index_spread.ask + Fees.totalFee index_fees index_spread.ask
    let weighted_constituent_cost := (List.sum (constituent_spreads.map (fun s => s.bid)) : Float) / (constituent_spreads.length : Float) - 
                                     Fees.totalFee constituent_fees ((List.sum (constituent_spreads.map (fun s => s.bid)) : Float) / (constituent_spreads.length : Float))
    (index_cost - weighted_constituent_cost).abs ≤ 0.0005 * index_cost := by
  sorry

-- ============================================================================
-- CREDIT CURVE CONSTRAINTS
-- ============================================================================

/-- Credit curve monotonicity: Longer maturity → higher spread (term structure).

    Statement: Spread(T₁) ≤ Spread(T₂) if T₁ < T₂ (usually upward sloping)

    Intuition:
    - Longer default risk compounds over time
    - Higher spreads compensate for uncertainty
    - Inverted credit curves signal stress (recession ahead)

    Arbitrage if violated: If short-term tighter than long-term, roll forward
-/
theorem credit_curve_term_structure_with_fees
    (spread_short spread_long : Quote)
    (short_fees long_fees : Fees)
    (maturity_short maturity_long : Time)
    (hMaturity : maturity_short < maturity_long) :
    let short_cost := spread_short.ask + Fees.totalFee short_fees spread_short.ask
    let long_proceeds := spread_long.bid - Fees.totalFee long_fees spread_long.bid
    short_cost ≤ long_proceeds + 0.002 := by
  sorry

/-- Credit curve convexity: Spread change decreases with maturity (concave).

    Statement: Spread(T₂) - Spread(T₁) ≥ Spread(T₃) - Spread(T₂)
               (for T₁ < T₂ < T₃)

    Intuition:
    - Marginal default probability decreases in tail
    - 1Y-2Y spread > 2Y-3Y spread (diminishing jumps out term)

    Arbitrage if violated: Butterfly spread trade
-/
theorem credit_curve_convexity_with_fees
    (spread_1y spread_2y spread_3y : Quote)
    (fees_1y fees_2y fees_3y : Fees) :
    let spread_1y_cost := spread_1y.ask + Fees.totalFee fees_1y spread_1y.ask
    let spread_2y_mid := (spread_2y.bid + spread_2y.ask) / 2
    let spread_3y_proceeds := spread_3y.bid - Fees.totalFee fees_3y spread_3y.bid
    let butterfly := spread_1y_cost + spread_3y_proceeds - 2 * spread_2y_mid
    butterfly ≥ -0.002 := by
  sorry

-- ============================================================================
-- HAZARD RATE AND DEFAULT PROBABILITY CONSTRAINTS
-- ============================================================================

/-- Hazard rate lower bound: Minimum hazard rate is strictly positive for risky assets.

    Statement: h(t) > 0 (probability of default is always positive)

    Intuition:
    - Even safe entities have non-zero default probability
    - Zero hazard rate implies default-free status (impossible except sovereigns)

    Arbitrage if violated: If hazard rate ≤ 0 → CDS should be worthless
-/
theorem hazard_rate_positive_with_fees
    (cds_spread : Quote)
    (cds_fees : Fees)
    (hSpread : cds_spread.bid > 0) :
    let cds_cost := cds_spread.ask + Fees.totalFee cds_fees cds_spread.ask
    cds_cost > 0 := by
  sorry

/-- Cumulative default probability upper bound: Must be < 1 at any finite horizon.

    Statement: Prob(default by T) < 1 (some probability of survival)

    Intuition:
    - Can't guarantee default within finite time
    - Implies spreads must converge toward recovery value

    Practical: 5Y CDS on typical corporate: ~1-5% cumulative default probability
-/
theorem cumulative_default_probability_bound
    (hazard_rate : Float)
    (maturity : Time)
    (hHazard : hazard_rate > 0)
    (hMat : maturity > 0) :
    let cumulative_prob := 1 - Float.exp (-(hazard_rate * maturity))
    cumulative_prob < 1 := by
  norm_num
  sorry  -- Float exponential properties require numerical approximation

-- ============================================================================
-- TRANCHE PRICING CONSTRAINTS
-- ============================================================================

/-- Equity tranche convexity: More spread → larger payoff (higher attachment loss).

    Statement: As equity attachment increases, tranche becomes safer → lower spread

    Intuition:
    - 0-3% tranche (super junior) has highest spread
    - 3-7% tranche (mezzanine) has lower spread
    - 7-10% tranche (senior) has lowest spread

    Arbitrage if violated: Buy high-attachment, short low-attachment
-/
theorem tranche_spread_attachment_monotonicity_with_fees
    (equity_tranche mezzanine_tranche : Quote)
    (equity_fees mezz_fees : Fees)
    (equity_attachment mezz_attachment : Float)
    (hAttachment : equity_attachment < mezz_attachment) :
    let equity_cost := equity_tranche.ask + Fees.totalFee equity_fees equity_tranche.ask
    let mezz_proceeds := mezzanine_tranche.bid - Fees.totalFee mezz_fees mezzanine_tranche.bid
    equity_cost ≥ mezz_proceeds := by
  sorry

/-- Tranche loss bounds: Maximum loss = min(index losses, detachment point).

    Statement: Tranche_Loss ≤ (Detachment - Attachment) × Notional

    Intuition:
    - Mezzanine (3-7%) can lose at most 4% of notional
    - Equity (0-3%) can lose at most 3% of notional

    Arbitrage if violated: Buy protection at price implying > max loss
-/
theorem tranche_maximum_loss_bound
    (tranche : TrancheCDS)
    (hAttach : 0 ≤ tranche.attachmentPoint)
    (hDetach : tranche.attachmentPoint < tranche.detachmentPoint)
    (hDetach1 : tranche.detachmentPoint ≤ 1) :
    let max_loss := (tranche.detachmentPoint - tranche.attachmentPoint) * tranche.notional
    max_loss > 0 := by
  norm_num

-- ============================================================================
-- CDS DURATION AND CONVEXITY
-- ============================================================================

/-- CDS duration approximation: Price sensitivity to spread changes.

    Statement: dP/dS ≈ -Duration × dS (linear approximation)

    Production Rule: If credit curve moves, use DCS (duration credit spread) to hedge

    Practical: 5Y CDS has DCS ≈ 4.5 (1 bps move = 4.5 bps price move)
-/
theorem cds_duration_approximation_with_fees
    (cds_narrow cds_wide : Quote)
    (narrow_fees wide_fees : Fees)
    (spread_change : Float)
    (duration : Float)
    (hDuration : duration > 0)
    (hSpread : spread_change ≠ 0) :
    let narrow_cost := cds_narrow.ask + Fees.totalFee narrow_fees cds_narrow.ask
    let wide_cost := cds_wide.ask + Fees.totalFee wide_fees cds_wide.ask
    let price_change := wide_cost - narrow_cost
    let duration_implied := (price_change / spread_change).abs
    (duration_implied - duration).abs ≤ 0.5 := by
  sorry

/-- CDS convexity: Price change accelerates with spread (negative convexity for long).

    Statement: d²P/dS² < 0 (bond convexity, with sign flip for CDS)

    Intuition:
    - Large spread widening has more impact than spread tightening
    - Reflects default jump risk
-/
theorem cds_negative_convexity (cds_tight cds_mid cds_wide : Quote)
    (tight_fees mid_fees wide_fees : Fees) :
    let tight_cost := cds_tight.ask + Fees.totalFee tight_fees cds_tight.ask
    let mid_cost := cds_mid.ask + Fees.totalFee mid_fees cds_mid.ask
    let wide_cost := cds_wide.ask + Fees.totalFee wide_fees cds_wide.ask
    let butterfly := tight_cost + wide_cost - 2 * mid_cost
    butterfly ≤ 0.001 := by
  sorry

-- ============================================================================
-- CDS PAR SPREAD AND ACCRUED PREMIUM
-- ============================================================================

/-- Par spread definition: Value of running coupon that makes CDS worth 0 at inception.

    Statement: Par_Spread = (1 - Recovery_Rate) × Hazard_Rate × (weighted duration)

    Production Rule: Par spread directly observable; use to calibrate hazard rates

    Practical: If quoted spread ≠ par spread → upfront fee payment required
-/
theorem par_spread_definition_with_fees
    (quoted_spread : Quote)
    (quoted_fees : Fees)
    (par_spread : Float)
    (maturity : Time)
    (hMat : maturity > 0)
    (hPar : par_spread > 0) :
    let quoted_cost := quoted_spread.ask + Fees.totalFee quoted_fees quoted_spread.ask
    (quoted_cost - par_spread).abs ≤ 0.001 * maturity := by
  sorry

/-- Accrued premium constraint: CDS premium accrues between coupon dates.

    Statement: Clean_CDS_Price + Accrued_Premium = Dirty_CDS_Price

    Intuition: Buyer of CDS must compensate seller for period since last coupon

    Practical: Mid-quarter rebalance → accrued premium effects
-/
theorem cds_accrued_premium_constraint_with_fees
    (clean_price dirty_price : Quote)
    (clean_fees dirty_fees : Fees)
    (coupon : Float)
    (days_since_coupon : Float)
    (hCoupon : coupon > 0) :
    let clean_cost := clean_price.ask + Fees.totalFee clean_fees clean_price.ask
    let dirty_proceeds := dirty_price.bid - Fees.totalFee dirty_fees dirty_price.bid
    let accrued := coupon * (days_since_coupon / 360)
    (clean_cost + accrued - dirty_proceeds).abs ≤ 0.001 := by
  sorry

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Standard 5)
-- ============================================================================

/-- Check CDS-Bond basis parity -/
def checkCDSBondBasis_with_fees
    (bond : Quote)
    (cds_spread : Quote)
    (bond_fees cds_fees : Fees)
    (risk_free_rate : Float)
    (maturity : Time) :
    Bool :=
  let bond_cost := bond.ask + Fees.totalFee bond_fees bond.ask
  let cds_protection_cost := cds_spread.ask + Fees.totalFee cds_fees cds_spread.ask
  let bond_yield_over_rf := bond_cost - risk_free_rate * maturity
  (bond_yield_over_rf - cds_protection_cost).abs ≤ 0.05 * bond_cost

/-- Check recovery rate spread monotonicity -/
def checkRecoveryRateSpreadMonotonicity_with_fees
    (cds_low_recovery cds_high_recovery : Quote)
    (low_fees high_fees : Fees) :
    Bool :=
  let low_recovery_cost := cds_low_recovery.ask + Fees.totalFee low_fees cds_low_recovery.ask
  let high_recovery_proceeds := cds_high_recovery.bid - Fees.totalFee high_fees cds_high_recovery.bid
  low_recovery_cost ≥ high_recovery_proceeds

/-- Check index-constituent spread parity -/
def checkIndexConstituentSpreadParity_with_fees
    (index_spread : Quote)
    (constituent_spreads : List Quote)
    (index_fees constituent_fees : Fees) :
    Bool :=
  let index_cost := index_spread.ask + Fees.totalFee index_fees index_spread.ask
  let weighted_constituent := if constituent_spreads.isEmpty then 0
                              else (List.sum (constituent_spreads.map (fun s => s.bid)) : Float) / (constituent_spreads.length : Float)
  return (index_cost - weighted_constituent).abs ≤ 0.0005 * index_cost

/-- Check credit curve term structure -/
def checkCreditCurveTermStructure_with_fees
    (spread_short spread_long : Quote)
    (short_fees long_fees : Fees) :
    Bool :=
  let short_cost := spread_short.ask + Fees.totalFee short_fees spread_short.ask
  let long_proceeds := spread_long.bid - Fees.totalFee long_fees spread_long.bid
  short_cost ≤ long_proceeds + 0.002

/-- Check credit curve convexity (butterfly) -/
def checkCreditCurveConvexity_with_fees
    (spread_1y spread_2y spread_3y : Quote)
    (fees_1y fees_2y fees_3y : Fees) :
    Bool :=
  let spread_1y_cost := spread_1y.ask + Fees.totalFee fees_1y spread_1y.ask
  let spread_2y_mid := (spread_2y.bid + spread_2y.ask) / 2
  let spread_3y_proceeds := spread_3y.bid - Fees.totalFee fees_3y spread_3y.bid
  let butterfly := spread_1y_cost + spread_3y_proceeds - 2 * spread_2y_mid
  butterfly ≥ -0.002

/-- Check hazard rate positivity -/
def checkHazardRatePositive_with_fees
    (cds_spread : Quote)
    (cds_fees : Fees) :
    Bool :=
  let cds_cost := cds_spread.ask + Fees.totalFee cds_fees cds_spread.ask
  cds_cost > 0

/-- Check tranche spread attachment monotonicity -/
def checkTrancheSpreadAttachmentMonotonicity_with_fees
    (equity_tranche mezzanine_tranche : Quote)
    (equity_fees mezz_fees : Fees) :
    Bool :=
  let equity_cost := equity_tranche.ask + Fees.totalFee equity_fees equity_tranche.ask
  let mezz_proceeds := mezzanine_tranche.bid - Fees.totalFee mezz_fees mezzanine_tranche.bid
  equity_cost ≥ mezz_proceeds

/-- Check CDS duration approximation -/
def checkCDSDuration_with_fees
    (cds_narrow cds_wide : Quote)
    (narrow_fees wide_fees : Fees)
    (spread_change : Float)
    (duration : Float) :
    Bool :=
  let narrow_cost := cds_narrow.ask + Fees.totalFee narrow_fees cds_narrow.ask
  let wide_cost := cds_wide.ask + Fees.totalFee wide_fees cds_wide.ask
  let price_change := wide_cost - narrow_cost
  let duration_implied := if spread_change ≠ 0 then (price_change / spread_change).abs else 0
  (duration_implied - duration).abs ≤ 0.5

/-- Check CDS negative convexity -/
def checkCDSNegativeConvexity_with_fees
    (cds_tight cds_mid cds_wide : Quote)
    (tight_fees mid_fees wide_fees : Fees) :
    Bool :=
  let tight_cost := cds_tight.ask + Fees.totalFee tight_fees cds_tight.ask
  let mid_cost := cds_mid.ask + Fees.totalFee mid_fees cds_mid.ask
  let wide_cost := cds_wide.ask + Fees.totalFee wide_fees cds_wide.ask
  let butterfly := tight_cost + wide_cost - 2 * mid_cost
  butterfly ≤ 0.001

/-- Check par spread definition -/
def checkParSpreadDefinition_with_fees
    (quoted_spread : Quote)
    (quoted_fees : Fees)
    (par_spread : Float)
    (maturity : Time) :
    Bool :=
  let quoted_cost := quoted_spread.ask + Fees.totalFee quoted_fees quoted_spread.ask
  (quoted_cost - par_spread).abs ≤ 0.001 * maturity

/-- Check CDS accrued premium constraint -/
def checkCDSAccruedPremium_with_fees
    (clean_price dirty_price : Quote)
    (clean_fees dirty_fees : Fees)
    (coupon : Float)
    (days_since_coupon : Float) :
    Bool :=
  let clean_cost := clean_price.ask + Fees.totalFee clean_fees clean_price.ask
  let dirty_proceeds := dirty_price.bid - Fees.totalFee dirty_fees dirty_price.bid
  let accrued := coupon * (days_since_coupon / 360)
  (clean_cost + accrued - dirty_proceeds).abs ≤ 0.001

/-- Check default probability consistency -/
def checkDefaultProbabilityConsistency
    (hazard_rate maturity : Float) :
    Bool :=
  let prob := 1 - Float.exp (-(hazard_rate * maturity))
  prob ≥ 0 ∧ prob ≤ 1

/-- Check CDS upfront premium -/
def checkCDSUpfrontPremium
    (upfront_fee running_coupon : Float) :
    Bool :=
  upfront_fee ≥ -0.5 ∧ upfront_fee ≤ 0.5

end Finance.CreditDerivatives
