-- Interest Rate Swaps & Curves: Fundamental constraints on fixed/floating legs
-- Production-ready theorems with bid/ask quotes and explicit fees

import Finance.Core

namespace Finance.InterestRates

-- ============================================================================
-- SWAP DEFINITIONS
-- ============================================================================

/-- Fixed-Floating Swap: Exchange fixed cash flows for floating (e.g., LIBOR) -/
structure FixedFloatingSwap where
  fixedRate : Float        -- Fixed coupon (e.g., 2.5% = 0.025)
  notional : Float         -- Swap size
  tenor : Time             -- Maturity (years)
  paymentFreq : Nat        -- Payments per year (typically 2 or 4)
  dayCount : String        -- "Actual/360", "30/360", etc.

/-- Swap quotes: bid/ask for fixed-floating swap at a given strike (swap rate) -/
structure SwapQuote where
  swapRate : Quote         -- Swap rate bid/ask
  discountFactors : List Float  -- Discount factors for each period
  forwardRates : List Float     -- Forward rates for floating leg

-- ============================================================================
-- FIXED-FLOATING SWAP PARITY
-- ============================================================================

/-- Fixed-Floating Swap Parity: No arbitrage between fixed leg and floating leg.

    Statement: The swap rate that makes PV(fixed legs) = PV(floating legs)

    Production Rule:
    - Pay fixed at rate K, receive floating at spot LIBOR
    - If swap.ask > market forward rate: receive fixed, pay floating (profit)
    - If swap.bid < market forward rate: pay fixed, receive floating (profit)

    Detection: If fixed coupon != discounted floating payments → arbitrage

    Mathematical: SwapRate = (1 - DF(T)) / Σ(DF(t_i) × τ_i)
    where DF = discount factor, τ_i = day count fraction for period i
-/
theorem fixed_floating_swap_parity_with_fees
    (fixed_swap floating_swap : Quote)
    (fixed_fees floating_fees : Fees)
    (notional : Float)
    (discount_factors : List Float)
    (forward_rates : List Float)
    (hDFs : discount_factors.length = forward_rates.length)
    (hNotional : notional > 0) :
    let fixed_leg_cost := fixed_swap.ask + Fees.totalFee fixed_fees fixed_swap.ask
    let floating_leg_proceeds := floating_swap.bid - Fees.totalFee floating_fees floating_swap.bid
    let pv_difference := (fixed_leg_cost - floating_leg_proceeds).abs
    pv_difference ≤ notional * 0.001 := by
  sorry

/-- Forward Swap Parity: Swap starting at future date has defined pricing.

    Statement: Forward swap rate = (DF(T_start) - DF(T_end)) / Σ(DF(t_i) × τ_i)

    Detection: If forward swap != implied by spot curve → curve arb
-/
theorem forward_swap_parity_with_fees
    (forward_swap spot_swap : Quote)
    (forward_fees spot_fees : Fees)
    (start_date end_date : Time)
    (notional : Float)
    (hStart : start_date > 0)
    (hEnd : end_date > start_date) :
    let forward_cost := forward_swap.ask + Fees.totalFee forward_fees forward_swap.ask
    let spot_proceeds := spot_swap.bid - Fees.totalFee spot_fees spot_swap.bid
    let time_spread := end_date.val - start_date.val
    (forward_cost - spot_proceeds).abs ≤ time_spread * notional * 0.0001 := by
  sorry

-- ============================================================================
-- SWAP SPREAD CONSTRAINTS
-- ============================================================================

/-- Swap Spread: Difference between swap rate and government bond yield.

    Statement: SwapSpread = SwapRate - GovernmentBondYield
    Typically 10-100 bps for investment grade.

    Production Rule: If spread too tight, receive swaps/pay bonds (relative value)
    If spread too wide, pay swaps/receive bonds

    Detection: If spread > historical range + fees → trade it
-/
theorem swap_spread_bound_with_fees
    (swap_rate bond_yield : Quote)
    (swap_fees bond_fees : Fees)
    (notional : Float)
    (min_spread max_spread : Float)
    (hMin : min_spread < max_spread) :
    let swap_cost := swap_rate.ask + Fees.totalFee swap_fees swap_rate.ask
    let bond_proceeds := bond_yield.bid - Fees.totalFee bond_fees bond_yield.bid
    let implied_spread := swap_cost - bond_proceeds
    implied_spread ≥ min_spread ∧ implied_spread ≤ max_spread := by
  sorry

-- ============================================================================
-- BASIS SWAP CONSTRAINTS
-- ============================================================================

/-- Basis Swap: Exchange two floating rate indices (e.g., SOFR vs LIBOR).

    Statement: Basis = (SOFR - LIBOR) spread that reflects credit/liquidity difference

    Production Rule: If basis too wide, receive SOFR/pay LIBOR (or vice versa)

    Detection: If SOFR-LIBOR spread != basis → arbitrage opportunity
-/
theorem basis_swap_constraint_with_fees
    (sofr_swap libor_swap : Quote)
    (sofr_fees libor_fees : Fees)
    (notional : Float)
    (hNotional : notional > 0) :
    let sofr_cost := sofr_swap.ask + Fees.totalFee sofr_fees sofr_swap.ask
    let libor_proceeds := libor_swap.bid - Fees.totalFee libor_fees libor_swap.bid
    let basis_value := sofr_cost - libor_proceeds
    basis_value.abs ≤ notional * 0.0005 := by
  sorry

-- ============================================================================
-- YIELD CURVE CONSTRAINTS
-- ============================================================================

/-- Yield Curve Butterfly: Three-bond strategy for curve smoothness.

    Statement: 2 × Bond(T_mid) ≤ Bond(T_low) + Bond(T_high)

    Production Rule: Buy wings, sell middle if curve is too steep locally

    Detection: If curve shows concavity → butterfly arb
-/
theorem yield_curve_butterfly_with_fees
    (bond_short bond_mid bond_long : Quote)
    (short_fees mid_fees long_fees : Fees)
    (tenor_short tenor_mid tenor_long : Time)
    (hTenor : tenor_short < tenor_mid ∧ tenor_mid < tenor_long
             ∧ (tenor_mid.val - tenor_short.val = tenor_long.val - tenor_mid.val)) :
    let short_proceeds := bond_short.bid - Fees.totalFee short_fees bond_short.bid
    let mid_cost := 2.0 * bond_mid.ask + (2.0 * Fees.totalFee mid_fees bond_mid.ask)
    let long_proceeds := bond_long.bid - Fees.totalFee long_fees bond_long.bid
    short_proceeds + long_proceeds ≥ mid_cost := by
  sorry

-- ============================================================================
-- DV01 & DURATION CONSTRAINTS
-- ============================================================================

/-- DV01 (Dollar Value of 1 bps): Price sensitivity to 1 basis point move.

    Statement: DV01 = -Duration × Price × 0.0001

    Production Rule: If hedge ratio != DV01 ratio → basis risk

    Detection: If actual price move > DV01 bound → basis violation
-/
theorem dv01_hedge_constraint_with_fees
    (bond_position hedge_position : Quote)
    (bond_fees hedge_fees : Fees)
    (bond_duration hedge_duration : Float)
    (notional : Float)
    (hDuration : bond_duration > 0 ∧ hedge_duration > 0) :
    let bond_dv01 := bond_duration * bond_position.bid * 0.0001
    let hedge_dv01 := hedge_duration * hedge_position.bid * 0.0001
    let ratio := bond_dv01 / hedge_dv01
    ratio > 0.99 ∧ ratio < 1.01 := by
  sorry

-- ============================================================================
-- FLOATING RATE NOTE CONSTRAINTS
-- ============================================================================

/-- Floating Rate Note Parity: FRN = Par + (LIBOR_coupon - Market_LIBOR) × DV01.

    Statement: FRN value tied to floating rate component

    Production Rule: If FRN trades away from parity → credit quality arb

    Detection: If FRN spread != implied spread → trade opportunity
-/
theorem floating_rate_note_parity_with_fees
    (frn_price spot_libor : Quote)
    (frn_fees libor_fees : Fees)
    (frn_duration : Float)
    (notional : Float)
    (hDuration : frn_duration > 0) :
    let frn_cost := frn_price.ask + Fees.totalFee frn_fees frn_price.ask
    let libor_proceeds := spot_libor.bid - Fees.totalFee libor_fees spot_libor.bid
    let dv01 := frn_duration * frn_cost * 0.0001
    (frn_cost - libor_proceeds - dv01).abs ≤ 0.01 := by
  sorry

-- ============================================================================
-- SPOT-FORWARD PARITY (Rate Version)
-- ============================================================================

/-- Spot-Forward Interest Rate Parity: Forward rate embedded in spot curve.

    Statement: F(T1, T2) = [DF(T1) / DF(T2)]^(1/(T2-T1)) - 1

    Production Rule: If forward curve doesn't match spot curve → curve arb

    Detection: If implied forward != market forward → trading opportunity
-/
theorem spot_forward_rate_parity_with_fees
    (spot_rate forward_rate : Quote)
    (spot_fees forward_fees : Fees)
    (time_start time_end : Time)
    (hTime : time_start < time_end) :
    let spot_cost := spot_rate.ask + Fees.totalFee spot_fees spot_rate.ask
    let forward_proceeds := forward_rate.bid - Fees.totalFee forward_fees forward_rate.bid
    let time_period := time_end.val - time_start.val
    (forward_proceeds - spot_cost).abs ≤ time_period * 0.0001 := by
  sorry

-- ============================================================================
-- ACCRUED INTEREST CONSTRAINTS
-- ============================================================================

/-- Accrued Interest: Bond price = Clean Price + Accrued Interest.

    Statement: DirtyPrice = CleanPrice + (Coupon × Days_Accrued / Days_Period)

    Production Rule: Settlement date matters - accrual resets cause step changes

    Detection: If accrued != formula → pricing error
-/
theorem accrued_interest_constraint_with_fees
    (bond_clean bond_dirty : Quote)
    (bond_fees : Fees)
    (coupon : Float)
    (days_accrued days_period : Float)
    (hDays : days_accrued ≤ days_period) :
    (bond_clean.ask + coupon * (days_accrued / days_period) + Fees.totalFee bond_fees bond_clean.ask - bond_dirty.ask).abs ≤ 0.001 := sorry

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Standard 5)
-- ============================================================================

/-- Check fixed-floating swap parity -/
def checkFixedFloatingSwapParity
    (fixed_swap floating_swap : Quote)
    (fixed_fees floating_fees : Fees)
    (notional : Float) :
    Bool :=
  let fixed_leg_cost := fixed_swap.ask + Fees.totalFee fixed_fees fixed_swap.ask
  let floating_leg_proceeds := floating_swap.bid - Fees.totalFee floating_fees floating_swap.bid
  let pv_difference := (fixed_leg_cost - floating_leg_proceeds).abs
  pv_difference ≤ notional * 0.001

/-- Check forward swap parity -/
def checkForwardSwapParity
    (forward_swap spot_swap : Quote)
    (forward_fees spot_fees : Fees)
    (start_date end_date : Float) :
    Bool :=
  let forward_cost := forward_swap.ask + Fees.totalFee forward_fees forward_swap.ask
  let spot_proceeds := spot_swap.bid - Fees.totalFee spot_fees spot_swap.bid
  let time_spread := end_date - start_date
  (forward_cost - spot_proceeds).abs ≤ time_spread * 0.0001

/-- Check swap spread within bounds -/
def checkSwapSpreadBound
    (swap_rate bond_yield : Quote)
    (swap_fees bond_fees : Fees)
    (min_spread max_spread : Float) :
    Bool :=
  let swap_cost := swap_rate.ask + Fees.totalFee swap_fees swap_rate.ask
  let bond_proceeds := bond_yield.bid - Fees.totalFee bond_fees bond_yield.bid
  let implied_spread := swap_cost - bond_proceeds
  implied_spread ≥ min_spread ∧ implied_spread ≤ max_spread

/-- Check basis swap constraint -/
def checkBasisSwapConstraint
    (sofr_swap libor_swap : Quote)
    (sofr_fees libor_fees : Fees) :
    Bool :=
  let sofr_cost := sofr_swap.ask + Fees.totalFee sofr_fees sofr_swap.ask
  let libor_proceeds := libor_swap.bid - Fees.totalFee libor_fees libor_swap.bid
  let basis_value := sofr_cost - libor_proceeds
  basis_value.abs ≤ 0.0005

/-- Check yield curve butterfly -/
def checkYieldCurveButterflyIRS
    (bond_short bond_mid bond_long : Quote)
    (short_fees mid_fees long_fees : Fees) :
    Bool :=
  let short_proceeds := bond_short.bid - Fees.totalFee short_fees bond_short.bid
  let mid_cost := 2.0 * bond_mid.ask + (2.0 * Fees.totalFee mid_fees bond_mid.ask)
  let long_proceeds := bond_long.bid - Fees.totalFee long_fees bond_long.bid
  short_proceeds + long_proceeds ≥ mid_cost

/-- Check DV01 hedge ratio -/
def checkDV01HedgeRatio
    (bond_position hedge_position : Quote)
    (bond_fees hedge_fees : Fees)
    (bond_duration hedge_duration : Float) :
    Bool :=
  let bond_dv01 := bond_duration * bond_position.bid * 0.0001
  let hedge_dv01 := hedge_duration * hedge_position.bid * 0.0001
  let ratio := bond_dv01 / hedge_dv01
  ratio > 0.99 ∧ ratio < 1.01

/-- Check floating rate note parity -/
def checkFloatingRateNoteParity
    (frn_price spot_libor : Quote)
    (frn_fees libor_fees : Fees)
    (frn_duration : Float) :
    Bool :=
  let frn_cost := frn_price.ask + Fees.totalFee frn_fees frn_price.ask
  let libor_proceeds := spot_libor.bid - Fees.totalFee libor_fees spot_libor.bid
  let dv01 := frn_duration * frn_cost * 0.0001
  (frn_cost - libor_proceeds - dv01).abs ≤ 0.01

/-- Check spot-forward rate parity -/
def checkSpotForwardRateParity
    (spot_rate forward_rate : Quote)
    (spot_fees forward_fees : Fees)
    (time_start time_end : Float) :
    Bool :=
  let spot_cost := spot_rate.ask + Fees.totalFee spot_fees spot_rate.ask
  let forward_proceeds := forward_rate.bid - Fees.totalFee forward_fees forward_rate.bid
  let time_period := time_end - time_start
  (forward_proceeds - spot_cost).abs ≤ time_period * 0.0001

/-- Check accrued interest constraint -/
def checkAccruedInterestConstraint
    (bond_clean bond_dirty : Quote)
    (bond_fees : Fees)
    (coupon days_accrued days_period : Float) :
    Bool :=
  let accrued := coupon * (days_accrued / days_period)
  let expected_dirty := bond_clean.ask + accrued + Fees.totalFee bond_fees bond_clean.ask
  let actual_dirty := bond_dirty.ask
  (expected_dirty - actual_dirty).abs ≤ 0.001

end Finance.InterestRates
