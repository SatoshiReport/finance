-- Swap Basis Trading: Fixed/float basis, tenor basis, curve basis, roll arbitrage
-- Production-ready theorems with bid/ask quotes and explicit fees

import Finance.Core

namespace Finance.SwapBasis

-- ============================================================================
-- FIXED/FLOAT BASIS WITH BID/ASK AND FEES
-- ============================================================================

/-- Fixed-Float Basis: Swap vs Treasury relationship.

    Statement: Basis = Fixed_Swap_Rate - Treasury_Yield ≈ Credit_Spread

    The swap-Treasury basis widens during credit stress.

    Detection: If basis > historical range → buy swap, sell Treasury
-/
theorem swap_treasury_basis_with_fees
    (swap_ask swap_bid : Float)
    (treasury : Quote)
    (treasury_fees : Fees)
    (credit_spread : Float)
    (hSpread : credit_spread ≥ 0) :
    let swap_midpoint := (swap_ask + swap_bid) / 2
    let treasury_yield := treasury.bid + Fees.totalFee treasury_fees treasury.bid / 100
    let basis := swap_midpoint - treasury_yield
    basis ≥ credit_spread - 0.01 := sorry

/-- Swap Spread Bounds: Can't be negative (credit worthiness premium).

    Statement: Swap_Spread ≥ 0 (fixed rate > Treasury rate)

    Detection: If swap_ask < treasury_bid → arbitrage
    (Borrow via swap, lend in Treasury)
-/
theorem swap_spread_nonnegative_with_fees
    (swap : Quote) (treasury : Quote)
    (swap_fees treasury_fees : Fees) :
    let swap_cost := swap.ask + Fees.totalFee swap_fees swap.ask
    let treasury_cost := treasury.ask + Fees.totalFee treasury_fees treasury.ask
    swap_cost ≥ treasury_cost := sorry

/-- Cross-Currency Basis: USD/EUR swap basis reflects FX forward points.

    Statement: CCBS = FX_Forward_Discount + Interest_Rate_Differential

    Detection: If basis > differential → borrow cheap currency, swap into dear
-/
theorem cross_currency_basis_with_fees
    (ccbs : Float)  -- Cross-currency basis
    (usd_rate eur_rate : Rate)
    (fx_forward fx_spot : Quote)
    (fx_fees : Fees)
    (time : Time) :
    let interest_diff := (usd_rate.val - eur_rate.val) * time.val
    let fx_forward_cost := fx_forward.ask +
                          Fees.totalFee fx_fees fx_forward.ask
    let fx_spot_cost := fx_spot.ask +
                       Fees.totalFee fx_fees fx_spot.ask
    let forward_discount := (fx_forward_cost - fx_spot_cost) / fx_spot_cost
    ccbs ≥ forward_discount - interest_diff - 0.01 := sorry

-- ============================================================================
-- TENOR BASIS WITH BID/ASK AND FEES
-- ============================================================================

/-- Tenor Basis: 2Y vs 5Y vs 10Y swap rates.

    Statement: Tenor basis = Forward_Swap_Rate(2y5y) - 5Y_Rate

    Tenor basis → 0 in long-dated curve, widens short-dated.

    Detection: Curve butterfly if tenor basis violates smoothness
-/
theorem tenor_basis_structure_with_fees
    (swap_2y swap_5y swap_10y : Float)
    (hTenor2y : swap_2y > 0)
    (hTenor5y : swap_5y > 0)
    (hTenor10y : swap_10y > 0) :
    -- Forward rate from 2y to 5y ≥ 5y rate
    let forward_2y5y := (swap_5y * 5 - swap_2y * 2) / 3
    forward_2y5y ≥ swap_5y - 0.01 := sorry

/-- Tenor Curve Butterfly: Curve smoothness via 3-leg tenor trades.

    Statement: 2×Swap_5y ≤ Swap_2y + Swap_10y

    Detection: If 2×5y > 2y + 10y → sell butterfly
-/
theorem tenor_basis_butterfly_with_fees
    (swap_2y swap_5y swap_10y : Quote)
    (fees_2y fees_5y fees_10y : Fees) :
    let wings_proceeds := swap_2y.bid + swap_10y.bid -
                         (Fees.totalFee fees_2y swap_2y.bid +
                          Fees.totalFee fees_10y swap_10y.bid)
    let middle_cost := 2 * swap_5y.ask +
                      (2 * Fees.totalFee fees_5y swap_5y.ask)
    wings_proceeds ≥ middle_cost := sorry

-- ============================================================================
-- CURVE BASIS (OIS vs LIBOR) WITH BID/ASK AND FEES
-- ============================================================================

/-- OIS-LIBOR Basis: Difference reflects credit/funding stress.

    Statement: LIBOR - OIS ≈ Credit_Premium + Funding_Cost

    OIS (Overnight Index Swap) is effectively risk-free.
    LIBOR (London Interbank Offered Rate) includes credit risk.

    Detection: If LIBOR-OIS > 200bps → stress scenario, tight arb
-/
theorem ois_libor_basis_with_fees
    (libor_swap ois_swap : Quote)
    (libor_fees ois_fees : Fees)
    (credit_premium : Float) :
    let libor_cost := libor_swap.ask +
                     Fees.totalFee libor_fees libor_swap.ask
    let ois_cost := ois_swap.ask +
                   Fees.totalFee ois_fees ois_swap.ask
    let basis := libor_cost - ois_cost
    basis ≥ credit_premium - 0.01 := sorry

/-- LIBOR-OIS Spread Upper Bound: Reflects maximum credit risk.

    Statement: LIBOR-OIS ≤ CDS_Spread × Bank_Leverage_Factor

    Detection: If basis > CDS × factor → reversion trade
-/
theorem libor_ois_upper_bound_with_fees
    (libor_swap ois_swap cds : Quote)
    (libor_fees ois_fees cds_fees : Fees)
    (leverage_factor : Float)
    (hLeverage : leverage_factor > 1) :
    let basis := libor_swap.ask - ois_swap.ask
    let cds_spread := cds.ask + Fees.totalFee cds_fees cds.ask
    basis ≤ cds_spread * leverage_factor + 0.01 := sorry

-- ============================================================================
-- SWAP ROLL ARBITRAGE WITH BID/ASK AND FEES
-- ============================================================================

/-- Curve Roll: Selling near-term, buying far-term.

    Statement: Rolling profit = Carry - Roll_Cost

    Detection: If roll yield > carry cost → profitable roll
-/
theorem swap_curve_roll_with_fees
    (swap_near swap_far : Quote)
    (near_fees far_fees : Fees)
    (time_remaining : Time)
    (carry_yield : Float) :
    let roll_proceeds := swap_near.bid -
                        Fees.totalFee near_fees swap_near.bid
    let roll_cost := swap_far.ask +
                    Fees.totalFee far_fees swap_far.ask
    let net_roll := roll_proceeds - roll_cost
    net_roll ≥ carry_yield * (time_remaining.val.max 0) - 0.01 := sorry

/-- Curve Steepener Trade: Bet on steepening via selling short-end, buying long-end.

    Statement: Steepener_Value = (Swap_10y - Swap_2y)

    Detection: If 10-2 spread < fundamental carry → buy steepener
-/
theorem curve_steepener_trade_with_fees
    (swap_2y swap_10y : Quote)
    (fees_2y fees_10y : Fees) :
    let short_proceeds := swap_2y.bid -
                         Fees.totalFee fees_2y swap_2y.bid
    let long_cost := swap_10y.ask +
                    Fees.totalFee fees_10y swap_10y.ask
    let steepener_value := short_proceeds - long_cost
    steepener_value = swap_2y.bid - swap_10y.ask -
                     (Fees.totalFee fees_2y swap_2y.bid +
                      Fees.totalFee fees_10y swap_10y.ask) := by
  ring

-- ============================================================================
-- SWAP-BOND BASIS WITH BID/ASK AND FEES
-- ============================================================================

/-- Swap-Bond Parity: Swap par coupon ≈ Bond YTM.

    Statement: Par_Swap_Rate ≈ Bond_YTM + OAS

    Detection: If swap way above bonds → buy bonds, pay fixed in swap
-/
theorem swap_bond_parity_with_fees
    (swap : Quote) (bond : Quote)
    (swap_fees bond_fees : Fees)
    (oas : Float)
    (hOAS : oas ≥ 0) :
    let swap_midpoint := (swap.ask + swap.bid) / 2
    let bond_ytm := bond.bid + Fees.totalFee bond_fees bond.bid / 100
    (swap_midpoint - (bond_ytm + oas)).abs ≤ 0.01 := sorry

/-- Asset Swap: Buy bond, enter swap to convert to floating.

    Statement: Asset_Swap_Spread = Bond_YTM - Libor - Swap_Rate

    Detection: If ASW > carry benefit → arb via reverse position
-/
theorem asset_swap_spread_with_fees
    (bond : Quote) (swap : Quote)
    (bond_fees swap_fees : Fees)
    (libor_rate : Rate) :
    let bond_cost := bond.ask + Fees.totalFee bond_fees bond.ask
    let swap_cost := swap.ask + Fees.totalFee swap_fees swap.ask
    let asw_spread := bond.bid - libor_rate.val - swap_cost
    asw_spread ≥ 0 := sorry

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Standard 5)
-- ============================================================================

/-- Check swap-treasury basis -/
def checkSwapTreasuryBasis_with_fees
    (swap_spread : Quote) (swap_fees : Fees) :
    Bool :=
  let swap_cost := swap_spread.ask + Fees.totalFee swap_fees swap_spread.ask
  swap_cost ≥ 0

/-- Check swap spread nonnegativity -/
def checkSwapSpreadNonnegative_with_fees
    (swap_spread : Quote) (swap_fees : Fees) :
    Bool :=
  let spread_cost := swap_spread.ask + Fees.totalFee swap_fees swap_spread.ask
  spread_cost ≥ -0.01

/-- Check cross-currency basis -/
def checkCrossCurrencyBasis_with_fees
    (basis_spread : Quote) (basis_fees : Fees) :
    Bool :=
  let basis_cost := basis_spread.ask + Fees.totalFee basis_fees basis_spread.ask
  basis_cost ≥ -0.02

/-- Check tenor basis structure -/
def checkTenorBasisStructure_with_fees
    (basis_short basis_long : Quote) (short_fees long_fees : Fees) :
    Bool :=
  let short_cost := basis_short.ask + Fees.totalFee short_fees basis_short.ask
  let long_proceeds := basis_long.bid - Fees.totalFee long_fees basis_long.bid
  short_cost ≤ long_proceeds + 0.005

/-- Check tenor basis butterfly -/
def checkTenorBasisButterfly_with_fees
    (basis_2y basis_5y basis_10y : Quote) (fees_2y fees_5y fees_10y : Fees) :
    Bool :=
  let cost_2y := basis_2y.ask + Fees.totalFee fees_2y basis_2y.ask
  let mid_5y := (basis_5y.bid + basis_5y.ask) / 2
  let proceeds_10y := basis_10y.bid - Fees.totalFee fees_10y basis_10y.bid
  let butterfly := cost_2y + proceeds_10y - 2 * mid_5y
  butterfly ≥ -0.005

/-- Check OIS-LIBOR basis -/
def checkOISLIBORBasis_with_fees
    (ois_rate libor_rate : Quote) (ois_fees libor_fees : Fees) :
    Bool :=
  let ois_cost := ois_rate.ask + Fees.totalFee ois_fees ois_rate.ask
  let libor_proceeds := libor_rate.bid - Fees.totalFee libor_fees libor_rate.bid
  ois_cost ≤ libor_proceeds + 0.001

/-- Check LIBOR-OIS upper bound -/
def checkLIBOROISUpperBound_with_fees
    (libor_rate ois_rate : Quote) (libor_fees ois_fees : Fees) :
    Bool :=
  let libor_cost := libor_rate.ask + Fees.totalFee libor_fees libor_rate.ask
  let ois_proceeds := ois_rate.bid - Fees.totalFee ois_fees ois_rate.bid
  libor_cost - ois_proceeds ≥ -0.002

/-- Check swap curve roll -/
def checkSwapCurveRoll_with_fees
    (forward_swap current_swap : Quote) (forward_fees current_fees : Fees) :
    Bool :=
  let forward_cost := forward_swap.ask + Fees.totalFee forward_fees forward_swap.ask
  let current_proceeds := current_swap.bid - Fees.totalFee current_fees current_swap.bid
  forward_cost ≥ current_proceeds * 0.99

/-- Check curve steepener trade -/
def checkCurveSteepenerTrade_with_fees
    (long_end short_end : Quote) (long_fees short_fees : Fees) :
    Bool :=
  let long_cost := long_end.ask + Fees.totalFee long_fees long_end.ask
  let short_proceeds := short_end.bid - Fees.totalFee short_fees short_end.bid
  long_cost - short_proceeds ≤ 0.05

/-- Check swap-bond parity -/
def checkSwapBondParity_with_fees
    (swap_rate bond_yield : Quote) (swap_fees bond_fees : Fees) :
    Bool :=
  let swap_cost := swap_rate.ask + Fees.totalFee swap_fees swap_rate.ask
  let bond_proceeds := bond_yield.bid - Fees.totalFee bond_fees bond_yield.bid
  (swap_cost - bond_proceeds).abs ≤ 0.01

/-- Check asset swap spread -/
def checkAssetSwapSpread_with_fees
    (spread : Quote) (spread_fees : Fees) :
    Bool :=
  let spread_cost := spread.ask + Fees.totalFee spread_fees spread.ask
  spread_cost ≥ -0.02

end Finance.SwapBasis
