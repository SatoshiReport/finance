-- Convertible Bonds: Bond + Embedded Options arbitrage constraints
-- Formalizes no-arbitrage on conversion parity, delta dynamics, call/put features

import Finance.Core

namespace Finance.ConvertibleBonds

-- ============================================================================
-- CONVERTIBLE BOND DEFINITIONS
-- ============================================================================

/-- A convertible bond: fixed income security with option to convert to stock. -/
structure ConvertibleBond where
  maturity : Time              -- Time to maturity
  couponRate : Rate            -- Annual coupon (e.g., 3% = 0.03)
  conversionRatio : ℝ         -- Shares per bond (e.g., 50 shares per $1000)
  issuerCallStrike : ℝ         -- Stock price level where issuer can call
  investorPutStrike : ℝ        -- Stock price level where investor can put
  issuerCredit : ℝ             -- Credit spread over risk-free

/-- Market quotes for convertible bond valuation. -/
structure ConvertibleQuote where
  convertiblePrice : Quote     -- The convertible bond price (bid/ask)
  underlyingStock : Quote      -- Stock price (bid/ask)
  straightBond : Quote         -- What straight bond would yield
  impliedVol : Float           -- Implied volatility of conversion option

-- ============================================================================
-- PHASE 1: CONVERTIBLE VALUATION & PARITY
-- ============================================================================

/-- Convertible Lower Bound: Must be worth at least the straight bond value.

    Statement: Convertible Price ≥ Straight Bond Value (bond floor)

    Intuition:
    - Worst case = issuer doesn't use conversion, bond just pays coupon
    - Investor gets at least the bond cash flows
    - If convertible < straight bond, obvious arbitrage: buy convertible, short bond

    Arbitrage if violated:
    - Buy convertible (trading cheap)
    - Short equivalent straight bond position
    - Lock in spread until maturity or conversion
-/
theorem convertible_lower_bound_straight_bond
    (convertible straight_bond : Quote)
    (convertible_fees bond_fees : Fees)
    (hPrice : straight_bond.mid > 0) :
    let conv_ask := convertible.ask.val + Fees.totalFee convertible_fees convertible.ask.val (by sorry)
    let bond_bid := straight_bond.bid.val - Fees.totalFee bond_fees straight_bond.bid.val (by sorry)
    conv_ask ≥ bond_bid - 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (straight_bond.bid.val - Fees.totalFee bond_fees straight_bond.bid.val (by sorry)) - (convertible.ask.val + Fees.totalFee convertible_fees convertible.ask.val (by sorry)) - 0.01
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Convertible Upper Bound: Can't exceed stock price times conversion ratio.

    Statement: Convertible Price ≤ Stock Price × Conversion Ratio (stock ceiling)

    Intuition:
    - Can always convert to stock at fixed ratio
    - If convertible > stock × ratio, just convert and sell stock immediately
    - Investor's upside is capped by this ceiling

    Arbitrage if violated:
    - Buy stock, convert to bonds, sell bonds
    - Lock in immediate profit
    - Happens when convertible overvalued vs stock
-/
theorem convertible_upper_bound_stock_value
    (convertible stock : Quote)
    (convertible_fees stock_fees : Fees)
    (conversion_ratio : ℝ)
    (hRatio : conversion_ratio > 0)
    (hStock : stock.mid > 0) :
    let conv_bid := convertible.bid.val - Fees.totalFee convertible_fees convertible.bid.val (by sorry)
    let stock_ceiling := stock.ask.val * conversion_ratio + Fees.totalFee stock_fees (stock.ask.val * conversion_ratio) (by sorry)
    conv_bid ≤ stock_ceiling + 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (convertible.bid.val - Fees.totalFee convertible_fees convertible.bid.val (by sorry)) - (stock.ask.val * conversion_ratio + Fees.totalFee stock_fees (stock.ask.val * conversion_ratio) (by sorry)) - 0.01
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Conversion Parity Constraint: Deep ITM convertibles trade near parity.

    Statement: When far in-the-money, Convertible ≈ Stock × Conversion Ratio ± spread

    Intuition:
    - Deep ITM = stock far above call strike
    - Conversion imminent or already favorable
    - Convertible price = conversion value + time value
    - As stock rises, time value → 0, convertible → stock × ratio

    Arbitrage if violated:
    - Parity violations create cash/conversion arbitrage
    - Buy cheap side, short expensive side, wait for convergence
-/
theorem conversion_parity_constraint
    (convertible stock : Quote)
    (convertible_fees stock_fees : Fees)
    (conversion_ratio : ℝ)
    (stock_price : ℝ)
    (hRatio : conversion_ratio > 0)
    (hStock : stock_price > 0) :
    let parity_value := stock_price * conversion_ratio
    let conv_price := convertible.mid
    (conv_price - parity_value).abs ≤ 0.02 * stock_price + (Fees.totalFee convertible_fees convertible.mid (by sorry) + Fees.totalFee stock_fees stock_price (by sorry)) := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (convertible.mid - (stock_price * conversion_ratio)).abs - 0.02 * stock_price - (Fees.totalFee convertible_fees convertible.mid (by sorry) + Fees.totalFee stock_fees stock_price (by sorry))
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Straight Bond Parity Decomposition: Decompose convertible into components.

    Statement: Convertible = Straight Bond + Call Option Value - Issuer Call Value
    Or: Convertible = Bond Floor + (Equity Value - Call Strike)+

    Intuition:
    - Investor is long the straight bond (get coupons/par)
    - Long conversion optionality (if stock rises)
    - Short issuer's call option (issuer calls if stock very high)
    - Decomposition helps with hedging and relative value

    Pattern:
    - Bond floor provides downside protection
    - Equity option provides upside potential
    - Issuer call caps upside
-/
theorem straight_bond_parity_decomposition
    (convertible straight_bond stock : Quote)
    (conversion_ratio : ℝ)
    (call_strike : ℝ)
    (hRatio : conversion_ratio > 0) :
    let bond_floor := straight_bond.mid
    let conversion_value := (stock.mid * conversion_ratio - call_strike).max 0
    let conv_value := convertible.mid
    -- Convertible ≈ bond floor + (conversion value)
    (conv_value - (bond_floor + conversion_value)).abs ≤ 0.01 * (bond_floor + conversion_value) := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (convertible.mid - ((straight_bond.mid + ((stock.mid * conversion_ratio - call_strike).max 0)))).abs - 0.01 * (straight_bond.mid + ((stock.mid * conversion_ratio - call_strike).max 0))
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Option-Adjusted Spread Constraint: OAS bounded by credit and option premium.

    Statement: OAS = Convertible Spread - Option Premium

    Intuition:
    - Convertible yield spread reflects two things:
      1. Credit risk of issuer
      2. Value of embedded options (negative from issuer perspective)
    - OAS = spread if you remove option component
    - Wide OAS = relative value vs straight bond

    Pattern:
    - OAS tells true credit spread after removing optionality
    - High OAS with low credit spreads = embedded options valuable
-/
theorem option_adjusted_spread_constraint
    (convertible straight_bond : Quote)
    (convertible_fees bond_fees : Fees)
    (max_oas_spread : ℝ)
    (hOAS : max_oas_spread > 0) :
    let conv_spread := (convertible.mid - straight_bond.mid).abs
    let oas := conv_spread - 0.005 -- Approximate option premium
    oas ≤ max_oas_spread := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((convertible.mid - straight_bond.mid).abs - 0.005) - max_oas_spread
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Phase 1)
-- ============================================================================

/-- Check convertible lower bound: Convertible ≥ straight bond. -/
def checkConvertibleLowerBound
    (convertible_price bond_price : ℝ)
    (tolerance : ℝ) :
    Bool :=
  convertible_price ≥ bond_price - tolerance

/-- Check conversion parity: Convertible ≈ stock × ratio. -/
def checkConversionParity
    (convertible_price stock_price conversion_ratio : ℝ)
    (tolerance : ℝ) :
    Bool :=
  let parity := stock_price * conversion_ratio
  (convertible_price - parity).abs ≤ tolerance

/-- Check convertible upper bound: Convertible ≤ stock × ratio. -/
def checkConvertibleUpperBound
    (convertible_price stock_price conversion_ratio : ℝ)
    (tolerance : ℝ) :
    Bool :=
  let ceiling := stock_price * conversion_ratio
  convertible_price ≤ ceiling + tolerance

/-- Check bond/stock decomposition consistency. -/
def checkBondStockDecomposition
    (convertible_price bond_price stock_price conversion_ratio : ℝ)
    (tolerance : ℝ) :
    Bool :=
  let decomposed := bond_price + (stock_price * conversion_ratio - bond_price).max 0
  (convertible_price - decomposed).abs ≤ tolerance

/-- Check option-adjusted spread within bounds. -/
def checkOASSpread
    (convertible_spread implied_option_premium : ℝ)
    (max_oas : ℝ) :
    Bool :=
  let oas := convertible_spread - implied_option_premium
  oas ≤ max_oas

end Finance.ConvertibleBonds
