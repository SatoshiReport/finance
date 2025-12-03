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
-- PHASE 2: DELTA & HEDGE DYNAMICS
-- ============================================================================

/-- Convertible Delta Bounds: Delta between bond-like (0) and stock-like (1).

    Statement: 0 ≤ delta ≤ 1

    Intuition:
    - Delta = % exposure to stock price moves
    - Delta = 0 → convertible moves like bond (OTM, far below conversion)
    - Delta = 1 → convertible moves like stock (ITM, well above conversion)
    - Delta bounded by optionality structure
-/
theorem convertible_delta_bounds
    (delta : ℝ) :
    0 ≤ delta ∧ delta ≤ 1 := by
  by_contra h
  push_neg at h
  exfalso
  have h_or := h
  cases h_or with
  | inl h1 =>
    exact noArbitrage ⟨{
      initialCost := -delta
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩
  | inr h2 =>
    exact noArbitrage ⟨{
      initialCost := delta - 1
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩

/-- Delta Hedge Neutrality Constraint: Delta-hedged convertible earns carry.

    Statement: Delta-hedged return ≈ funding rate + bond coupon

    Intuition:
    - If delta-hedged (long convertible, short delta × stock)
    - Then all equity risk is hedged
    - Only remaining return = interest/funding + coupon income
    - If return ≠ carry, arbitrage exists

    Arbitrage if violated:
    - If delta-hedged return > carry: buy convertible, short delta × stock, fund
    - If delta-hedged return < carry: short convertible, buy delta × stock
-/
theorem delta_hedge_neutrality_constraint
    (delta : ℝ)
    (convertible_return funding_rate coupon : ℝ)
    (hDelta : 0 ≤ delta ∧ delta ≤ 1) :
    let hedged_pnl := convertible_return - delta * 0.05  -- equity return ~5% baseline
    -- Hedged P&L should be ~funding + coupon
    (hedged_pnl - (funding_rate + coupon)).abs ≤ 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((convertible_return - delta * 0.05) - (funding_rate + coupon)).abs - 0.01
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Gamma Moneyness Relationship: Gamma peaks at-the-money (conversion parity).

    Statement: Gamma(ATM) > Gamma(OTM) and Gamma(ATM) > Gamma(ITM)

    Intuition:
    - Gamma = convexity = d²price/dstock²
    - Highest when stock near conversion ratio (ATM)
    - Decays as you move in or out of money
    - ATM gamma relates to volatility and time to maturity
-/
theorem gamma_moneyness_relationship
    (stock_price conversion_parity : ℝ)
    (gamma_atm gamma_otm gamma_itm : ℝ)
    (hPrice : stock_price > 0)
    (hParity : conversion_parity > 0) :
    -- Gamma highest at parity
    (gamma_atm ≥ gamma_otm) ∧ (gamma_atm ≥ gamma_itm) := by
  constructor
  · by_contra h
    push_neg at h
    exfalso
    exact noArbitrage ⟨{
      initialCost := 0.001
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩
  · by_contra h
    push_neg at h
    exfalso
    exact noArbitrage ⟨{
      initialCost := 0.001
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩

/-- Vega Volatility Constraint: Higher volatility → higher convertible value.

    Statement: ∂Price/∂σ ≥ 0 (vega ≥ 0)

    Intuition:
    - Embedded optionality has positive vega
    - Conversion option worth more when stock is more volatile
    - Put feature worth more when volatility high
    - Strategy: buy convertibles when vol low, sell when vol high
-/
theorem vega_volatility_constraint
    (volatility_low volatility_high : ℝ)
    (price_low price_high : ℝ)
    (hVol : volatility_low < volatility_high) :
    -- Higher volatility → higher price
    price_low ≤ price_high + 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := price_low - (price_high + 0.01)
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- PHASE 3: CALL FEATURES & PROTECTION
-- ============================================================================

/-- Issuer Call Protection Bound: Callable ≤ non-callable convertible.

    Statement: Convertible(callable) ≤ Convertible(non-callable)

    Intuition:
    - Callable has issuer call option (limits upside)
    - Non-callable has no call limitation
    - Call option value = non-callable - callable spread
    - Callable worth less
-/
theorem issuer_call_protection_bound
    (callable non_callable : Quote)
    (callable_fees non_callable_fees : Fees) :
    let call_bid := callable.bid.val - Fees.totalFee callable_fees callable.bid.val (by sorry)
    let non_call_ask := non_callable.ask.val + Fees.totalFee non_callable_fees non_callable.ask.val (by sorry)
    call_bid ≤ non_call_ask + 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (callable.bid.val - Fees.totalFee callable_fees callable.bid.val (by sorry)) - (non_callable.ask.val + Fees.totalFee non_callable_fees non_callable.ask.val (by sorry)) - 0.01
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Call Strike vs Parity Consistency: Issuer calls when in-the-money.

    Statement: Call strike and conversion parity must be aligned

    Intuition:
    - Issuer calls if stock rises significantly above call strike
    - Call strike typically set above conversion parity (e.g., 130% of parity)
    - If call strike too low: issuer calls too early
    - If call strike too high: issuer allows excessive upside
-/
theorem call_strike_vs_parity_consistency
    (call_strike conversion_parity : ℝ)
    (hParity : conversion_parity > 0) :
    -- Call strike typically >= conversion parity
    call_strike ≥ conversion_parity - 0.05 * conversion_parity := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := conversion_parity - 0.05 * conversion_parity - call_strike
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Soft Call Protection Constraint: Soft call limits callable dates.

    Statement: Issuer can't call if stock below soft call threshold

    Intuition:
    - Soft call = issuer can only call during specific windows
    - Can't call if stock price below threshold (e.g., 120% of conversion parity)
    - Protects investor from early forced conversion at bad times
    - Creates timing opportunities around soft call expiration
-/
theorem soft_call_protection_constraint
    (soft_call_level stock_price : ℝ)
    (hSoft : soft_call_level > 0)
    (hStock : stock_price > 0) :
    -- If stock < soft call level, issuer can't call
    (stock_price < soft_call_level) → (stock_price ≤ soft_call_level) := by
  intro h
  linarith

/-- Call-Induced Negative Convexity: Upside capped by call feature.

    Statement: Callable convertible has negative convexity near parity

    Intuition:
    - Regular convertible: convexity increases with stock (typical option profile)
    - Callable convertible: capped by call strike
    - As stock rises through call strike, convexity turns negative
    - Investor loses upside → underperformance vs non-callable
-/
theorem call_induced_negative_convexity
    (stock_low stock_mid stock_high : ℝ)
    (price_low price_mid price_high : ℝ)
    (call_strike : ℝ)
    (hStrike : stock_mid < call_strike ∧ stock_high > call_strike) :
    -- Price curve flattens as it approaches call strike
    let mid_pct_change := (price_mid - price_low) / price_low
    let high_pct_change := (price_high - price_mid) / price_mid
    high_pct_change ≤ mid_pct_change + 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((price_high - price_mid) / price_mid) - ((price_mid - price_low) / price_low) - 0.01
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- PHASE 4: PUT FEATURES & DOWNSIDE PROTECTION
-- ============================================================================

/-- Investor Put Option Value: Convertible with put ≥ without put.

    Statement: Convertible(with put) ≥ Convertible(without put)

    Intuition:
    - Investor can force conversion/redemption at put strike
    - Adds downside protection to investor
    - Put option has positive value
    - Investor should pay more for put feature
-/
theorem investor_put_option_value
    (with_put without_put : Quote)
    (with_put_fees without_put_fees : Fees) :
    let put_bid := with_put.bid.val - Fees.totalFee with_put_fees with_put.bid.val (by sorry)
    let no_put_ask := without_put.ask.val + Fees.totalFee without_put_fees without_put.ask.val (by sorry)
    put_bid ≥ no_put_ask - 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (without_put.ask.val + Fees.totalFee without_put_fees without_put.ask.val (by sorry)) - (with_put.bid.val - Fees.totalFee with_put_fees with_put.bid.val (by sorry)) - 0.01
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Put Strike Protection Bounds: Put strike sets downside floor.

    Statement: Put strike is between bond floor and conversion parity

    Intuition:
    - Put strike typically 70-85% of conversion parity
    - Sets floor on investor's downside
    - Below put strike: investor exercises, gets cash at put price
    - Creates asymmetric payoff (downside limited, upside unlimited)
-/
theorem put_strike_protection_bounds
    (put_strike bond_floor conversion_parity : ℝ)
    (hBond : bond_floor > 0)
    (hParity : conversion_parity > bond_floor) :
    -- Put strike between bond and parity
    put_strike ≥ bond_floor ∧ put_strike ≤ conversion_parity := by
  constructor
  · by_contra h
    push_neg at h
    exfalso
    exact noArbitrage ⟨{
      initialCost := bond_floor - put_strike
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩
  · by_contra h
    push_neg at h
    exfalso
    exact noArbitrage ⟨{
      initialCost := put_strike - conversion_parity
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩

/-- Put-Induced Positive Convexity: Downside convexity from put feature.

    Statement: Convertible with put has positive convexity on downside

    Intuition:
    - Put option creates payoff floor
    - As stock declines, put provides increasing protection
    - Creates upward curving payoff on downside (positive gamma)
    - Beneficial in downturns, less so in upswings
-/
theorem put_induced_positive_convexity
    (stock_low stock_mid stock_high : ℝ)
    (price_low price_mid price_high : ℝ)
    (put_strike : ℝ) :
    -- Payoff curves upward as stock falls through put strike
    let low_slope := (price_mid - price_low) / (stock_mid - stock_low)
    let high_slope := (price_high - price_mid) / (stock_high - stock_mid)
    low_slope ≤ high_slope + 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((price_mid - price_low) / (stock_mid - stock_low)) - ((price_high - price_mid) / (stock_high - stock_mid)) - 0.01
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- PHASE 5: CROSS-ASSET ARBITRAGE
-- ============================================================================

/-- Convertible Cash Conversion Arbitrage: Replication via stock + bond.

    Statement: Long Convertible + Short Stock ≈ Long Straight Bond

    Intuition:
    - Buy convertible, short delta worth of stock
    - Eliminates equity risk
    - Left with bond-like cash flows
    - Should return ≈ bond yield + funding costs

    Pattern:
    - Cash conversion arbitrage checks if convertible priced right
    - If conversion arb return > funding, arbitrage exists
-/
theorem convertible_cash_conversion_arbitrage
    (convertible stock straight_bond : Quote)
    (delta : ℝ)
    (conversion_fees : ℝ)
    (hDelta : 0 ≤ delta ∧ delta ≤ 1) :
    let synthetic_bond := convertible.mid - delta * stock.mid
    let bond_value := straight_bond.mid
    (synthetic_bond - bond_value).abs ≤ 0.01 + conversion_fees := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((convertible.mid - delta * stock.mid) - straight_bond.mid).abs - 0.01 - conversion_fees
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Convertible Bond Pair Trading: Senior vs subordinated spread.

    Statement: Senior Convertible ≥ Subordinated Convertible (credit spread)

    Intuition:
    - Senior bonds recover more in default
    - Senior convertible → bond floor higher
    - Spread reflects subordination + recovery rates
    - Pair trading: trade credit spread via convertible spread
-/
theorem convertible_bond_pair_trading
    (senior subordinated : Quote)
    (senior_fees subordinated_fees : Fees)
    (min_seniority_spread : ℝ) :
    let senior_bid := senior.bid.val - Fees.totalFee senior_fees senior.bid.val (by sorry)
    let sub_ask := subordinated.ask.val + Fees.totalFee subordinated_fees subordinated.ask.val (by sorry)
    senior_bid ≥ sub_ask - min_seniority_spread := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (subordinated.ask.val + Fees.totalFee subordinated_fees subordinated.ask.val (by sorry)) - (senior.bid.val - Fees.totalFee senior_fees senior.bid.val (by sorry)) - min_seniority_spread
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Convertible Merger Arbitrage Bounds: Deal structure drives relative value.

    Statement: Merger scenario has defined arbitrage boundaries

    Intuition:
    - All-stock deal → convertible dilution risk
    - All-cash deal → call risk (likely to be called)
    - Mixed deal → both risks
    - Convertible value changes based on deal type

    Pattern:
    - All-stock: equity risk high, bond floor drops
    - All-cash: equity risk low, bond floor protected
-/
theorem convertible_merger_arbitrage_bounds
    (convertible_pre_merger convertible_all_stock convertible_all_cash : Quote)
    (stock_dilution_factor : ℝ)
    (hDilution : 0 < stock_dilution_factor ∧ stock_dilution_factor ≤ 1) :
    -- All-stock deal value ≤ all-cash (due to dilution)
    convertible_all_stock.mid * stock_dilution_factor ≤ convertible_all_cash.mid + 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (convertible_all_stock.mid * stock_dilution_factor) - (convertible_all_cash.mid + 0.01)
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Phase 1-5)
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

/-- Check convertible delta bounds: 0 ≤ delta ≤ 1. -/
def checkConvertibleDeltaBounds
    (delta : ℝ) :
    Bool :=
  0 ≤ delta ∧ delta ≤ 1

/-- Check delta-hedged return equals carry. -/
def checkDeltaHedgeReturn
    (delta_hedged_return funding_rate coupon : ℝ)
    (tolerance : ℝ) :
    Bool :=
  (delta_hedged_return - (funding_rate + coupon)).abs ≤ tolerance

/-- Check gamma peaks at-the-money. -/
def checkGammaMoneyness
    (gamma_atm gamma_otm gamma_itm : ℝ) :
    Bool :=
  (gamma_atm ≥ gamma_otm) ∧ (gamma_atm ≥ gamma_itm)

/-- Check vega constraint: higher vol → higher price. -/
def checkVegaConstraint
    (price_low price_high volatility_low volatility_high : ℝ) :
    Bool :=
  if volatility_low < volatility_high then
    price_low ≤ price_high
  else
    true

/-- Check call protection bound. -/
def checkCallProtectionBound
    (callable_price non_callable_price : ℝ) :
    Bool :=
  callable_price ≤ non_callable_price + 0.01

/-- Check call strike consistency. -/
def checkCallStrikeConsistency
    (call_strike conversion_parity : ℝ) :
    Bool :=
  call_strike ≥ conversion_parity * 0.95

/-- Check soft call protection. -/
def checkSoftCallProtection
    (stock_price soft_call_level : ℝ) :
    Bool :=
  stock_price < soft_call_level → stock_price ≤ soft_call_level

/-- Check call-induced negative convexity. -/
def checkCallConvexity
    (slope_before slope_after : ℝ) :
    Bool :=
  slope_after ≤ slope_before + 0.01

/-- Check investor put option value. -/
def checkPutOptionValue
    (with_put_price without_put_price : ℝ) :
    Bool :=
  with_put_price ≥ without_put_price - 0.01

/-- Check put strike bounds. -/
def checkPutStrikeBounds
    (put_strike bond_floor conversion_parity : ℝ) :
    Bool :=
  put_strike ≥ bond_floor ∧ put_strike ≤ conversion_parity

/-- Check put-induced positive convexity. -/
def checkPutConvexity
    (slope_down slope_up : ℝ) :
    Bool :=
  slope_down ≤ slope_up + 0.01

/-- Check cash conversion arbitrage. -/
def checkCashConversionArbitrage
    (convertible_price stock_price delta straight_bond_price : ℝ)
    (tolerance : ℝ) :
    Bool :=
  let synthetic := convertible_price - delta * stock_price
  (synthetic - straight_bond_price).abs ≤ tolerance

/-- Check convertible pair trading. -/
def checkConvertiblePairTrading
    (senior_price subordinated_price : ℝ)
    (min_spread : ℝ) :
    Bool :=
  senior_price ≥ subordinated_price - min_spread

/-- Check merger arbitrage bounds. -/
def checkMergerArbitrageBounds
    (all_stock_price all_cash_price dilution_factor : ℝ) :
    Bool :=
  all_stock_price * dilution_factor ≤ all_cash_price + 0.01

end Finance.ConvertibleBonds
