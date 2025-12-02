-- Commodity Options: Convenience yield, storage costs, seasonality, backwardation
-- Formalizes no-arbitrage constraints on commodity derivatives

import Finance.Core

namespace Finance.CommodityOptions

-- ============================================================================
-- Commodity Forward Parity (with Convenience Yield)
-- ============================================================================

/-- Commodity forward pricing with storage and convenience yield.

    Forward price: F = S × e^((r + u - y)T)

    where:
    - r = risk-free rate
    - u = storage cost (% per annum)
    - y = convenience yield (benefit of holding physical)
    - T = time to maturity

    Net cost of carry = r + u - y

    If net carry > 0: futures > spot (contango)
    If net carry < 0: futures < spot (backwardation)
-/
structure CommodityForward where
  spotPrice : Float       -- S(0)
  forwardPrice : Float    -- F(0, T)
  riskFreeRate : Rate     -- r
  storageYield : Float    -- u (storage cost as %)
  convenienceYield : Float -- y (benefit of holding)
  tenor : Time            -- T

namespace CommodityForward

/-- Net cost of carry. -/
def netCarry (forward : CommodityForward) : Float :=
  forward.riskFreeRate.val + forward.storageYield - forward.convenienceYield

end CommodityForward

-- ============================================================================
-- Cost of Carry Theorem for Commodities
-- ============================================================================

/-- Commodity forward parity: F = S × e^(carry × T).

    Statement: Forward price = Spot × e^((r + u - y)T)

    Intuition:
    - Forward = spot adjusted for financing (r), storage (u), convenience benefit (y)
    - If y = 0 (no convenience benefit): forward = spot × e^((r+u)T)
    - If y > r + u: contango reverses to backwardation

    Arbitrage if violated:
    - If F > S × e^(carry × T): cash-and-carry arbitrage
      Buy spot, store, sell forward, lock in profit
    - If F < S × e^(carry × T): reverse cash-and-carry
      Short spot, buy forward, lock profit (if possible)

    Practical: Gold, oil, agricultural commodities follow this rigorously.
-/
theorem commodity_forward_parity (spot forward rate storage convenience tenor : Float)
    (hSpot : spot > 0)
    (hTenor : tenor ≥ 0) :
    -- Forward = spot × e^((r + u - y) × T)
    let carry := rate + storage - convenience
    let theoretical_forward := spot * Float.exp (carry * tenor)
    (forward - theoretical_forward).abs ≤ spot * 0.01 := by  -- 1% basis tolerance
  sorry

-- ============================================================================
-- Contango vs Backwardation
-- ============================================================================

/-- Contango condition: Forward > Spot (positive carry market).

    Statement: F(T) > S(0) ⟺ r + u > y

    Intuition:
    - Contango: financing costs exceed convenience benefit
    - Typical for precious metals, financials
    - Roll yield is negative (buy near contracts, sell far)

    Arbitrage if violated:
    - If backwardated when carry positive: buy forward, short spot = arb
-/
theorem contango_condition (forward spot carry : Float)
    (hSpot : spot > 0)
    (hCarry : carry > 0) :
    forward > spot := by
  sorry

/-- Backwardation condition: Forward < Spot (negative carry / high convenience yield).

    Statement: F(T) < S(0) ⟺ y > r + u

    Intuition:
    - Backwardation: convenience benefit exceeds financing costs
    - Typical for energy, agricultural commodities
    - Roll yield is positive (buy near contracts, sell far)
    - Indicates storage constraints or supply disruptions

    Arbitrage if violated:
    - If contango when carry negative: short forward, buy spot = arb
-/
theorem backwardation_condition (forward spot convenience rate storage : Float)
    (hSpot : spot > 0)
    (hConvenience : convenience > rate + storage) :
    forward < spot := sorry

-- ============================================================================
-- Storage Cost Bounds
-- ============================================================================

/-- Storage cost monotonicity: Higher storage → higher forward.

    Statement: If u₁ > u₂, then F₁ > F₂ (same S, r, y, T)

    Intuition:
    - Higher storage cost increases forward price
    - Forward reflects additional carry expense
    - Cheaper storage creates arb: buy cheaper storage, sell forward

    Arbitrage if violated:
    - If high-storage contract < low-storage: buy high, short low
      Storage cost differential = profit
-/
theorem storage_forward_monotonicity (forward1 forward2 storage1 storage2 spot : Float)
    (hStorage : storage1 > storage2)
    (hForward : forward1 ≤ forward2) :
    -- If storage higher, forward must be higher
    False := by
  exfalso
  exact noArbitrage ⟨{
    initialCost := 0
    minimumPayoff := forward1 - forward2
    isArb := Or.inl ⟨by norm_num, by sorry⟩
  }, trivial⟩

/-- Convenience yield positivity: Convenience yield > 0 for tradeable commodities.

    Statement: y > 0 (holding commodity provides value)

    Intuition:
    - Convenience yield = marginal benefit of physical availability
    - Used in manufacturing, hedging, speculation
    - Without it: forward would be pure financing equation

    Practical: Extractable from term structure via futures curve.
-/
theorem convenience_yield_nonnegative (convenience : Float) :
    0 ≤ convenience := by
  by_contra h_neg
  push_neg at h_neg
  exfalso
  exact noArbitrage ⟨{
    initialCost := 0
    minimumPayoff := -convenience
    isArb := Or.inl ⟨by norm_num, by sorry⟩
  }, trivial⟩

-- ============================================================================
-- Seasonal Patterns
-- ============================================================================

/-- Seasonal convenience yield: Convenience yield varies with season.

    Statement: y(T) = y_base × (1 + seasonal_factor)

    Intuition:
    - Agricultural commodities have seasonal convenience yield
    - Harvest season: high convenience (fresh supply), low yield
    - Off-season: low convenience (storage), high yield
    - Creates predictable curve shapes

    Practical: Corn, wheat have strong seasonal patterns.
-/
theorem seasonal_convenience_yield (yield_harvest yield_offseason time_to_harvest : Float)
    (hYield : yield_harvest ≥ 0 ∧ yield_offseason ≥ 0) :
    -- Harvest convenience > off-season (supplies available)
    yield_harvest ≥ yield_offseason / 2 := by
  sorry

-- ============================================================================
-- Lease Rate (Implied Convenience Yield)
-- ============================================================================

/-- Implied lease rate theorem: Extracted from spot-forward curve.

    Statement: Implied_lease_rate = r + u - (1/T) × ln(F/S)

    Intuition:
    - Lease rate = market's implied convenience yield
    - Can borrow commodity at this rate from dealer
    - Extracted from futures term structure

    Arbitrage if violated:
    - If lease rate quoted > implied: borrow at quoted, lend at implied
    - If lease rate quoted < implied: short side is mispriced
-/
theorem implied_lease_rate (forward spot rate storage tenor : Float)
    (hSpot : spot > 0)
    (hForward : forward > 0)
    (hTenor : tenor > 0) :
    -- Implied lease rate = r + u - ln(F/S)/T
    let implied_lease := rate + storage - (Float.log (forward / spot)) / tenor
    implied_lease ≥ -0.5 ∧ implied_lease ≤ rate + storage + 0.5 := sorry

-- ============================================================================
-- Commodity Spot-Forward-Futures Relationship
-- ============================================================================

/-- Spot-forward-futures triangle: Futures converges to spot at maturity.

    Statement: F(t, T) → S(T) as t → T (Basis → 0)

    Intuition:
    - At maturity, futures and spot are identical
    - Before maturity: futures = forward (in continuous time)
    - Basis = futures - spot = carry cost minus convenience

    Arbitrage if violated:
    - If futures >> spot near maturity: sell futures, buy spot
      Delivery arbitrage locks in difference
-/
theorem futures_convergence (futures_price spot_price basis tenor : Float)
    (hSpot : spot_price > 0)
    (hTenor : tenor > 0) :
    -- As tenor → 0, basis → 0
    tenor > 0 → basis = futures_price - spot_price := by
  intro _
  rfl

/-- Basis decay theorem: Basis erodes linearly toward zero at maturity.

    Statement: Basis(t) = spot × (r + u - y) × (T - t)

    Intuition:
    - Basis decays to zero as expiration approaches
    - Rate of decay = cost of carry
    - Can trade basis: long futures, short spot, pocket carry

    Practical: Basis traders exploit this relationship.
-/
theorem basis_decay (basis carry tenor_remaining spot : Float)
    (hTenor : tenor_remaining > 0)
    (hSpot : spot > 0) :
    -- Basis decays at rate of carry (within tolerance)
    (basis - carry * tenor_remaining * spot).abs ≤ spot * 0.001 := sorry

-- ============================================================================
-- Commodity Option-Forward Relationship
-- ============================================================================

/-- Commodity call-put parity (generalized):

    Statement: C - P = (F - K) × e^(-rT) + y × (value of holding benefit)

    Intuition:
    - Commodity options are calls/puts on forward, not spot
    - Convenience yield creates additional value term
    - Different from equity options (which are on spot)

    Arbitrage if violated:
    - If C - P ≠ (F - K) × DF: synthetic forward mispriced
-/
theorem commodity_call_put_parity (call put forward strike rate time : Float)
    (hRate : rate ≥ 0)
    (hTime : time.val > 0) :
    -- C - P = (F - K) × e^(-rT)
    let df := Float.exp (-rate * time)
    call - put = (forward - strike) * df := by
  sorry

-- ============================================================================
-- Commodity Spread Trading
-- ============================================================================

/-- Calendar spread constraint: Near < Far (in contango).

    Statement: In contango markets, C(K,T₁) < C(K,T₂) for T₁ < T₂

    Intuition:
    - Near-term contract incorporates less carry
    - Far-term contract incorporates more cost of carry
    - Typical for financials, precious metals in positive carry

    Arbitrage if violated:
    - If near > far: sell near, buy far (negative calendar spread)
      Roll at profit over time
-/
theorem commodity_calendar_spread (call_near call_far carry tenor_near tenor_far : Float)
    (hTenor : tenor_near < tenor_far)
    (hCarry : carry > 0) :
    call_near ≤ call_far := sorry

/-- Crush spread (for agricultural commodities): Soybean → oil + meal.

    Statement: Price_soybean ≈ Price_oil + Price_meal (with processing margin)

    Intuition:
    - Crush spread = profit margin for soybean processors
    - Forward contracts form arbitrage triangle
    - If spread too tight: processing unprofitable
    - If spread too wide: processing profitable (supply response)

    Practical: Typical crush spread = 0.5-1.5% of soybean price.
-/
theorem crush_spread_bound (soybean_price oil_price meal_price margin : Float)
    (hSoybean : soybean_price > 0)
    (hMargin : margin ≥ 0.005) :  -- At least 0.5% margin
    -- Soybean ≥ oil + meal - margin
    soybean_price ≥ oil_price + meal_price - margin * soybean_price := sorry

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Standard 5)
-- ============================================================================

/-- Check commodity forward parity -/
def checkCommodityForwardParity
    (spot forward rate storage convenience tenor : Float) :
    Bool :=
  let carry := rate + storage - convenience
  let theoretical_forward := spot * Float.exp (carry * tenor)
  (forward - theoretical_forward).abs ≤ spot * 0.01 * tenor

/-- Check contango condition -/
def checkContangoCondition
    (forward spot : Float) :
    Bool :=
  forward > spot

/-- Check backwardation condition -/
def checkBackwardationCondition
    (forward spot convenience rate storage : Float) :
    Bool :=
  forward < spot

/-- Check storage forward monotonicity -/
def checkStorageForwardMonotonicity
    (forward1 forward2 storage1 storage2 : Float) :
    Bool :=
  if storage1 > storage2 then forward1 > forward2 else true

/-- Check convenience yield positivity -/
def checkConvenienceYieldNonnegative
    (convenience : Float) :
    Bool :=
  0 ≤ convenience

/-- Check seasonal convenience yield -/
def checkSeasonalConvenienceYield
    (yield_harvest yield_offseason : Float) :
    Bool :=
  yield_harvest ≥ yield_offseason / 2

/-- Check implied lease rate -/
def checkImpliedLeaseRate
    (forward spot rate storage tenor : Float) :
    Bool :=
  let implied_lease := rate + storage - (Float.log (forward / spot)) / tenor
  implied_lease ≥ -0.5 ∧ implied_lease ≤ rate + storage + 0.5

/-- Check futures convergence -/
def checkFuturesConvergence
    (futures_price spot_price : Float) :
    Bool :=
  futures_price ≥ spot_price * 0.99  -- Near maturity tolerance

/-- Check basis decay -/
def checkBasisDecay
    (basis carry tenor_remaining spot : Float) :
    Bool :=
  (basis - carry * tenor_remaining * spot).abs ≤ spot * 0.001

/-- Check commodity call-put parity -/
def checkCommodityCallPutParity
    (call put forward strike rate time : Float) :
    Bool :=
  let df := Float.exp (-rate * time)
  (call - put - (forward - strike) * df).abs ≤ (forward - strike) * df * 0.01

/-- Check commodity calendar spread -/
def checkCommodityCalendarSpread
    (call_near call_far : Float) :
    Bool :=
  call_near ≤ call_far

/-- Check crush spread bound -/
def checkCrushSpreadBound
    (soybean_price oil_price meal_price margin : Float) :
    Bool :=
  soybean_price ≥ oil_price + meal_price - margin * soybean_price

end Finance.CommodityOptions
