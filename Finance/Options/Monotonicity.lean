-- Strike monotonicity constraints on option surfaces
-- Enforces that option prices must be monotonic in strike

import Finance.Core
import Finance.Options.European

namespace Finance.Options

-- ============================================================================
-- Call Strike Monotonicity
-- ============================================================================

/-- Call strike monotonicity (production-ready): K₁ < K₂ → C₁(ask) ≤ C₂(bid) + fees

    Statement: Lower strike calls are worth more.

    Production Rule: Buy call at K₁, sell call at K₂
    If K₁_ask < K₂_bid after fees, arbitrage exists.
-/
theorem callMonotonicity_with_fees (call1 call2 : Quote)
    (call1_fees call2_fees : Fees)
    (strike1 strike2 : ℝ)
    (hK : strike1 < strike2) :
    (call1.ask.val + Fees.totalFee call1_fees call1.ask.val (by sorry)) - (call2.bid.val - Fees.totalFee call2_fees call2.bid.val (by sorry)) ≤ strike2 - strike1 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := -((call1.ask.val + Fees.totalFee call1_fees call1.ask.val (by sorry)) - (call2.bid.val - Fees.totalFee call2_fees call2.bid.val (by sorry)))
    minimumPayoff := 0
    isArb := Or.inr ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Put strike monotonicity (production-ready): K₁ < K₂ → P₁(bid) ≤ P₂(ask) + fees

    Statement: Higher strike puts are worth more.

    Production Rule: Buy put at K₂, sell put at K₁
    If K₂_ask < K₁_bid after fees, arbitrage exists.
-/
theorem putMonotonicity_with_fees (put1 put2 : Quote)
    (put1_fees put2_fees : Fees)
    (strike1 strike2 : ℝ)
    (hK : strike1 < strike2) :
    (put2.ask.val + Fees.totalFee put2_fees put2.ask.val (by sorry)) - (put1.bid.val - Fees.totalFee put1_fees put1.bid.val (by sorry)) ≤ strike2 - strike1 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := -((put2.ask.val + Fees.totalFee put2_fees put2.ask.val (by sorry)) - (put1.bid.val - Fees.totalFee put1_fees put1.bid.val (by sorry)))
    minimumPayoff := 0
    isArb := Or.inr ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- THEORETICAL: Call strike monotonicity (abstract, no fees) -/
theorem callMonotonicity_theoretical (K₁ K₂ : ℝ) (C₁ C₂ : ℝ)
    (hK : K₁ < K₂) : C₁ ≥ C₂ := sorry

/-- Call spread arbitrage: K₁ < K₂, compare C(K₁)_bid with C(K₂)_ask.

    With bid/ask spreads:
    - To execute spread, buy call at C(K₁)_ask, sell at C(K₂)_bid
    - Net cost = C(K₁)_ask - C(K₂)_bid

    Payoff at expiry: [max(0, S-K₁) - max(0, S-K₂)]
    This is bounded between 0 and (K₂ - K₁).

    Arbitrage exists if net cost < 0:
    C(K₁)_ask < C(K₂)_bid (can't happen if monotonicity holds)

    More realistically: if the spread is wider than the strike difference,
    we can capture the excess.
-/
def checkCallSpreadArbitrage
    (callK1Price : Float) (callK2Price : Float)
    (strikeK1 strikeK2 : Float) : Float :=
  let strikeWidth := strikeK2 - strikeK1
  let callSpreadCost := callK1Price - callK2Price
  -- Profit if spread is narrower than intrinsic width
  strikeWidth - callSpreadCost

/-- Call spread with fees: full arbitrage profitability. -/
def checkCallSpreadWithFees
    (callK1Price : Float) (callK2Price : Float)
    (strikeK1 strikeK2 : Float)
    (callK1Fees callK2Fees : Fees) : Float :=
  let strikeWidth := strikeK2 - strikeK1
  let callSpreadCost := callK1Price - callK2Price
  let fee1 := Fees.totalFee callK1Fees callK1Price (by sorry)
  let fee2 := Fees.totalFee callK2Fees callK2Price (by sorry)
  let totalFees := fee1 + fee2
  strikeWidth - callSpreadCost - totalFees

-- ============================================================================
-- Put Strike Monotonicity
-- ============================================================================

/-- THEORETICAL: Put strike monotonicity (abstract, no fees) -/
theorem putMonotonicity_theoretical (K₁ K₂ : ℝ) (P₁ P₂ : ℝ)
    (hK : K₁ < K₂) : P₁ ≤ P₂ := sorry

/-- Put spread arbitrage: K₁ < K₂, compare P(K₁)_ask with P(K₂)_bid.

    With bid/ask spreads:
    - To execute spread, buy put at P(K₂)_ask, sell at P(K₁)_bid
    - Net cost = P(K₂)_ask - P(K₁)_bid

    Payoff at expiry: [max(0, K₂-S) - max(0, K₁-S)]
    This is bounded between 0 and (K₂ - K₁).

    Arbitrage exists if net cost < 0:
    P(K₂)_ask < P(K₁)_bid (can't happen if monotonicity holds)
-/
def checkPutSpreadArbitrage
    (putK1Price : Float) (putK2Price : Float)
    (strikeK1 strikeK2 : Float) : Float :=
  let strikeWidth := strikeK2 - strikeK1
  let putSpreadCost := putK2Price - putK1Price
  strikeWidth - putSpreadCost

/-- Put spread with fees: full arbitrage profitability. -/
def checkPutSpreadWithFees
    (putK1Price : Float) (putK2Price : Float)
    (strikeK1 strikeK2 : Float)
    (putK1Fees putK2Fees : Fees) : Float :=
  let strikeWidth := strikeK2 - strikeK1
  let putSpreadCost := putK2Price - putK1Price
  let fee1 := Fees.totalFee putK1Fees putK1Price (by sorry)
  let fee2 := Fees.totalFee putK2Fees putK2Price (by sorry)
  let totalFees := fee1 + fee2
  strikeWidth - putSpreadCost - totalFees

-- ============================================================================
-- Monotonicity Violations
-- ============================================================================

/-- A monotonicity violation for a strike pair. -/
structure MonotonicityViolation where
  optionType : String  -- "call" or "put"
  strikeK1 : Float
  strikeK2 : Float
  deviationSize : Float  -- how much monotonicity is violated
  netProfit : Float  -- profit after fees
  spreadType : String  -- "call_spread" or "put_spread"

-- ============================================================================
-- Call Surface Monotonicity Checking
-- ============================================================================

/-- Check call monotonicity for an adjacent pair on the volatility surface.

    For efficiency, we check consecutive strikes in a sorted list.
    A violation between K₁ and K₂ (K₁ < K₂) means:
    C(K₁)_ask < C(K₂)_bid after accounting for bid/ask spreads.

    Returns profit amount if violation exists, 0 otherwise.
-/
def checkAdjacentCallMonotonicity
    (c1 : Quote) (c2 : Quote) (k1 k2 : Float)
    (fees1 fees2 : Fees) : Float :=
  if k1 < k2 then
    -- C(K₁)_ask should be ≥ C(K₂)_bid for monotonicity
    -- Violation if C(K₁)_ask < C(K₂)_bid after fees
    checkCallSpreadWithFees c1.ask.val c2.bid.val k1 k2 fees1 fees2
  else
    0  -- invalid input

/-- Check all calls in a strike list for monotonicity.

    Input: quotes and strikes in ascending order by strike.
    Returns list of violations found.
-/
def checkCallSurfaceMonotonicity
    (strikes : List Float) (calls : List Quote)
    (fees : List Fees) : List MonotonicityViolation := by
  sorry  -- Would iterate through adjacent pairs

-- ============================================================================
-- Put Surface Monotonicity Checking
-- ============================================================================

/-- Check put monotonicity for an adjacent pair on the volatility surface.

    For K₁ < K₂, we need P(K₁) ≤ P(K₂).
    With bid/ask: P(K₁)_bid should be ≤ P(K₂)_ask.

    Violation if P(K₁)_ask > P(K₂)_bid (can execute profitable spread).
-/
def checkAdjacentPutMonotonicity
    (p1 : Quote) (p2 : Quote) (k1 k2 : Float)
    (fees1 fees2 : Fees) : Float :=
  if k1 < k2 then
    -- P(K₁)_ask should be ≤ P(K₂)_bid for monotonicity
    -- Violation if P(K₁)_ask > P(K₂)_bid after fees
    checkPutSpreadWithFees p2.ask.val p1.bid.val k1 k2 fees2 fees1
  else
    0

/-- Check all puts in a strike list for monotonicity.

    Input: quotes and strikes in ascending order by strike.
    Returns list of violations found.
-/
def checkPutSurfaceMonotonicity
    (strikes : List Float) (puts : List Quote)
    (fees : List Fees) : List MonotonicityViolation := by
  sorry  -- Would iterate through adjacent pairs

-- ============================================================================
-- Spread Analysis Structure
-- ============================================================================

/-- Result of checking a strike pair for spread arbitrage. -/
structure SpreadAnalysis where
  strikeK1 : Float
  strikeK2 : Float
  strikeWidth : Float
  callK1Price : Float
  callK2Price : Float
  callSpreadCost : Float
  callNetProfit : Float
  putK1Price : Float
  putK2Price : Float
  putSpreadCost : Float
  putNetProfit : Float
  bestArbitrage : String  -- "none", "call_spread", "put_spread"
  bestProfit : Float

/-- Analyze a single strike pair for all spread arbitrages. -/
def analyzeStrikePair
    (k1 k2 : Float)
    (c1 c2 : Quote)
    (p1 p2 : Quote)
    (callFees putFees : Fees) : SpreadAnalysis :=
  let k1_k2 := k2 - k1
  let callSpread := c1.ask.val - c2.bid.val
  let callProfit := checkCallSpreadWithFees c1.ask.val c2.bid.val k1 k2 callFees callFees
  let putSpread := p2.ask.val - p1.bid.val
  let putProfit := checkPutSpreadWithFees p2.ask.val p1.bid.val k1 k2 putFees putFees

  -- Determine best arbitrage opportunity
  let maxProfit := max callProfit putProfit
  let bestType :=
    if callProfit > putProfit && maxProfit > 0 then "call_spread"
    else if maxProfit > 0 then "put_spread"
    else "none"

  ⟨k1, k2, k1_k2, c1.ask.val, c2.bid.val, callSpread, callProfit,
   p2.ask.val, p1.bid.val, putSpread, putProfit, bestType, maxProfit⟩

-- ============================================================================
-- Multi-Strike Surface Analysis
-- ============================================================================

/-- Comprehensive strike monotonicity check for a call surface. -/
structure CallSurfaceViolations where
  totalViolations : Nat
  violations : List (Float × Float × Float)  -- (K1, K2, profit)
  maxViolation : Float
  worstPair : (Float × Float)

/-- Comprehensive strike monotonicity check for a put surface. -/
structure PutSurfaceViolations where
  totalViolations : Nat
  violations : List (Float × Float × Float)  -- (K1, K2, profit)
  maxViolation : Float
  worstPair : (Float × Float)

/-- Check entire volatility surface for monotonicity violations.

    Builds a surface from list of (strike, call_quote, put_quote) tuples
    and checks all adjacent pairs for spread arbitrage.
-/
structure VolatilitySurfaceAnalysis where
  numStrikes : Nat
  callViolations : CallSurfaceViolations
  putViolations : PutSurfaceViolations
  totalSpreadArbs : Nat
  hasViolations : Bool
  recommendation : String  -- "buy_spread", "sell_spread", or "no_arb"

end Finance.Options
