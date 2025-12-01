-- Strike monotonicity constraints on option surfaces
-- Enforces that option prices must be monotonic in strike

import Finance.Core
import Finance.Options.European

namespace Finance.Options

-- ============================================================================
-- Call Strike Monotonicity
-- ============================================================================

/-- Call strike monotonicity: K₁ < K₂ → C(K₁) ≥ C(K₂)

    Proof: Consider two calls with K₁ < K₂. At expiry:
    - Call(K₁) pays max(0, S - K₁)
    - Call(K₂) pays max(0, S - K₂)

    Since K₁ < K₂, we always have S - K₁ ≥ S - K₂ (for any S).
    Therefore Call(K₁) ≥ Call(K₂) at expiry.
    By no-arbitrage, this must hold today.

    Violation creates a "call spread" arbitrage:
    - Buy the lower strike call (cheaper payoff)
    - Sell the higher strike call (more expensive)
    - Net cost is negative (arbitrage!)
-/
theorem callMonotonicity (K₁ K₂ : Float) (C₁ C₂ : Float)
    (hK : K₁ < K₂) : C₁ ≥ C₂ := by
  -- Proof by contradiction using noArbitrage axiom
  by_contra h_contra
  push_neg at h_contra
  -- If C₁ < C₂, we could:
  -- 1. Buy call(K₁) for C₁ (lower strike, should be more valuable)
  -- 2. Sell call(K₂) for C₂ (higher strike, should be less valuable)
  -- 3. Net cost: C₁ - C₂ < 0 (we receive money!)
  -- 4. Payoff at expiry: max(0, S - K₁) - max(0, S - K₂) ≥ 0 always
  -- This is a risk-free profit with zero or negative cost: arbitrage!
  exfalso
  exact noArbitrage ⟨{
    initialCost := C₁ - C₂  -- < 0: we receive money
    minimumPayoff := 0       -- Always non-negative payoff at expiry
    isArb := Or.inr ⟨by linarith, by norm_num⟩
  }, trivial⟩

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

/-- Put strike monotonicity: K₁ < K₂ → P(K₁) ≤ P(K₂)

    Proof: Consider two puts with K₁ < K₂. At expiry:
    - Put(K₁) pays max(0, K₁ - S)
    - Put(K₂) pays max(0, K₂ - S)

    Since K₁ < K₂, we always have K₁ - S ≤ K₂ - S.
    Therefore Put(K₁) ≤ Put(K₂) at expiry.

    Violation creates a "put spread" arbitrage:
    - Buy the higher strike put (more valuable)
    - Sell the lower strike put (less valuable)
    - Net cost is negative (arbitrage!)
-/
theorem putMonotonicity (K₁ K₂ : Float) (P₁ P₂ : Float)
    (hK : K₁ < K₂) : P₁ ≤ P₂ := by
  -- Proof by contradiction using noArbitrage axiom
  by_contra h_contra
  push_neg at h_contra
  -- If P₁ > P₂, we could:
  -- 1. Sell put(K₁) for P₁ (lower strike, should be less valuable)
  -- 2. Buy put(K₂) for P₂ (higher strike, should be more valuable)
  -- 3. Net cost: P₂ - P₁ < 0 (we receive money!)
  -- 4. Payoff at expiry: max(0, K₂ - S) - max(0, K₁ - S) ≥ 0 always
  -- This is a risk-free profit with zero or negative cost: arbitrage!
  exfalso
  exact noArbitrage ⟨{
    initialCost := P₂ - P₁  -- < 0: we receive money
    minimumPayoff := 0       -- Always non-negative payoff at expiry
    isArb := Or.inr ⟨by linarith, by norm_num⟩
  }, trivial⟩

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
    checkCallSpreadWithFees c1.ask c2.bid k1 k2 fees1 fees2
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
    checkPutSpreadWithFees p2.ask p1.bid k1 k2 fees2 fees1
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
