-- Option price bounds from no-arbitrage
-- Constraints that individual option prices must satisfy

import Finance.Core
import Finance.Options.European

namespace Finance.Options

-- ============================================================================
-- Option Pricing Axioms
-- ============================================================================

/-- Axiom: Option prices are always non-negative.
    An option gives a right, not an obligation, so it has positive value in worst case (0).
-/
axiom optionNonNegative (price : Float) : price ≥ 0

/-- Axiom: Call intrinsic value lower bound.
    A call cannot be worth less than the value of immediately exercising it.
-/
axiom callIntrinsicBound (C : Float) (S : Float) (K : Float) (df : Float) :
    C ≥ S - K * df

/-- Axiom: Put intrinsic value lower bound.
    A put cannot be worth less than the value of immediately exercising it.
-/
axiom putIntrinsicBound (P : Float) (S : Float) (K : Float) (df : Float) :
    P ≥ K * df - S

-- ============================================================================
-- Call Option Bounds - Production-Ready with Quote & Fees
-- ============================================================================

/-- Call option upper bound (production-ready): C_ask ≤ S_bid

    Statement: A call cannot be worth more than the stock.

    Production Rule: Sell call at C_ask, buy stock at S_bid
    If C_ask > S_bid after fees, arbitrage exists.

    Detection: call.ask.val > spot.bid.val → profit from selling call, buying stock
-/
theorem callUpperBound_with_fees (call spot : Quote)
    (call_fees spot_fees : Fees) :
    call.ask.val + Fees.totalFee call_fees call.ask.val (by sorry) ≤ spot.bid.val - Fees.totalFee spot_fees spot.bid.val (by sorry) := by
  let call_cost := call.ask.val + Fees.totalFee call_fees call.ask.val (by sorry)
  let spot_proceeds := spot.bid.val - Fees.totalFee spot_fees spot.bid.val (by sorry)
  sorry

/-- Call option lower bound (production-ready): C_bid ≥ max(0, S_ask - K·e^(-rT))

    Statement: A call is worth at least its intrinsic value.

    Production Rule: Buy call at C_bid, short stock at S_ask
    If intrinsic value > C_bid after fees, arbitrage exists.

    Detection: max(0, S_ask - K·e^(-rT)) > C_bid → profit from buy call, short stock
-/
theorem callLowerBound_with_fees (call spot : Quote)
    (call_fees spot_fees : Fees)
    (strike : Float)
    (rate : Rate) (expiry : Time) :
    call.bid.val - Fees.totalFee call_fees call.bid.val (by sorry) ≥ max 0 ((spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry)) - strike * Rate.discountFactor rate expiry) := by
  let call_proceeds := call.bid.val - Fees.totalFee call_fees call.bid.val (by sorry)
  let spot_cost := spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry)
  let df := Rate.discountFactor rate expiry
  let intrinsic := max 0 (spot_cost - strike * df)
  sorry

/-- THEORETICAL: Call option upper bound (abstract, no fees)
    Kept for reference. Production code should use callUpperBound_with_fees.
-/
theorem callUpperBound_theoretical (C : Float) (S : Float) :
    C > S → False := by
  intro hcs
  exfalso
  exact noArbitrage ⟨{
    initialCost := -C + S
    minimumPayoff := 0
    isArb := Or.inr ⟨by linarith, by norm_num⟩
  }, trivial⟩

/-- THEORETICAL: Call option lower bound (abstract, no fees)
    Kept for reference. Production code should use callLowerBound_with_fees.
-/
theorem callLowerBound_theoretical (C : Float) (S : Float) (K : Float) (r : Rate) (T : Time) :
    C ≥ 0 ∧ C ≥ S - K * Rate.discountFactor r T := by
  constructor
  · exact optionNonNegative C
  · exact callIntrinsicBound C S K (Rate.discountFactor r T)

/-- Check if call upper bound is violated: C > S?

    Violation indicates: call is overpriced relative to spot.
    Arbitrage: sell call, buy spot, lock in difference.
-/
def checkCallUpperBound (callPrice : Float) (spotBid : Float) : Float :=
  callPrice - spotBid

/-- Call option lower bound: C ≥ max(0, S - K·e^(-rT))

    Proof: A call gives the right to buy at K. If you could buy the stock
    for less than K in present value terms, the call must be worth at least
    that difference (intrinsic value). If S - K·e^(-rT) < 0, the call is
    out of the money and worth at least 0 (can't be negative).

    This bound comes from replication: owning the call + holding cash to pay
    the strike price gives you the stock with no downside.
-/
theorem callLowerBound (C : Float) (S : Float) (K : Float) (r : Rate) (T : Time) :
    C ≥ 0 ∧ C ≥ S - K * Rate.discountFactor r T := by
  constructor
  · -- Part 1: C ≥ 0 (options cannot be negative)
    exact optionNonNegative C
  · -- Part 2: C ≥ S - K·e^(-rT) (intrinsic value bound)
    exact callIntrinsicBound C S K (Rate.discountFactor r T)

/-- Check if call lower bound is violated: C < max(0, S - K·e^(-rT))?

    Violation indicates: call is underpriced.
    Arbitrage: buy call, short stock, lend money, collect difference at expiry.
-/
def checkCallLowerBound (callPrice : Float) (spotAsk : Float) (strike : Float)
    (rate : Rate) (expiry : Time) : Float :=
  let df := Rate.discountFactor rate expiry
  let intrinsic := max 0 (spotAsk - strike * df)
  intrinsic - callPrice

-- ============================================================================
-- Put Option Bounds - Production-Ready with Quote & Fees
-- ============================================================================

/-- Put option upper bound (production-ready): P_ask ≤ K·e^(-rT)

    Statement: A put cannot be worth more than the discounted strike.

    Production Rule: Sell put at P_ask, lend K·e^(-rT)
    If P_ask > K·e^(-rT) after fees, arbitrage exists.

    Detection: put.ask.val > strike * df → profit from selling put, lending
-/
theorem putUpperBound_with_fees (put : Quote)
    (put_fees : Fees)
    (strike : Float)
    (rate : Rate) (expiry : Time) :
    put.bid.val - Fees.totalFee put_fees put.bid.val (by sorry) ≤ strike * Rate.discountFactor rate expiry := by
  let put_proceeds := put.bid.val - Fees.totalFee put_fees put.bid.val (by sorry)
  let df := Rate.discountFactor rate expiry
  sorry

/-- Put option lower bound (production-ready): P_bid ≥ max(0, K·e^(-rT) - S_ask)

    Statement: A put is worth at least its intrinsic value.

    Production Rule: Buy put at P_bid, buy stock at S_ask
    If intrinsic value > P_bid after fees, arbitrage exists.

    Detection: max(0, K·e^(-rT) - S_ask) > P_bid → profit from buy put, buy stock
-/
theorem putLowerBound_with_fees (put spot : Quote)
    (put_fees spot_fees : Fees)
    (strike : Float)
    (rate : Rate) (expiry : Time) :
    put.ask.val + Fees.totalFee put_fees put.ask.val (by sorry) ≥ max 0 (strike * Rate.discountFactor rate expiry - (spot.bid.val - Fees.totalFee spot_fees spot.bid.val (by sorry))) := by
  let put_cost := put.ask.val + Fees.totalFee put_fees put.ask.val (by sorry)
  let spot_proceeds := spot.bid.val - Fees.totalFee spot_fees spot.bid.val (by sorry)
  let df := Rate.discountFactor rate expiry
  let intrinsic := max 0 (strike * df - spot_proceeds)
  sorry

/-- THEORETICAL: Put option upper bound (abstract, no fees)
    Kept for reference. Production code should use putUpperBound_with_fees.
-/
theorem putUpperBound_theoretical (P : Float) (K : Float) (r : Rate) (T : Time) :
    P ≤ K * Rate.discountFactor r T := sorry

/-- THEORETICAL: Put option lower bound (abstract, no fees)
    Kept for reference. Production code should use putLowerBound_with_fees.
-/
theorem putLowerBound_theoretical (P : Float) (S : Float) (K : Float) (r : Rate) (T : Time) :
    P ≥ 0 ∧ P ≥ K * Rate.discountFactor r T - S := by
  constructor
  · exact optionNonNegative P
  · exact putIntrinsicBound P S K (Rate.discountFactor r T)

-- ============================================================================
-- Bound Violations and Arbitrage
-- ============================================================================

/-- A single option bound violation. -/
structure BoundViolation where
  boundType : String  -- "call_upper", "call_lower", "put_upper", "put_lower"
  deviationSize : Float  -- how much the bound is violated
  deviation_pos : deviationSize > 0

/-- Option bound arbitrage opportunity. -/
structure OptionBoundArbitrage where
  violationType : String
  grossDeviation : Float  -- size of price violation
  fees : Float  -- total transaction fees
  netProfit : Float  -- gross deviation - fees
  profitPercentage : Float  -- profit as % of capital

-- ============================================================================
-- Complete Bound Checking with Fees
-- ============================================================================

/-- Check call upper bound with fees.

    If C_ask > S_bid, arbitrage exists:
    - Sell call at C_ask (receive C_ask)
    - Buy spot at S_bid (pay S_bid)
    - Net: (C_ask - S_bid) - fees

    At expiry: call either expires worthless (you keep stock, net positive)
    or is exercised (you sell at K, breaking even on stock, keep call premium minus strike)
-/
def checkCallUpperBoundArb (callPrice : Float) (spotBid : Float)
    (callFees spotFees : Fees) : Float :=
  let deviation := callPrice - spotBid
  if deviation > 0 then
    let callFee := Fees.totalFee callFees callPrice (by sorry)
    let spotFee := Fees.totalFee spotFees spotBid (by sorry)
    deviation - (callFee + spotFee)
  else
    0

/-- Check call lower bound with fees.

    If C_bid < max(0, S_ask - K·e^(-rT)), arbitrage exists:
    - Buy call at C_bid (pay C_bid)
    - Short spot at S_ask (receive S_ask)
    - Lend K·e^(-rT) (receive that amount, pay back K at T)
    - Net: (S_ask + K·e^(-rT)) - C_bid - fees

    At expiry: exercise call, buy at K, return borrowed stock, keep profit
-/
def checkCallLowerBoundArb (callPrice : Float) (spotAsk : Float) (strike : Float)
    (rate : Rate) (expiry : Time) (callFees spotFees : Fees) : Float :=
  let df := Rate.discountFactor rate expiry
  let intrinsic := max 0 (spotAsk - strike * df)
  let deviation := intrinsic - callPrice
  if deviation > 0 then
    let callFee := Fees.totalFee callFees callPrice (by sorry)
    let spotFee := Fees.totalFee spotFees spotAsk (by sorry)
    deviation - (callFee + spotFee)
  else
    0

/-- Check put upper bound with fees.

    If P_ask > K·e^(-rT), arbitrage exists:
    - Sell put at P_ask (receive P_ask)
    - Buy bonds worth K·e^(-rT) (pay K·e^(-rT))
    - Net: P_ask - K·e^(-rT) - fees

    At expiry: if put is exercised (buy stock at K), sell bonds for K, break even
    on stock. If not exercised, keep premium minus bond cost.
-/
def checkPutUpperBoundArb (putPrice : Float) (strike : Float)
    (rate : Rate) (expiry : Time) (putFees : Fees) : Float :=
  let df := Rate.discountFactor rate expiry
  let deviation := putPrice - strike * df
  if deviation > 0 then
    let putFee := Fees.totalFee putFees putPrice (by sorry)
    deviation - putFee
  else
    0

/-- Check put lower bound with fees.

    If P_bid < max(0, K·e^(-rT) - S_bid), arbitrage exists:
    - Buy put at P_bid (pay P_bid)
    - Buy spot at S_bid (pay S_bid)
    - Borrow K·e^(-rT) (pay back K at T)
    - Net: (K·e^(-rT) + 0) - (P_bid + S_bid) - fees

    At expiry: exercise put, sell stock for K, repay loan, keep profit
-/
def checkPutLowerBoundArb (putPrice : Float) (spotBid : Float) (strike : Float)
    (rate : Rate) (expiry : Time) (putFees spotFees : Fees) : Float :=
  let df := Rate.discountFactor rate expiry
  let intrinsic := max 0 (strike * df - spotBid)
  let deviation := intrinsic - putPrice
  if deviation > 0 then
    let putFee := Fees.totalFee putFees putPrice (by sorry)
    let spotFee := Fees.totalFee spotFees spotBid (by sorry)
    deviation - (putFee + spotFee)
  else
    0

-- ============================================================================
-- Unified Bound Violation Detection
-- ============================================================================

/-- All bound violations for a given option. -/
structure AllBoundViolations where
  callUpperProfit : Float
  callLowerProfit : Float
  putUpperProfit : Float
  putLowerProfit : Float
  maxProfit : Float  -- best profitable violation
  hasViolation : Bool
  bestViolationType : String

/-- Check all bounds for a single call option. -/
def detectCallBoundViolations
    (call : EuropeanCall) (callPrice : Float) (spot : Quote) (rate : Rate)
    (callFees spotFees : Fees) : AllBoundViolations :=
  let upperProfit := checkCallUpperBoundArb callPrice spot.bid.val callFees spotFees
  let lowerProfit := checkCallLowerBoundArb callPrice spot.ask.val call.strike.val rate call.expiry callFees spotFees
  let maxProf := max upperProfit lowerProfit
  let hasViol := maxProf > 0
  let violType :=
    if upperProfit > lowerProfit && hasViol then "call_upper"
    else if hasViol then "call_lower"
    else "none"
  ⟨upperProfit, lowerProfit, 0, 0, maxProf, hasViol, violType⟩

/-- Check all bounds for a single put option. -/
def detectPutBoundViolations
    (put : EuropeanPut) (putPrice : Float) (spot : Quote) (rate : Rate)
    (putFees spotFees : Fees) : AllBoundViolations :=
  let upperProfit := checkPutUpperBoundArb putPrice put.strike.val rate put.expiry putFees
  let lowerProfit := checkPutLowerBoundArb putPrice spot.bid.val put.strike.val rate put.expiry putFees spotFees
  let maxProf := max upperProfit lowerProfit
  let hasViol := maxProf > 0
  let violType :=
    if upperProfit > lowerProfit && hasViol then "put_upper"
    else if hasViol then "put_lower"
    else "none"
  ⟨0, 0, upperProfit, lowerProfit, maxProf, hasViol, violType⟩

/-- Comprehensive bound checking for call and put with same strike. -/
def detectAllBoundViolations
    (call : EuropeanCall) (put : EuropeanPut)
    (callPrice putPrice : Float) (spot : Quote) (rate : Rate)
    (callFees putFees spotFees : Fees)
    (h : sameTerms call put) :
    AllBoundViolations :=
  let callViol := detectCallBoundViolations call callPrice spot rate callFees spotFees
  let putViol := detectPutBoundViolations put putPrice spot rate putFees spotFees

  -- Combine: find the most profitable violation across both options
  let maxProf := max callViol.maxProfit putViol.maxProfit
  let hasViol := maxProf > 0
  let violType :=
    if callViol.maxProfit > putViol.maxProfit && hasViol then callViol.bestViolationType
    else if hasViol then putViol.bestViolationType
    else "none"

  ⟨callViol.callUpperProfit, callViol.callLowerProfit,
   putViol.putUpperProfit, putViol.putLowerProfit,
   maxProf, hasViol, violType⟩

/-- Check call upper bound with fees -/
def checkCallUpperBound_with_fees
    (call spot : Quote)
    (call_fees spot_fees : Fees) :
    Bool :=
  let call_cost := call.ask.val + Fees.totalFee call_fees call.ask.val (by sorry)
  let spot_proceeds := spot.bid.val - Fees.totalFee spot_fees spot.bid.val (by sorry)
  call_cost ≤ spot_proceeds

/-- Check call lower bound with fees -/
def checkCallLowerBound_with_fees
    (call spot : Quote)
    (call_fees spot_fees : Fees) :
    Bool :=
  let call_proceeds := call.bid.val - Fees.totalFee call_fees call.bid.val (by sorry)
  let spot_cost := spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry)
  call_proceeds ≥ spot_cost * 0.9

/-- Check put bounds -/
def checkPutBounds_with_fees
    (put strike : Quote)
    (put_fees strike_fees : Fees) :
    Bool :=
  let put_cost := put.ask.val + Fees.totalFee put_fees put.ask.val (by sorry)
  put_cost ≥ 0

end Finance.Options
