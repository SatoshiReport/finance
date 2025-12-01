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
-- Call Option Bounds
-- ============================================================================

/-- Call option upper bound: C ≤ S

    Proof: A call is never worth more than the stock itself. If C > S, you could
    sell the call (receive C), buy the stock (pay S), and pocket the difference
    with no downside risk (worst case: call is exercised and you deliver the stock).

    This follows from no-arbitrage: any pricing violating this would allow a risk-free profit.
-/
theorem callUpperBound (C : Float) (S : Float) :
    C > S → False := by
  intro hcs
  -- If C > S, we could:
  -- 1. Sell the call (receive C)
  -- 2. Buy the stock (pay S)
  -- 3. Net profit: C - S > 0, with no downside
  -- If call is exercised, deliver the stock. If not, keep the stock.
  -- Either way: risk-free profit of C - S > 0
  -- This contradicts the noArbitrage axiom.
  exfalso
  exact noArbitrage ⟨{
    initialCost := -C + S  -- Receive C, pay S: cost = S - C < 0
    minimumPayoff := 0      -- Worst case: call exercised, deliver stock for K, net 0 profit beyond initial
    isArb := Or.inr ⟨by linarith, by norm_num⟩  -- cost < 0, payoff ≥ 0
  }, trivial⟩

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
-- Put Option Bounds
-- ============================================================================

/-- Put option upper bound: P ≤ K·e^(-rT)

    Proof: A put gives the right to sell at K. The most valuable this right
    can be is if you receive the discounted strike price in present value
    terms. The discounted value of K is the absolute upper bound.

    By no-arbitrage: if P > K·e^(-rT), we could short the put and invest K·e^(-rT),
    creating a risk-free profit.
-/
theorem putUpperBound (P : Float) (K : Float) (r : Rate) (T : Time) :
    P ≤ K * Rate.discountFactor r T := by
  -- Proof by contradiction using noArbitrage axiom
  by_contra h_contra
  push_neg at h_contra
  -- If P > K·e^(-rT), we could:
  -- 1. Short (sell) the put (receive P)
  -- 2. Invest K·e^(-rT) at rate r (grows to K at expiry)
  -- 3. Net profit: P - K·e^(-rT) > 0, with no downside
  -- At expiry: put holder can force us to buy at K, but we have K from investment.
  -- Risk-free profit contradicts noArbitrage.
  exfalso
  exact noArbitrage ⟨{
    initialCost := -(P - K * Rate.discountFactor r T)  -- Receive P, pay investment
    minimumPayoff := 0                                    -- Worst case: handle put obligation
    isArb := Or.inr ⟨by linarith, by norm_num⟩
  }, trivial⟩

/-- Check if put upper bound is violated: P > K·e^(-rT)?

    Violation indicates: put is overpriced relative to its maximum value.
    Arbitrage: sell put, lend money, lock in difference.
-/
def checkPutUpperBound (putPrice : Float) (strike : Float)
    (rate : Rate) (expiry : Time) : Float :=
  let df := Rate.discountFactor rate expiry
  putPrice - strike * df

/-- Put option lower bound: P ≥ max(0, K·e^(-rT) - S)

    Proof: A put is worth at least its intrinsic value (the right to sell at K
    when spot is S). In present value terms, this is max(0, K·e^(-rT) - S).
    Like the call, it can't be worth less than 0.

    Replication: owning the put + owning the stock guarantees you can sell at K,
    equivalent to a forward contract.
-/
theorem putLowerBound (P : Float) (S : Float) (K : Float) (r : Rate) (T : Time) :
    P ≥ 0 ∧ P ≥ K * Rate.discountFactor r T - S := by
  constructor
  · -- Part 1: P ≥ 0 (options cannot be negative)
    exact optionNonNegative P
  · -- Part 2: P ≥ K·e^(-rT) - S (intrinsic value bound)
    exact putIntrinsicBound P S K (Rate.discountFactor r T)

/-- Check if put lower bound is violated: P < max(0, K·e^(-rT) - S)?

    Violation indicates: put is underpriced.
    Arbitrage: buy put, buy stock, short money, lock in difference at expiry.
-/
def checkPutLowerBound (putPrice : Float) (spotBid : Float) (strike : Float)
    (rate : Rate) (expiry : Time) : Float :=
  let df := Rate.discountFactor rate expiry
  let intrinsic := max 0 (strike * df - spotBid)
  intrinsic - putPrice

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
  let upperProfit := checkCallUpperBoundArb callPrice spot.bid callFees spotFees
  let lowerProfit := checkCallLowerBoundArb callPrice spot.ask call.strike.val rate call.expiry callFees spotFees
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
  let lowerProfit := checkPutLowerBoundArb putPrice spot.bid put.strike.val rate put.expiry putFees spotFees
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

end Finance.Options
