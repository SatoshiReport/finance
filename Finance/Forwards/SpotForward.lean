-- Spot-forward parity and cost of carry
-- Formalizes the fundamental relationship between spot and forward prices

import Finance.Core

namespace Finance.Forwards

-- ============================================================================
-- Forward Price Definition
-- ============================================================================

/-- A forward contract: obligation to buy underlying at strike F at time T. -/
structure Forward where
  strike : ℝ  -- The agreed forward price
  expiry : Time

/-- The forward price under cost of carry: F = S·e^((r-q)T)

    Where:
    - S: spot price
    - r: risk-free rate
    - q: convenience yield or dividend yield
    - T: time to expiry

    Intuition: Forward price accounts for:
    1. Time value of money (earning at rate r while holding cash)
    2. Foregone dividends/yield (receiving at rate q by holding spot)
    3. Net effect: F = S·e^((r-q)T)
-/
noncomputable def forwardPrice (spot : ℝ) (rate : Rate) (yield : Rate) (time : Time) : ℝ :=
  spot * Real.exp ((rate.val - yield.val) * time.val)

-- ============================================================================
-- Spot-Forward Parity
-- ============================================================================

/-- Spot-Forward Parity: The fundamental no-arbitrage relationship.

    F = S·e^((r-q)T)

    Where:
    - F: forward price
    - S: spot price
    - r: risk-free rate
    - q: yield (dividends or convenience yield)
    - T: time to expiry
    - e^((r-q)T): growth factor accounting for carry costs

    This relationship holds because:
    1. Buy spot at S, finance at rate r, hold for T
    2. Receive dividends at rate q over T
    3. Must sell forward to lock in price F
    4. By no-arbitrage: F = S·e^((r-q)T)

    Violation creates carry arbitrage:
    - If F < S·e^((r-q)T): buy spot + sell forward
    - If F > S·e^((r-q)T): sell spot + buy forward
-/
theorem spotForwardParity
    (spot : ℝ) (rate yield : Rate) (time : Time)
    (F : ℝ) :
    F = forwardPrice spot rate yield time := by
  -- Spot-Forward Parity is the fundamental no-arbitrage relationship.
  -- Proof: Consider replicating a forward contract with cash and spot:
  -- 1. Buy spot at S
  -- 2. Borrow at rate r for time T
  -- 3. Collect dividends at rate q
  -- 4. Sell forward at price F
  -- By no-arbitrage, the cost of this replicating portfolio must equal
  -- the cost of entering the forward contract directly.
  -- The replication cost is: S·e^((r-q)T)
  -- Therefore: F = S·e^((r-q)T)
  sorry  -- Requires no-arbitrage axiom and replication argument

-- ============================================================================
-- Forward Mispricing Detection
-- ============================================================================

/-- Check if forward is overpriced (selling opportunity).

    Returns the profit from: sell forward at F, buy spot at S_ask,
    borrow at r, and collect dividends at q.

    Profit = F·e^(-rT) - S_ask
    (discounting forward payoff and comparing to spot cost)

    If positive: forward is too expensive, sell it.
-/
noncomputable def checkForwardTooExpensive
    (spotAsk : ℝ) (forwardBid : ℝ) (rate yield : Rate) (time : Time) :
    ℝ :=
  let df := Rate.discountFactor rate time
  let theoreticalForward := forwardPrice spotAsk rate yield time
  forwardBid - theoreticalForward

/-- Check if forward is underpriced (buying opportunity).

    Returns the profit from: buy forward at F, short sell spot at S_bid,
    lend at r, and pay dividends at q.

    Profit = S_bid - F·e^(-rT)

    If positive: forward is too cheap, buy it.
-/
noncomputable def checkForwardToocheap
    (spotBid : ℝ) (forwardAsk : ℝ) (rate yield : Rate) (time : Time) :
    ℝ :=
  let theoreticalForward := forwardPrice spotBid rate yield time
  theoreticalForward - forwardAsk

/-- A forward price violation. -/
structure ForwardViolation where
  deviationType : String  -- "overpriced" or "underpriced"
  deviationSize : ℝ   -- How much forward deviates from fair value
  profitOpportunity : ℝ  -- Net profit after accounting for deviations

/-- Check if forward contract shows arbitrage opportunity. -/
noncomputable def hasForwardArbitrage
    (spotQuote : Quote) (forwardBid forwardAsk : ℝ)
    (rate yield : Rate) (time : Time) :
    Bool :=
  let overpriced := checkForwardTooExpensive spotQuote.ask.val forwardBid rate yield time
  let underpriced := checkForwardToocheap spotQuote.bid.val forwardAsk rate yield time
  overpriced > 0 || underpriced > 0

/-- Analyze forward for arbitrage with fees. -/
noncomputable def analyzeForwardArbitrage
    (spotQuote : Quote) (forwardBid forwardAsk : ℝ)
    (rate yield : Rate) (time : Time)
    (spotFees forwardFees : Fees) :
    Option ForwardViolation :=
  let overpriced := checkForwardTooExpensive spotQuote.ask.val forwardBid rate yield time
  let underpriced := checkForwardToocheap spotQuote.bid.val forwardAsk rate yield time

  if overpriced > 0 then
    let spotFee := Fees.totalFee spotFees spotQuote.ask.val (by sorry)
    let fwdFee := Fees.totalFee forwardFees forwardBid (by sorry)
    let netProfit := overpriced - spotFee - fwdFee
    if netProfit > 0 then
      some ⟨"overpriced", overpriced, netProfit⟩
    else
      none
  else if underpriced > 0 then
    let spotFee := Fees.totalFee spotFees spotQuote.bid.val (by sorry)
    let fwdFee := Fees.totalFee forwardFees forwardAsk (by sorry)
    let netProfit := underpriced - spotFee - fwdFee
    if netProfit > 0 then
      some ⟨"underpriced", underpriced, netProfit⟩
    else
      none
  else
    none

-- ============================================================================
-- Cost of Carry Framework
-- ============================================================================

/-- The "cost of carry" is (r - q): the net rate of financing less yield.

    Positive carry: r > q (pay more to finance than you get in dividends)
    Negative carry: r < q (get more in dividends than you pay to finance)
    Zero carry: r = q (perfectly balanced)

    This determines whether futures trade at premium or discount to spot.
-/
def carryRate (rate yield : Rate) : Rate :=
  ⟨rate.val - yield.val⟩

/-- Storage costs increase forward prices.

    F = S·e^((r+u-q)T)

    Where u is the storage/convenience yield adjustment.

    Examples:
    - Physical commodities: storage increases forward price
    - Financial assets: storage negligible
    - Perishables: negative storage (spoilage)
-/
noncomputable def forwardPriceWithStorage
    (spot : ℝ) (rate yield storage : Rate) (time : Time) : ℝ :=
  spot * Real.exp ((rate.val + storage.val - yield.val) * time.val)

-- ============================================================================
-- Forward Basis
-- ============================================================================

/-- The "basis" is the difference between forward and spot: B = F - S

    The basis should equal carry costs: B = S·(e^((r-q)T) - 1) ≈ S·(r-q)·T

    Basis should be predictable and should decrease as expiry approaches.
    Abnormal basis = arbitrage opportunity.
-/
def basis (forward spot : ℝ) : ℝ :=
  forward - spot

/-- Theoretical basis accounting for carry. -/
noncomputable def theoreticalBasis (spot : ℝ) (rate yield : Rate) (time : Time) : ℝ :=
  spot * (Real.exp ((rate.val - yield.val) * time.val) - 1)

/-- Check if basis is abnormal (too wide or too narrow). -/
noncomputable def checkBasisViolation
    (forward spot : ℝ) (rate yield : Rate) (time : Time) :
    ℝ :=
  let actualBasis := basis forward spot
  let theoretical := theoreticalBasis spot rate yield time
  -- Positive = basis too wide (forward too expensive)
  -- Negative = basis too narrow (forward too cheap)
  actualBasis - theoretical

end Finance.Forwards
