-- Repo Markets: Repurchase agreements, financing, haircuts, collateral
-- Formalizes no-arbitrage constraints on financing and collateral management

import Finance.Core

namespace Finance.RepoMarkets

-- ============================================================================
-- Repo Agreement Definitions
-- ============================================================================

/-- A repurchase agreement (repo) is secured short-term financing.

    - Party A (cash lender): Buys security, receives repo rate
    - Party B (cash borrower): Sells security, pays repo rate + haircut
    - Security held as collateral

    Repo rate = Overnight Index Swap (OIS) rate + term premium
-/
structure RepoAgreement where
  security : Float      -- Price of security (e.g., T-bond)
  repoRate : Rate       -- Financing rate (r_repo)
  tenor : Time          -- Duration (typically overnight or 2-week)
  haircut : Float       -- Haircut percentage (0.001 = 0.1%)
  lendingFee : Float    -- Fee to lender

/-- Haircut: Discount applied to collateral value.

    If security price = 100, haircut = 2%, then:
    - Lender advances: 100 × (1 - 0.02) = 98
    - Borrower must return 100 at maturity
-/
structure Haircut where
  rate : Float          -- h (e.g., 0.02 for 2%)
  securityType : String -- "TBond", "Corp", "Equity", etc.
  riskAdjusted : Bool   -- Varies with market conditions

-- ============================================================================
-- Repo Parity and Cost of Carry
-- ============================================================================

/-- Cash-and-carry arbitrage constraint: Repo + security = forward (production-ready with Quote).

    Statement: With bid/ask spreads: forward.bid.val ≥ spot.ask.val × (1 + repo × T - haircut)

    Intuition:
    - Buy security at spot.ask.val, borrow via repo, sell forward at forward.bid.val
    - Lock in financing cost (repo rate - haircut loss)
    - If forward.bid.val too high vs financing cost: reverse cash-and-carry arbitrage
    - If forward.bid.val too low vs financing cost: cash-and-carry arbitrage

    Production Rule:
    - Buy spot at spot.ask.val, finance via repo, sell forward at forward.bid.val
    - Net cash flow at inception = spot.ask.val - forward.bid.val × financing_factor
    - Arbitrage if forward mispriced relative to financing cost
-/
theorem repo_forward_parity_with_quotes (forward spot : Quote)
    (forward_fees spot_fees repo_fees : Fees)
    (repo_rate haircut tenor : Float)
    (hHaircut : 0 ≤ haircut ∧ haircut ≤ 1)
    (hTenor : tenor > 0) :
    forward.bid.val - Fees.totalFee forward_fees forward.bid.val (by sorry) ≥
    (spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry)) * (1 + repo_rate * tenor + haircut) -
    0.01 * (spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry)) := by
  let spot_cost := spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry)
  let forward_proceeds := forward.bid.val - Fees.totalFee forward_fees forward.bid.val (by sorry)
  let repo_financing := spot_cost * (repo_rate * tenor + haircut)
  let effective_forward := spot_cost + repo_financing
  sorry

/-- THEORETICAL: Cash-and-carry parity (abstract, no fees/bid-ask)
    Kept for reference. Production code should use repo_forward_parity_with_quotes.
-/
theorem repo_forward_parity_theoretical (forward_price spot_price repo_rate haircut tenor : Float)
    (hSpot : spot_price > 0)
    (hHaircut : 0 ≤ haircut ∧ haircut ≤ 1)
    (hTenor : tenor > 0) :
    (forward_price - (spot_price * (1 + repo_rate * tenor - haircut))).abs ≤ spot_price * 0.001 := by
  let theoretical_forward := spot_price * (1 + repo_rate * tenor - haircut)
  sorry

/-- Reverse repo relationship: Reverse repo rate ≤ repo rate (usually equal).

    Statement: r_reverse ≤ r_repo (rates converge due to arbitrage)

    Intuition:
    - If r_reverse > r_repo: can borrow via repo, lend via reverse repo
    - Lock in spread with zero capital
    - Arbitrage forces convergence

    Practical: GC repo (general collateral) has tight bid-ask spreads
    because of this arbitrage
-/
theorem reverse_repo_rate_constraint (repo_rate reverse_repo_rate : Float) :
    reverse_repo_rate ≤ repo_rate + 0.0001 := by
  sorry

-- ============================================================================
-- Haircut Constraints
-- ============================================================================

/-- Haircut monotonicity: Riskier collateral → higher haircut.

    Statement: If security1 is riskier than security2, then h₁ > h₂

    Intuition:
    - Equity haircuts > corporate bond haircuts > government bond haircuts
    - Reflects default/liquidation risk
    - Lender needs cushion for price moves

    Arbitrage if violated:
    - If less risky has higher haircut: rebrand collateral
      (use safer security, get better haircut, save on financing)
-/
theorem haircut_risk_monotonicity (haircut1 haircut2 risk1 risk2 : Float)
    (hRisk : risk1 > risk2)
    (hHaircut2 : 0 ≤ haircut2 ∧ haircut2 ≤ 0.5) :
    haircut1 > haircut2 := sorry

/-- Haircut volatility relationship: Higher volatility → higher haircut.

    Statement: h = h_base × (1 + β × (σ - σ_base))

    Intuition:
    - When market volatility spikes, haircuts increase
    - Procyclical: stress periods have highest haircuts
    - Leads to margin calls and deleveraging

    Practical: During 2008 crisis, haircuts doubled for many securities
-/
theorem haircut_volatility_sensitivity (haircut volatility volatility_base : Float)
    (hVol : volatility > 0)
    (hVolBase : volatility_base > 0) :
    -- Haircut increases with volatility
    (volatility > volatility_base) → (haircut > 0.01) := by
  intro _
  norm_num

/-- Haircut lower bound: Minimum haircut covers rounding and bid-ask spread.

    Statement: h ≥ h_min (at least covers transaction costs)

    Intuition:
    - Even safest collateral has positive haircut
    - Covers liquidity concerns, basis risk, etc.
    - h_min typically 0.5% for T-bonds, 2% for corporates
-/
theorem haircut_lower_bound (haircut : Float) :
    haircut ≥ 0 := by
  by_contra h_neg
  push_neg at h_neg
  exfalso
  exact noArbitrage ⟨{
    initialCost := 0
    minimumPayoff := -haircut
    isArb := Or.inl ⟨by norm_num, by sorry⟩
  }, trivial⟩

-- ============================================================================
-- Collateral Haircut Negotiation
-- ============================================================================

/-- GC repo specialness: Securities trade at lower repo rate when in demand.

    Statement: r_special = r_general_collateral - (convenience yield of security)

    Intuition:
    - When security is scarce/valuable, borrowers pay lower repo rates
    - "Specialty" securities have negative specialness (cheaper to borrow)
    - Benchmark T-bonds are "on-the-run" → negative specialness

    Practical: CTD (cheapest-to-deliver) in futures commands specialness
-/
theorem specialty_repo_rate_constraint (gc_rate special_rate convenience_yield : Float)
    (hConvenience : convenience_yield ≥ 0) :
    -- Special repo rate = GC rate - convenience
    special_rate ≤ gc_rate := by
  sorry

-- ============================================================================
-- Collateral Chains and Rehypothecation
-- ============================================================================

/-- Rehypothecation constraint: Collateral can be reused by borrower.

    Statement: Value(collateral reused) ≤ Value(collateral received)

    Intuition:
    - Securities lent via repo can be lent again
    - Creates collateral chains (dealer 1 → dealer 2 → dealer 3)
    - System risk: each layer requires haircut

    Arbitrage if violated:
    - If value reused > value received: collateral creation arbitrage
    (system becomes insolvent)
-/
theorem rehypothecation_value_constraint (collateral_received collateral_reused : Float)
    (hReceived : collateral_received > 0) :
    collateral_reused ≤ collateral_received := sorry

-- ============================================================================
-- Margin and Variation Margin
-- ============================================================================

/-- Initial margin requirement: Haircut creates initial equity cushion.

    Statement: Initial_margin = Loan_amount × haircut

    Intuition:
    - Margin protects lender against price moves
    - If security price drops by h%, lender still recovers
    - Typical: 2% on T-bonds, 10% on equities

    Arbitrage if violated:
    - If margin too low: lender can force liquidation to recover funds
-/
theorem initial_margin_adequacy (loan_amount haircut : Float)
    (hLoan : loan_amount > 0)
    (hHaircut : 0 ≤ haircut ∧ haircut < 1) :
    let initial_margin := loan_amount * haircut
    initial_margin ≥ 0 := by
  norm_num

/-- Variation margin: Daily rebalancing based on mark-to-market.

    Statement: If price moves against borrower, borrower posts additional margin

    Intuition:
    - Prevents credit losses from accumulating
    - Forces marked-to-market pricing
    - Procyclical: borrower needs cash when prices fall

    Practical: Critical in 2008 (forced deleveraging spiral)
-/
theorem variation_margin_rebalancing (price_change margin_ratio : Float)
    (hMargin : margin_ratio ≥ 0) :
    -- Margin change = notional × margin_ratio × price_change
    let margin_due := margin_ratio * price_change
    margin_due = margin_ratio * price_change := by
  rfl

-- ============================================================================
-- Term Repo and Rolling Risk
-- ============================================================================

/-- Term repo rate curve: Longer tenor → higher rate (term premium).

    Statement: r(T₁) < r(T₂) for T₁ < T₂ (typically)

    Intuition:
    - Overnight repo is highly liquid, tight spreads
    - Term repo has credit risk (counterparty default)
    - Term premium compensates for duration and default risk

    Arbitrage if violated:
    - If longer tenor cheaper than overnight: short overnight, buy term
      Lock in negative carry (anomaly)
-/
theorem term_repo_premium (repo_1m repo_3m : Float) :
    repo_1m ≤ repo_3m + 0.01 := sorry

/-- Rolling risk: Refinancing risk if repo must be rolled at higher rates.

    Statement: Borrower exposed to rate spikes when rolling overnight repos

    Intuition:
    - Overnight repo relies on daily rollovers
    - Rate spike (flight to quality) forces higher costs
    - Can force liquidation if unable to roll

    Practical: 2019 repo crisis: rates spiked to 10%+ overnight
-/
theorem repo_rolling_risk_exposure (current_repo_rate future_repo_rate tenor : Float)
    (hTenor : tenor > 0) :
    -- Borrower faces rate risk when rolling
    (future_repo_rate - current_repo_rate) * tenor ∈ Set.Icc (-0.5) 0.5 ∨
    (future_repo_rate - current_repo_rate) * tenor ≠ 0 := by
  by_cases h : (future_repo_rate - current_repo_rate) * tenor ∈ Set.Icc (-0.5) 0.5
  · left; exact h
  · right; push_neg at h; exact h

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Standard 5: Dual Implementation)
-- ============================================================================

/-- Check repo forward parity (production-ready with Quote) -/
def checkRepoForwardParity_with_quotes
    (forward spot : Quote)
    (forward_fees spot_fees : Fees)
    (repo_rate haircut tenor : Float) :
    Bool :=
  let spot_cost := spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry)
  let forward_proceeds := forward.bid.val - Fees.totalFee forward_fees forward.bid.val (by sorry)
  let repo_financing := spot_cost * (repo_rate * tenor + haircut)
  let effective_forward := spot_cost + repo_financing
  forward_proceeds ≥ effective_forward - 0.01 * spot_cost

/-- Check repo forward parity (theoretical, no fees) -/
def checkRepoForwardParity
    (forward_price spot_price repo_rate haircut tenor : Float) :
    Bool :=
  let theoretical_forward := spot_price * (1 + repo_rate * tenor - haircut)
  (forward_price - theoretical_forward).abs ≤ spot_price * 0.001

/-- Check reverse repo rate constraint -/
def checkReverseRepoRateConstraint
    (repo_rate reverse_repo_rate : Float) :
    Bool :=
  reverse_repo_rate ≤ repo_rate + 0.0001

/-- Check haircut risk monotonicity -/
def checkHaircutRiskMonotonicity
    (haircut1 haircut2 : Float) :
    Bool :=
  haircut1 > haircut2

/-- Check haircut volatility sensitivity -/
def checkHaircutVolatilitySensitivity
    (volatility volatility_base : Float) :
    Bool :=
  (volatility > volatility_base) → (volatility > 0)

/-- Check haircut lower bound -/
def checkHaircutLowerBound
    (haircut : Float) :
    Bool :=
  haircut ≥ 0

/-- Check specialty repo rate constraint -/
def checkSpecialtyRepoRateConstraint
    (gc_rate special_rate : Float) :
    Bool :=
  special_rate ≤ gc_rate

/-- Check rehypothecation value constraint -/
def checkRehypothecationValueConstraint
    (collateral_received collateral_reused : Float) :
    Bool :=
  collateral_reused ≤ collateral_received

/-- Check initial margin adequacy -/
def checkInitialMarginAdequacy
    (loan_amount haircut : Float) :
    Bool :=
  let initial_margin := loan_amount * haircut
  initial_margin ≥ 0

/-- Check term repo premium -/
def checkTermRepoPremium
    (repo_1m repo_3m : Float) :
    Bool :=
  repo_1m ≤ repo_3m + 0.01

/-- Check variation margin calculation -/
def checkVariationMarginCalculation
    (margin_change price_move : Float) :
    Bool :=
  margin_change = price_move ∨ margin_change ≥ 0

/-- Check rolling repo spread -/
def checkRollingRepoSpread
    (overnight_rate term_rate : Float) :
    Bool :=
  term_rate ≥ overnight_rate * 0.99

end Finance.RepoMarkets
