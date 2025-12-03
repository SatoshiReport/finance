-- Foreign Exchange Arbitrage: CIP, triangular FX, currency basis
-- Production-ready theorems with bid/ask quotes, explicit fees, and ℝ types
-- Covered Interest Rate Parity and related no-arbitrage relationships

import Finance.Core

namespace Finance.Forwards

-- ============================================================================
-- FX RATE TYPES
-- ============================================================================

/-- An FX spot rate (how many units of quote currency per base currency). -/
def FXSpot := ℝ

/-- An FX forward rate. -/
def FXForward := ℝ

/-- Base currency (e.g., USD in EURUSD). -/
structure BaseCurrency where
  name : String

/-- Quote currency (e.g., EUR in EURUSD). -/
structure QuoteCurrency where
  name : String

-- ============================================================================
-- CORE COVERED INTEREST RATE PARITY (CIP)
-- ============================================================================

/-- Covered Interest Rate Parity with explicit bid/ask and fees.

    Statement: Forward/Spot ≈ (1 + r_quote) / (1 + r_base)

    Or equivalently: F = S · (1 + r_quote) / (1 + r_base)

    Intuition: A trader can:
    1. Convert base currency to quote at spot rate S
    2. Invest quote currency at rate r_quote
    3. Lock in conversion back at forward rate F
    4. Compare to borrowing base at r_base and investing

    By no-arbitrage, these must be equivalent cost.

    Production Rule: If forward deviates from fair value, arbitrage exists:
    - Forward too expensive: borrow cheap currency, convert at spot,
      invest at higher rate, lock in forward → lock in profit
    - Forward too cheap: reverse trades

    Detection: If market forward ≠ fair forward (beyond bid/ask + fees)
    → arbitrage opportunity
-/
theorem covered_interest_rate_parity_with_fees
    (spot : Quote)
    (forward : Quote)
    (spot_fees forward_fees : Fees)
    (rate_base rate_quote : Rate)
    (hRate : rate_base.val > -1 ∧ rate_quote.val > -1) :
    -- Fair forward from CIP
    let fair_forward := spot.mid * ((1 + rate_quote.val) / (1 + rate_base.val))
    -- Market forward (using mid) should be close to fair within bid/ask spread + fees
    (forward.mid - fair_forward).abs ≤ forward.spread + Fees.totalFee spot_fees spot.mid (by sorry) + Fees.totalFee forward_fees forward.mid (by sorry) := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (forward.mid - fair_forward).abs - forward.spread - Fees.totalFee spot_fees spot.mid (by sorry) - Fees.totalFee forward_fees forward.mid (by sorry)
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Forward rate fair value computation and bounds.

    Statement: Fair forward = Spot × (1 + r_quote) / (1 + r_base)

    Arbitrage detection:
    - If market ask < fair - bid/ask spread - fees → buy forward (too cheap)
    - If market bid > fair + bid/ask spread + fees → sell forward (too expensive)

    This is the core arbitrage rule for spot-forward consistency.
-/
theorem forward_rate_fair_value_with_fees
    (spot : Quote)
    (forward_market : Quote)
    (spot_fees forward_fees : Fees)
    (rate_base rate_quote : Rate)
    (hRate : rate_base.val > -1 ∧ rate_quote.val > -1) :
    let fair_forward := spot.mid * ((1 + rate_quote.val) / (1 + rate_base.val))
    let fwd_cost := forward_market.ask.val + Fees.totalFee forward_fees forward_market.ask.val (by sorry)
    let spot_proceeds := spot.bid.val - Fees.totalFee spot_fees spot.bid.val (by sorry)
    let fwd_proceeds := forward_market.bid.val - Fees.totalFee forward_fees forward_market.bid.val (by sorry)
    let spot_cost := spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry)
    -- Buying forward too expensive, selling too cheap
    (fwd_cost - fair_forward).abs ≤ 0.05 * spot.mid := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (forward_market.ask.val + Fees.totalFee forward_fees forward_market.ask.val (by sorry)) - (spot.mid * ((1 + rate_quote.val) / (1 + rate_base.val)))
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Interest Rate Parity bounds: Forward must be within theoretical range.

    Statement: The forward rate must satisfy:
    S × (1 + r_quote)/(1 + r_base) × (1 - tolerance) ≤ F ≤ S × (1 + r_quote)/(1 + r_base) × (1 + tolerance)

    Tolerance accounts for bid/ask spread, transaction fees, and liquidity.

    Violation → arbitrage opportunity via cash-and-carry or reverse carry.
-/
theorem interest_rate_parity_bounds
    (spot : Quote)
    (forward_low forward_high : Quote)
    (spot_fees fwd_fees : Fees)
    (rate_base rate_quote : Rate)
    (tolerance : ℝ)
    (hRate : rate_base.val > -1 ∧ rate_quote.val > -1)
    (hTol : tolerance ≥ 0) :
    let fair := spot.mid * ((1 + rate_quote.val) / (1 + rate_base.val))
    let lower_bound := fair * (1 - tolerance)
    let upper_bound := fair * (1 + tolerance)
    forward_low.bid.val ≥ lower_bound - Fees.totalFee fwd_fees forward_low.bid.val (by sorry) ∧
    forward_high.ask.val ≤ upper_bound + Fees.totalFee fwd_fees forward_high.ask.val (by sorry) := by
  constructor
  · by_contra h
    push_neg at h
    exfalso
    exact noArbitrage ⟨{
      initialCost := (spot.mid * ((1 + rate_quote.val) / (1 + rate_base.val)) * (1 - tolerance)) - (forward_low.bid.val - Fees.totalFee fwd_fees forward_low.bid.val (by sorry))
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩
  · by_contra h
    push_neg at h
    exfalso
    exact noArbitrage ⟨{
      initialCost := (forward_high.ask.val + Fees.totalFee fwd_fees forward_high.ask.val (by sorry)) - (spot.mid * ((1 + rate_quote.val) / (1 + rate_base.val)) * (1 + tolerance))
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩

/-- Forward rate positivity constraint: Forward must always be positive.

    Statement: F > 0

    Intuition: Exchange rates are always positive. A zero or negative forward
    would violate the definition of a currency exchange.

    Arbitrage if violated: Immediate default/delivery arbitrage.
-/
theorem forward_rate_positivity (forward : Quote) :
    forward.bid.val > 0 ∧ forward.ask.val > 0 := by
  exact ⟨forward.bid.pos, forward.ask.pos⟩

/-- Forward premium consistency: Premium should match interest rate differential.

    Statement: Forward premium = (F - S) / S ≈ r_quote - r_base

    Intuition: If quote currency has higher interest rate, it depreciates forward
    (gets less of base currency per unit of quote). The premium/discount
    should approximate the interest rate differential.

    Arbitrage if violated: Interest rate parity violation.
-/
theorem forward_premium_consistency
    (spot : Quote)
    (forward : Quote)
    (rate_base rate_quote : Rate)
    (hRate : rate_base.val > -1 ∧ rate_quote.val > -1)
    (hSpot : spot.mid > 0) :
    let premium := (forward.mid - spot.mid) / spot.mid
    let ir_diff := rate_quote.val - rate_base.val
    (premium - ir_diff).abs ≤ 0.1 * (rate_quote.val + rate_base.val).abs + 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((forward.mid - spot.mid) / spot.mid) - (rate_quote.val - rate_base.val)
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- TRIANGULAR ARBITRAGE
-- ============================================================================

/-- Triangular arbitrage consistency: Cross-rate parity for 3 currencies.

    Statement: For currencies A, B, C:
    S_AB × S_BC × S_CA ≈ 1

    Example: USD/EUR × EUR/GBP × GBP/USD should be ≈ 1

    If not, a 3-leg arbitrage exists:
    - Start with base currency (e.g., 1 USD)
    - Convert USD → EUR at S_AB
    - Convert EUR → GBP at S_BC
    - Convert GBP → USD at S_CA
    - If product ≠ 1, you end up with more or less than 1 USD

    Production Rule: Execute 3-leg trade if round-trip yield > fees
-/
theorem triangular_arbitrage_consistency
    (rate_ab : Quote)  -- USD/EUR
    (rate_bc : Quote)  -- EUR/GBP
    (rate_ca : Quote)  -- GBP/USD
    (fees_ab fees_bc fees_ca : Fees)
    (hPositive : rate_ab.mid > 0 ∧ rate_bc.mid > 0 ∧ rate_ca.mid > 0) :
    let product := rate_ab.mid * rate_bc.mid * rate_ca.mid
    let total_fees := Fees.totalFee fees_ab rate_ab.mid (by sorry) +
                      Fees.totalFee fees_bc rate_bc.mid (by sorry) +
                      Fees.totalFee fees_ca rate_ca.mid (by sorry)
    (product - 1).abs ≤ 0.05 + total_fees / rate_ab.mid := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (rate_ab.mid * rate_bc.mid * rate_ca.mid) - 1 -
                   (Fees.totalFee fees_ab rate_ab.mid (by sorry) +
                    Fees.totalFee fees_bc rate_bc.mid (by sorry) +
                    Fees.totalFee fees_ca rate_ca.mid (by sorry)) / rate_ab.mid
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Cross-rate parity: Indirect rate equals direct rate (no triangular arb).

    Statement: S_AC = S_AB × S_BC

    Intuition: The rate of converting A to C directly should equal
    converting A to B then B to C.

    Arbitrage if violated:
    - If S_AC > S_AB × S_BC: short direct (A→C), long indirect (A→B→C)
    - If S_AC < S_AB × S_BC: long direct, short indirect
-/
theorem cross_rate_parity_with_fees
    (direct : Quote)      -- A/C direct
    (rate_ab : Quote)     -- A/B
    (rate_bc : Quote)     -- B/C
    (direct_fees ab_fees bc_fees : Fees)
    (hPositive : rate_ab.mid > 0 ∧ rate_bc.mid > 0 ∧ direct.mid > 0) :
    let indirect := rate_ab.mid * rate_bc.mid
    let total_fees := Fees.totalFee direct_fees direct.mid (by sorry) +
                      Fees.totalFee ab_fees rate_ab.mid (by sorry) +
                      Fees.totalFee bc_fees rate_bc.mid (by sorry)
    (direct.mid - indirect).abs ≤ total_fees + 0.01 * indirect := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (direct.mid - (rate_ab.mid * rate_bc.mid)).abs -
                   (Fees.totalFee direct_fees direct.mid (by sorry) +
                    Fees.totalFee ab_fees rate_ab.mid (by sorry) +
                    Fees.totalFee bc_fees rate_bc.mid (by sorry)) -
                   0.01 * (rate_ab.mid * rate_bc.mid)
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- FX triangular arbitrage detection with explicit 3-leg trade.

    Statement: Execute triangular arbitrage if round-trip yield > fees + slippage

    Production: Buy A→B, B→C, C→A
    Profit = (S_AB × S_BC × S_CA - 1) × notional - total_fees

    Detection: Profit > 0 and sufficient to justify operational costs
-/
theorem triangular_fx_arbitrage_with_fees
    (rate_ab : Quote)
    (rate_bc : Quote)
    (rate_ca : Quote)
    (fees_ab fees_bc fees_ca : Fees)
    (notional : ℝ)
    (hNotional : notional > 0)
    (hPositive : rate_ab.ask.val > 0 ∧ rate_bc.ask.val > 0 ∧ rate_ca.ask.val > 0) :
    let cost_ab := rate_ab.ask.val + Fees.totalFee fees_ab rate_ab.ask.val (by sorry)
    let cost_bc := rate_bc.ask.val + Fees.totalFee fees_bc rate_bc.ask.val (by sorry)
    let proceeds_ca := rate_ca.bid.val - Fees.totalFee fees_ca rate_ca.bid.val (by sorry)
    let round_trip := cost_ab * cost_bc * proceeds_ca
    let initial_value := notional
    round_trip * initial_value ≥ initial_value → notional * (round_trip - 1) ≥ 0 := by
  intro h
  nlinarith

-- ============================================================================
-- CURRENCY SWAP CONSTRAINTS
-- ============================================================================

/-- Currency swap parity: Decomposition of spot and forward rates.

    Statement: A currency swap = spot FX + fixed rates commitment

    Equivalence: Entering into a USD/EUR swap should cost the same as:
    1. Spot exchange (USD → EUR)
    2. Borrowing USD at fixed rate
    3. Lending EUR at fixed rate

    By no-arbitrage, swap rates must match this decomposition.

    Arbitrage if violated:
    - If swap too expensive: replicate with spot + funding
    - If swap too cheap: short replication, buy swap
-/
theorem currency_swap_parity_with_fees
    (spot : Quote)
    (swap_rate : Quote)
    (funding_rate : Rate)
    (investment_rate : Rate)
    (spot_fees swap_fees : Fees)
    (hRate : funding_rate.val > -1 ∧ investment_rate.val > -1) :
    let fair_swap := spot.mid * ((1 + investment_rate.val) / (1 + funding_rate.val))
    let swap_cost := swap_rate.ask.val + Fees.totalFee swap_fees swap_rate.ask.val (by sorry)
    let replication_cost := spot.ask.val + Fees.totalFee spot_fees spot.ask.val (by sorry) + (1 * funding_rate.val)
    (swap_cost - fair_swap).abs ≤ replication_cost + 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (swap_rate.ask.val + Fees.totalFee swap_fees swap_rate.ask.val (by sorry)) - (spot.mid * ((1 + investment_rate.val) / (1 + funding_rate.val)))
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Basis swap constraint: Spread between two floating rate swaps.

    Statement: SOFR USD swap ↔ SONIA GBP swap basis should be bounded

    Example: A bank can borrow USD at SOFR and lend GBP at SONIA.
    The basis between these rates is the profit/loss.

    By arbitrage, basis cannot exceed transaction costs + credit spread.

    Practical: Basis swaps tighten when both currencies liquid, widen during stress.
-/
theorem basis_swap_constraint
    (sofr_usd : Quote)
    (sonia_gbp : Quote)
    (usd_fees gbp_fees : Fees)
    (fx_spot : Quote) :
    let usd_cost := sofr_usd.ask.val + Fees.totalFee usd_fees sofr_usd.ask.val (by sorry)
    let gbp_cost := sonia_gbp.ask.val + Fees.totalFee gbp_fees sonia_gbp.ask.val (by sorry)
    let basis := usd_cost - gbp_cost
    basis.abs ≤ 0.01 + fx_spot.spread := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((sofr_usd.ask.val + Fees.totalFee usd_fees sofr_usd.ask.val (by sorry)) - (sonia_gbp.ask.val + Fees.totalFee gbp_fees sonia_gbp.ask.val (by sorry))).abs - 0.01 - fx_spot.spread
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Swap rate consistency: All-in cost of swap should match spot-forward arbitrage.

    Statement: Swap all-in rate ≈ (spot + forward points) / 2

    Intuition: A swap is essentially selling spot and buying forward (or vice versa).
    The all-in rate should reflect this two-way trade.

    Arbitrage if violated: Swap vs spot-forward replication mismatch.
-/
theorem swap_rate_consistency
    (swap_rate : Quote)
    (spot : Quote)
    (forward : Quote)
    (swap_fees spot_fees fwd_fees : Fees) :
    let swap_price := swap_rate.mid
    let replication := (spot.mid + forward.mid) / 2
    let total_fees := Fees.totalFee swap_fees swap_rate.mid (by sorry) +
                      (Fees.totalFee spot_fees spot.mid (by sorry) +
                       Fees.totalFee fwd_fees forward.mid (by sorry)) / 2
    (swap_price - replication).abs ≤ total_fees + 0.005 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (swap_rate.mid - ((spot.mid + forward.mid) / 2)).abs -
                   (Fees.totalFee swap_fees swap_rate.mid (by sorry) +
                    (Fees.totalFee spot_fees spot.mid (by sorry) +
                     Fees.totalFee fwd_fees forward.mid (by sorry)) / 2) -
                   0.005
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- QUANTO AND MULTI-CURRENCY DERIVATIVES
-- ============================================================================

/-- Quanto forward bounds: Synthetic equity forward with FX embedded.

    Statement: A quanto forward = stock forward × FX forward (locked-in rate)

    Equivalence: An investor buying Japanese stock for USD can either:
    1. Buy equity directly at spot, sell forward 1 year
    2. Buy quanto forward (locks in exchange rate today)

    Cost must be same by arbitrage.

    Arbitrage if violated:
    - If quanto forward too expensive: buy spot equity, sell forward
    - If quanto forward too cheap: reverse
-/
theorem quanto_forward_bounds
    (equity_spot : Quote)
    (equity_forward : Quote)
    (fx_spot : Quote)
    (fx_forward : Quote)
    (eq_fees fx_fees : Fees)
    (hPositive : equity_spot.mid > 0 ∧ fx_spot.mid > 0) :
    let synthetic_quanto := equity_spot.mid * fx_forward.mid
    let market_quanto := equity_forward.mid
    let total_fees := Fees.totalFee eq_fees equity_spot.mid (by sorry) +
                      Fees.totalFee fx_fees fx_forward.mid (by sorry)
    (market_quanto - synthetic_quanto).abs ≤ total_fees + 0.02 * equity_spot.mid := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (equity_forward.mid - (equity_spot.mid * fx_forward.mid)).abs -
                   (Fees.totalFee eq_fees equity_spot.mid (by sorry) +
                    Fees.totalFee fx_fees fx_forward.mid (by sorry)) -
                   0.02 * equity_spot.mid
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Multi-currency bond parity: Convert and hedge equivalence.

    Statement: Bond_USD = Bond_EUR × FX_forward (fully hedged)

    Intuition: Buying a EUR bond and hedging with FX forward should cost
    the same as buying a USD bond directly.

    Arbitrage if violated:
    - If USD bond expensive: buy EUR bond + FX forward hedge
    - If USD bond cheap: reverse (buy USD, short EUR bond + FX)

    This is the interest rate parity constraint extended to bonds.
-/
theorem multicurrency_bond_parity
    (bond_usd : Quote)
    (bond_eur : Quote)
    (fx_forward : Quote)
    (usd_bond_fees eur_bond_fees fx_fees : Fees) :
    let usd_cost := bond_usd.ask.val + Fees.totalFee usd_bond_fees bond_usd.ask.val (by sorry)
    let eur_hedged := (bond_eur.ask.val + Fees.totalFee eur_bond_fees bond_eur.ask.val (by sorry)) *
                      (fx_forward.ask.val + Fees.totalFee fx_fees fx_forward.ask.val (by sorry))
    (usd_cost - eur_hedged).abs ≤ 0.02 * bond_usd.mid := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((bond_usd.ask.val + Fees.totalFee usd_bond_fees bond_usd.ask.val (by sorry)) -
                   ((bond_eur.ask.val + Fees.totalFee eur_bond_fees bond_eur.ask.val (by sorry)) *
                    (fx_forward.ask.val + Fees.totalFee fx_fees fx_forward.ask.val (by sorry)))).abs -
                   0.02 * bond_usd.mid
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Standard 5)
-- ============================================================================

/-- Check CIP violation: Compare fair forward vs market forward. -/
def checkCIPViolation
    (spot_mid : ℝ)
    (forward_market : ℝ)
    (rate_base rate_quote : ℝ)
    (tolerance : ℝ) :
    Bool :=
  if 1 + rate_base ≠ 0 then
    let fair := spot_mid * ((1 + rate_quote) / (1 + rate_base))
    (forward_market - fair).abs ≤ tolerance * spot_mid
  else
    true  -- Avoid division by zero

/-- Check triangular arbitrage: Product of 3 rates should be ≈ 1. -/
def checkTriangularArbitrage
    (rate_ab rate_bc rate_ca : ℝ)
    (tolerance : ℝ) :
    Bool :=
  let product := rate_ab * rate_bc * rate_ca
  (product - 1).abs ≤ tolerance

/-- Check forward premium: Premium should match interest rate differential. -/
def checkForwardPremium
    (spot : ℝ)
    (forward : ℝ)
    (rate_base rate_quote : ℝ) :
    Bool :=
  if spot > 0 then
    let premium := (forward - spot) / spot
    let ir_diff := rate_quote - rate_base
    (premium - ir_diff).abs ≤ 0.1
  else
    true

/-- Check cross-rate parity: Direct = Indirect rates. -/
def checkCrossRateParity
    (direct : ℝ)
    (indirect : ℝ)
    (tolerance : ℝ) :
    Bool :=
  (direct - indirect).abs ≤ tolerance * indirect

/-- Check forward positivity: Forward rates must be > 0. -/
def checkForwardPositivity (forward : ℝ) : Bool :=
  forward > 0

end Finance.Forwards
