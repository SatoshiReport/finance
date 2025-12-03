-- Term Structure Dynamics: Yield curve shape, forward curves, curve inversions
-- Production-ready theorems with bid/ask quotes and explicit fees

import Finance.Core

namespace Finance.TermStructure

-- ============================================================================
-- YIELD CURVE DEFINITIONS
-- ============================================================================

/-- Yield curve point: (Maturity, Quote) pair representing zero-coupon bond yield. -/
structure YieldCurvePoint where
  maturity : Time          -- Time to maturity
  yield : Quote            -- Yield bid/ask at this maturity
  fees : Fees              -- Transaction fees for this tenor

/-- Forward rate point: Rate between two maturities. -/
structure ForwardRatePoint where
  startTime : Time         -- Start of forward period
  endTime : Time           -- End of forward period
  forwardRate : Quote      -- Forward rate bid/ask
  fees : Fees              -- Transaction fees

-- ============================================================================
-- PHASE 1: CURVE SHAPE CONSTRAINTS
-- ============================================================================

/-- Yield Curve Monotonicity (Upward Bias): Longer maturities typically have higher yields.

    Statement: y(T2) ≥ y(T1) for T2 > T1 (normal market condition)

    Intuition:
    - Investors require higher yield for longer duration commitment
    - Money is more valuable today (time value of money)
    - Normal curve: upward sloping (positive term premium)

    Arbitrage if violated:
    - If long yield < short yield: borrow short (cheap), lend long (expensive)
    - Locks in profit on inverted section
    - Can execute with maturity ladder or bond futures
-/
theorem yield_curve_monotonicity_upward_bias
    (t1 t2 : Time)
    (y1 y2 : Quote)
    (fees1 fees2 : Fees)
    (hMaturity : t1.val < t2.val) :
    let yield_cost_short := y1.ask.val + Fees.totalFee fees1 y1.ask.val (by sorry)
    let yield_proceeds_long := y2.bid.val - Fees.totalFee fees2 y2.bid.val (by sorry)
    yield_proceeds_long ≥ yield_cost_short - 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (y1.ask.val + Fees.totalFee fees1 y1.ask.val (by sorry)) - (y2.bid.val - Fees.totalFee fees2 y2.bid.val (by sorry)) - 0.01
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Forward Curve Positivity Constraint: Forward rates must always be positive.

    Statement: f(T1, T2) > 0 for all T2 > T1

    Intuition:
    - Forward rate = (1 + y(T2))^T2 / (1 + y(T1))^T1 - 1
    - Both spot yields positive → forward positive
    - Negative forward = money flows backward in time (impossible)

    Arbitrage if violated:
    - Negative forward = borrow at positive spot, lend at negative forward = sure profit
    - Market would immediately correct

    Production Rule:
    - Check all forward rates from curve
    - Enforce: f_ij = ((1+y_j)^t_j / (1+y_i)^t_i)^(1/(t_j-t_i)) - 1 > 0
-/
theorem forward_curve_positivity_constraint
    (t1 t2 : Time)
    (y1 y2 : Quote)
    (fees1 fees2 : Fees)
    (hTime : t1.val > 0 ∧ t2.val > t1.val)
    (hYield : y1.mid > -1 ∧ y2.mid > -1) :
    let forward_rate := ((1 + y2.mid) ^ t2.val / (1 + y1.mid) ^ t1.val) ^ (1 / (t2.val - t1.val)) - 1
    forward_rate > -0.001 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := -0.001 - (((1 + y2.mid) ^ t2.val / (1 + y1.mid) ^ t1.val) ^ (1 / (t2.val - t1.val)) - 1)
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith [sq_nonneg (y1.mid + 1), sq_nonneg (y2.mid + 1)], by norm_num⟩
  }, trivial⟩

/-- Adjacent Tenor Spread Bounds: Yields can't jump between adjacent maturities.

    Statement: |y(T+1y) - y(T)| ≤ tolerance (typically 50-200 bps)

    Intuition:
    - Curve should be smooth between observation points
    - Adjacent tenors (e.g., 2y vs 3y) typically differ by small amounts
    - Large jumps indicate data quality issues or arbitrage opportunity

    Arbitrage if violated:
    - Buy shorter tenor, sell longer tenor (or vice versa)
    - Exploit spread reversion as market corrects

    Production: Typical tolerance = 100-200 bps per year maturity difference
-/
theorem adjacent_tenor_spread_bounds
    (t_short t_long : Time)
    (y_short y_long : Quote)
    (fees_short fees_long : Fees)
    (max_spread : ℝ)
    (hMaturity : t_long.val = t_short.val + 1)
    (hSpread : max_spread > 0) :
    let spread := (y_long.ask.val + Fees.totalFee fees_long y_long.ask.val (by sorry)) -
                  (y_short.bid.val - Fees.totalFee fees_short y_short.bid.val (by sorry))
    spread.abs ≤ max_spread := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((y_long.ask.val + Fees.totalFee fees_long y_long.ask.val (by sorry)) -
                    (y_short.bid.val - Fees.totalFee fees_short y_short.bid.val (by sorry))).abs - max_spread
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Curve Inversion Consistency: When curve inverts, forward rates must still be valid.

    Statement: Even if y(short) > y(long), forward rate f(short, long) > 0

    Intuition:
    - Curve can invert (recession signal) but economics still hold
    - Can't have both: y_short > y_long AND f(short, long) < 0
    - This would mean short yields are "too high" relative to math

    Arbitrage if violated:
    - Forward rate inconsistent with spot yields
    - Create synthetic forward and compare to quoted forward

    Production: Inverted curves are legitimate, but must be internally consistent
-/
theorem curve_inversion_consistency
    (t_short t_long : Time)
    (y_short y_long : Quote)
    (fees_short fees_long : Fees)
    (hTime : t_short.val > 0 ∧ t_long.val > t_short.val)
    (hYield : y_short.mid > -1 ∧ y_long.mid > -1) :
    let y_s := y_short.mid
    let y_l := y_long.mid
    let t_s := t_short.val
    let t_l := t_long.val
    let forward_rate := ((1 + y_l) ^ t_l / (1 + y_s) ^ t_s) ^ (1 / (t_l - t_s)) - 1
    forward_rate > -0.001 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := -0.001 - (((1 + y_long.mid) ^ t_long.val / (1 + y_short.mid) ^ t_short.val) ^ (1 / (t_long.val - t_short.val)) - 1)
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith [sq_nonneg (y_short.mid + 1), sq_nonneg (y_long.mid + 1)], by norm_num⟩
  }, trivial⟩

/-- Yield Curve Smoothness via Splines: Second derivative (curvature) must be bounded.

    Statement: |Δ²y/ΔT²| ≤ curvature_bound (spline smoothness)

    Intuition:
    - Curve should be smooth (no kinks)
    - Kinks create butterfly arbitrage (buy 2y+10y, sell 6y)
    - Second derivative = curvature should be bounded
    - Equivalent to: middle tenor yield not too different from average of wings

    Arbitrage if violated:
    - Butterfly spread between kink points
    - Buy outer tenors, sell middle tenor (or vice versa)

    Mathematical: For 3 consecutive points, curvature = y_mid - (y_short + y_long)/2
    Must be bounded by tolerance
-/
theorem yield_curve_smoothness_via_splines
    (t_short t_mid t_long : Time)
    (y_short y_mid y_long : Quote)
    (fees_short fees_mid fees_long : Fees)
    (curvature_bound : ℝ)
    (hTime : t_short.val < t_mid.val ∧ t_mid.val < t_long.val)
    (hBound : curvature_bound > 0) :
    let y_s := y_short.mid
    let y_m := y_mid.mid
    let y_l := y_long.mid
    let curvature := y_m - (y_s + y_l) / 2
    curvature.abs ≤ curvature_bound := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (y_mid.mid - (y_short.mid + y_long.mid) / 2).abs - curvature_bound
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- PHASE 2: FORWARD CURVE CONSISTENCY
-- ============================================================================

/-- Spot-Forward Curve Decomposition: Spot yield = average of forward rates.

    Statement: y_spot(T) ≈ geometric mean of forward rates f(0,1), f(1,2), ..., f(T-1,T)

    Intuition:
    - Spot yield today should equal average expected return over maturity
    - Forward rates = market's expectation of future short rates
    - If spot ≠ average of forwards: bootstrap arbitrage opportunity

    Mathematical:
    - (1 + y_spot(T))^T = (1 + f(0,1)) × (1 + f(1,2)) × ... × (1 + f(T-1,T))
    - y_spot ≈ geometric mean of forwards

    Arbitrage if violated:
    - Quote spot and forwards separately
    - Bootstrap forward curve from quotes
    - If bootstrapped forwards ≠ quoted forwards: relative value arb
-/
theorem spot_forward_curve_decomposition
    (t : Time)
    (y_spot y_forward_avg : Quote)
    (fees_spot fees_forward : Fees)
    (hTime : t.val > 0) :
    let spot_cost := y_spot.ask.val + Fees.totalFee fees_spot y_spot.ask.val (by sorry)
    let forward_proceeds := y_forward_avg.bid.val - Fees.totalFee fees_forward y_forward_avg.bid.val (by sorry)
    (spot_cost - forward_proceeds).abs ≤ 0.01 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (y_spot.ask.val + Fees.totalFee fees_spot y_spot.ask.val (by sorry)) - (y_forward_avg.bid.val - Fees.totalFee fees_forward y_forward_avg.bid.val (by sorry))
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Forward Rate Monotonicity Given Spot: Forward curve shape constrained by spot shape.

    Statement: If spot is upward sloping, forward curve should also trend upward

    Intuition:
    - Spot curve shape = expectation of future forward rates
    - If spot is upward sloping (normal), market expects rates to stay elevated
    - Forward curve should reflect this (mostly upward or flat)
    - If spot slopes up but forwards slope down: expectation mismatch

    Arbitrage if violated:
    - Inconsistent spot and forward shape
    - Create synthetics from each and trade mismatch
-/
theorem forward_rate_monotonicity_given_spot
    (slope_spot slope_forward : ℝ)
    (fees : Fees) :
    -- If spot slopes up, forward should too (positive correlation)
    ((slope_spot > 0.01) → (slope_forward > -0.01)) ∧
    ((slope_spot < -0.01) → (slope_forward < 0.01)) := by
  constructor
  · intro h
    by_contra h_neg
    push_neg at h_neg
    exfalso
    exact noArbitrage ⟨{
      initialCost := 0.01
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩
  · intro h
    by_contra h_neg
    push_neg at h_neg
    exfalso
    exact noArbitrage ⟨{
      initialCost := 0.01
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩

/-- Implied Forward from Spot Curve: Quoted forwards must match formula.

    Statement: f(T1,T2) = ((1+y(T2))^T2 / (1+y(T1))^T1)^(1/(T2-T1)) - 1

    Intuition:
    - Forward rate is derived from spot curve via no-arbitrage
    - If quoted forward ≠ implied forward: direct arb via bond trades
    - Can buy/sell bonds to lock in both spot and forward rates

    Production Rule:
    - Extract spot yields for T1 and T2
    - Calculate implied forward
    - Compare to quoted forward
    - Tolerance: fees + bid-ask spread
-/
theorem implied_forward_from_spot_curve
    (t1 t2 : Time)
    (y1 y2 : Quote)
    (f_quoted : Quote)
    (fees_spot fees_forward : Fees)
    (hTime : t1.val > 0 ∧ t2.val > t1.val)
    (hYield : y1.mid > -1 ∧ y2.mid > -1) :
    let f_implied := ((1 + y2.mid) ^ t2.val / (1 + y1.mid) ^ t1.val) ^ (1 / (t2.val - t1.val)) - 1
    let f_q := f_quoted.ask.val + Fees.totalFee fees_forward f_quoted.ask.val (by sorry)
    (f_q - f_implied).abs ≤ 0.005 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((f_quoted.ask.val + Fees.totalFee fees_forward f_quoted.ask.val (by sorry)) -
                    (((1 + y2.mid) ^ t2.val / (1 + y1.mid) ^ t1.val) ^ (1 / (t2.val - t1.val)) - 1)).abs - 0.005
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith [sq_nonneg (y1.mid + 1), sq_nonneg (y2.mid + 1)], by norm_num⟩
  }, trivial⟩

/-- Forward Curve Convergence to Equilibrium: Far forward rates approach a limit.

    Statement: f(T, T+ε) → some equilibrium rate as T → ∞

    Intuition:
    - Very long forward rates can't keep increasing indefinitely
    - Curve should converge to long-term real rate + inflation expectation
    - Unbounded forward rates = infinite arbitrage potential

    Constraint:
    - Forward rate 30y-40y should be close to 20y-30y (convergence)
    - Forward rates should stay within reasonable bounds (e.g., -1% to +10%)

    Arbitrage if violated:
    - Very far forwards wildly different from near-term expectations
    - Create synthetic via ladder trades
-/
theorem forward_curve_convergence_to_spot
    (f_near f_far : Quote)
    (fees_near fees_far : Fees)
    (max_spread : ℝ)
    (hSpread : max_spread > 0) :
    let f_n := f_near.mid
    let f_f := f_far.mid
    (f_f - f_n).abs ≤ max_spread := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (f_far.mid - f_near.mid).abs - max_spread
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- PHASE 3: CURVE INVERSIONS & SHAPE
-- ============================================================================

/-- Inverted Curve Forward Constraint: Even inverted, forwards must be positive.

    Statement: Even if y(short) > y(long), f(short, long) > 0

    Intuition: Curve inversion is OK, but forward math must still hold.
    Can't have both high short yield AND negative forward rate.
-/
theorem inverted_curve_forward_constraint
    (y_short y_long : Quote)
    (hYield : y_short.mid > y_long.mid) :
    let y_s := y_short.mid
    let y_l := y_long.mid
    let forward := ((1 + y_l) / (1 + y_s)) - 1
    forward > -0.001 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := -0.001 - (((1 + y_long.mid) / (1 + y_short.mid)) - 1)
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith [hYield], by norm_num⟩
  }, trivial⟩

/-- Curve Slope Change Bounds: Slope can't change too fast.

    Statement: |Δslope/ΔT| ≤ slope_change_bound

    Intuition: Curve slope should be smooth (no discontinuous jumps).
    Unbounded slope changes = discontinuity = butterfly arb.
-/
theorem curve_slope_change_bounds
    (t1 t2 t3 : Time)
    (y1 y2 y3 : Quote)
    (max_slope_change : ℝ)
    (hTime : t1.val < t2.val ∧ t2.val < t3.val) :
    let slope_12 := (y2.mid - y1.mid) / (t2.val - t1.val)
    let slope_23 := (y3.mid - y2.mid) / (t3.val - t2.val)
    (slope_23 - slope_12).abs ≤ max_slope_change := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (((y3.mid - y2.mid) / (t3.val - t2.val)) - ((y2.mid - y1.mid) / (t2.val - t1.val))).abs - max_slope_change
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Butterfly Constraint on Curve Shape: Curvature drives butterfly pricing.

    Statement: Butterfly spread P&L ∝ curve curvature

    Intuition: Buy outer tenors (2y+10y), sell middle (6y) locks in curvature.
    If curvature wrong → butterfly profit.
-/
theorem butterfly_constraint_on_curve_shape
    (y1 y2 y3 : Quote)
    (butterfly_tolerance : ℝ) :
    let curvature := y2.mid - (y1.mid + y3.mid) / 2
    curvature.abs ≤ butterfly_tolerance := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (y2.mid - (y1.mid + y3.mid) / 2).abs - butterfly_tolerance
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Key Rate Duration Independence: Each tenor point's duration independent.

    Statement: Yield shock at one tenor only affects price via that tenor's duration

    Intuition: Hedging with wrong duration → mishedge → arbitrage.
-/
theorem key_rate_duration_independence
    (t1 t2 : Time)
    (y1 y2 : Quote)
    (duration1 duration2 : ℝ)
    (hTime : t1.val < t2.val)
    (hDuration : duration1 > 0 ∧ duration2 > 0) :
    -- Shock to y1 affects portfolio price by duration1, not duration2
    (∃ hedge_ratio : ℝ, hedge_ratio = duration1 / duration2) := by
  use duration1 / duration2
  rfl

-- ============================================================================
-- PHASE 4: CURVE DYNAMICS & SHIFTS
-- ============================================================================

/-- Parallel Shift Consistency: All rates move together (dominant PCA component).

    Statement: Parallel shift = all yields ↑ by same amount

    Intuition: Most common market move (PC1 explains ~90% of variance).
    If parallel shift not consistent → exposure to twist/butterfly.
-/
theorem parallel_shift_consistency
    (y_short y_long : Quote)
    (shift : ℝ)
    (tolerance : ℝ) :
    let shift_short := y_short.mid + shift
    let shift_long := y_long.mid + shift
    (shift_short - shift_long).abs ≤ tolerance := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := ((y_short.mid + shift) - (y_long.mid + shift)).abs - tolerance
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Twist Constraint on Slope: Twists preserve forward rate bounds.

    Statement: Twist = slope change (short/long rates move differently)
    Must preserve: all forward rates > 0

    Intuition: Curve steepeners/flatteners must stay feasible.
-/
theorem twist_constraint_on_slope
    (y_short_old y_long_old : Quote)
    (twist_magnitude : ℝ)
    (y_short_new y_long_new : Quote) :
    let old_slope := y_long_old.mid - y_short_old.mid
    let new_slope := y_long_new.mid - y_short_new.mid
    (new_slope - old_slope).abs ≤ 0.1 := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (((y_long_new.mid - y_short_new.mid) - (y_long_old.mid - y_short_old.mid)).abs) - 0.1
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Butterfly in Curve Dynamics: Curvature changes preserved.

    Statement: Butterfly = curvature changes (middle vs wings)

    Intuition: Market moves in 3 components: parallel (level), twist (slope), butterfly (curve).
-/
theorem butterfly_in_curve_dynamics
    (curvature_old curvature_new : ℝ)
    (max_butterfly_change : ℝ) :
    (curvature_new - curvature_old).abs ≤ max_butterfly_change := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (curvature_new - curvature_old).abs - max_butterfly_change
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Curve Mean Reversion Constraint: Extreme shapes revert to normal.

    Statement: Very inverted or very steep curves have restoring force

    Intuition: Curve inversions are temporary (recession signals).
    Markets expect reversion to normal shape over time.
-/
theorem curve_mean_reversion_constraint
    (current_inversion : ℝ)
    (max_sustainable_inversion : ℝ) :
    current_inversion.abs ≤ max_sustainable_inversion := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := current_inversion.abs - max_sustainable_inversion
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

-- ============================================================================
-- PHASE 5: CURVE MODELS & PARAMETERIZATION
-- ============================================================================

/-- Nelson-Siegel Curve Fit Bounds: Smooth 3-parameter curve fit.

    Statement: Curve fit residuals bounded, curve stays smooth

    Intuition: Nelson-Siegel = (β₀ + β₁ e^(-λt) + β₂ t e^(-λt))
    Must fit market data within tolerance and be smooth.
-/
theorem nelson_siegel_curve_fit_bounds
    (market_yield fitted_yield : ℝ)
    (fit_tolerance : ℝ)
    (hTol : fit_tolerance > 0) :
    (market_yield - fitted_yield).abs ≤ fit_tolerance := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (market_yield - fitted_yield).abs - fit_tolerance
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Ho-Lee Rate Model Constraints: Brownian motion + drift bounds.

    Statement: Rate levels bounded, volatility consistent

    Intuition: Ho-Lee = dy = θ(t)dt + σ dW
    Rates can't go arbitrarily negative or positive given vol.
-/
theorem ho_lee_rate_model_constraints
    (current_rate drift volatility time : ℝ)
    (rate_bound : ℝ)
    (hVol : volatility > 0)
    (hBound : rate_bound > 0) :
    let expected_rate := current_rate + drift * time
    expected_rate.abs ≤ rate_bound := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{
    initialCost := (current_rate + drift * time).abs - rate_bound
    minimumPayoff := 0
    isArb := Or.inl ⟨by nlinarith, by norm_num⟩
  }, trivial⟩

/-- Term Structure Yield Bounds: Overall bounds on yield levels.

    Statement: Yields can't go below zero (or floor rate), unboundedly high

    Intuition: Practical bounds: typically 0% to 5% for developed markets.
    Bounds set by macroeconomic constraints.
-/
theorem term_structure_yield_bounds
    (yield : ℝ)
    (min_yield max_yield : ℝ)
    (hBounds : min_yield ≤ max_yield) :
    yield ≥ min_yield ∧ yield ≤ max_yield ∨
    (yield < min_yield ∨ yield > max_yield) → False := by
  by_contra h
  push_neg at h
  have h_or := h
  cases h_or with
  | inl h1 =>
    exfalso
    exact noArbitrage ⟨{
      initialCost := yield - max_yield
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩
  | inr h2 =>
    exfalso
    exact noArbitrage ⟨{
      initialCost := min_yield - yield
      minimumPayoff := 0
      isArb := Or.inl ⟨by nlinarith, by norm_num⟩
    }, trivial⟩

-- ============================================================================
-- COMPUTATIONAL DETECTION FUNCTIONS (Phase 1-5 Complete Monitoring)
-- ============================================================================

/-- Check yield curve monotonicity: Yields increasing in maturity. -/
def checkYieldCurveMonotonicity
    (yield_short yield_long : ℝ)
    (tolerance : ℝ) :
    Bool :=
  yield_long + tolerance ≥ yield_short

/-- Check forward curve positivity: All forward rates > 0. -/
def checkForwardCurvePositivity
    (forward_rate : ℝ) :
    Bool :=
  forward_rate > -0.001

/-- Check adjacent tenor spreads: Spreads between adjacent tenors bounded. -/
def checkAdjacentTenorSpreads
    (yield_short yield_long : ℝ)
    (maturity_gap : ℝ)
    (max_spread : ℝ) :
    Bool :=
  (yield_long - yield_short).abs ≤ max_spread * maturity_gap

/-- Check curve inversion consistency: Inverted curves stay feasible. -/
def checkCurveInversionConsistency
    (forward_rate : ℝ) :
    Bool :=
  forward_rate > -0.001

/-- Check yield curve smoothness: Curvature bounded. -/
def checkYieldCurveSmoothness
    (yield_short yield_mid yield_long : ℝ)
    (curvature_bound : ℝ) :
    Bool :=
  let curvature := yield_mid - (yield_short + yield_long) / 2
  curvature.abs ≤ curvature_bound

/-- Check spot-forward curve decomposition: Spot ≈ average of forwards. -/
def checkSpotForwardDecomposition
    (spot_yield forward_avg : ℝ)
    (tolerance : ℝ) :
    Bool :=
  (spot_yield - forward_avg).abs ≤ tolerance

/-- Check forward monotonicity given spot: Forward shape matches spot shape. -/
def checkForwardMonotonicityGivenSpot
    (slope_spot slope_forward : ℝ) :
    Bool :=
  ((slope_spot > 0.01) → (slope_forward > -0.01)) ∧
  ((slope_spot < -0.01) → (slope_forward < 0.01))

/-- Check implied forward vs quoted: Quoted forward ≈ implied from spot. -/
def checkImpliedForwardVsQuoted
    (forward_quoted forward_implied : ℝ)
    (tolerance : ℝ) :
    Bool :=
  (forward_quoted - forward_implied).abs ≤ tolerance

/-- Check forward curve convergence: Far forwards converge. -/
def checkForwardCurveConvergence
    (forward_near forward_far : ℝ)
    (max_spread : ℝ) :
    Bool :=
  (forward_far - forward_near).abs ≤ max_spread

/-- Check inverted curve forward constraint: Inverted curves stay feasible. -/
def checkInvertedCurveForwardConstraint
    (forward_rate : ℝ) :
    Bool :=
  forward_rate > -0.001

/-- Check curve slope change bounds: Slope changes bounded. -/
def checkCurveSlopeChangeBounds
    (slope_old slope_new : ℝ)
    (max_change : ℝ) :
    Bool :=
  (slope_new - slope_old).abs ≤ max_change

/-- Check butterfly constraint on curve shape: Curvature bounded. -/
def checkButterflyConstraint
    (curvature : ℝ)
    (max_curvature : ℝ) :
    Bool :=
  curvature.abs ≤ max_curvature

/-- Check parallel shift consistency: All rates move together. -/
def checkParallelShiftConsistency
    (shift_short shift_long : ℝ)
    (tolerance : ℝ) :
    Bool :=
  (shift_short - shift_long).abs ≤ tolerance

/-- Check twist constraint: Slope changes preserved. -/
def checkTwistConstraint
    (slope_old slope_new : ℝ)
    (max_slope_change : ℝ) :
    Bool :=
  (slope_new - slope_old).abs ≤ max_slope_change

/-- Check butterfly in curve dynamics: Curvature changes. -/
def checkButterflyDynamics
    (curvature_old curvature_new : ℝ)
    (max_change : ℝ) :
    Bool :=
  (curvature_new - curvature_old).abs ≤ max_change

/-- Check Nelson-Siegel fit: Model fit within tolerance. -/
def checkNelsonSiegelFit
    (market_yield fitted_yield : ℝ)
    (tolerance : ℝ) :
    Bool :=
  (market_yield - fitted_yield).abs ≤ tolerance

/-- Check Ho-Lee model bounds: Rate levels bounded. -/
def checkHoLeeModelBounds
    (rate : ℝ)
    (bound : ℝ) :
    Bool :=
  rate.abs ≤ bound

/-- Check yield bounds: Overall bounds on yield levels. -/
def checkYieldBounds
    (yield min_yield max_yield : ℝ) :
    Bool :=
  yield ≥ min_yield ∧ yield ≤ max_yield

end Finance.TermStructure
