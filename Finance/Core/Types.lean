-- Core financial types and structures
-- Provides foundations for all no-arbitrage rules

import Mathlib.Tactic
import Mathlib.Analysis.SpecialFunctions.ExpDeriv
import Mathlib.Data.Real.Basic

namespace Finance

-- ============================================================================
-- Positive Reals
-- ============================================================================

/-- A strictly positive real number, used for prices and time values. -/
structure PosReal where
  val : ℝ
  pos : val > 0

namespace PosReal

/-- Create a positive real from a real, with proof of positivity. -/
def mk' {x : ℝ} (h : x > 0) : PosReal := ⟨x, h⟩

/-- Extract the underlying real value. -/
def toFloat (r : PosReal) : ℝ := r.val


/-- Addition of positive reals (result is positive). -/
def add (a b : PosReal) : PosReal :=
  ⟨a.val + b.val, by sorry⟩

/-- Multiplication of positive reals. -/
def mul (a b : PosReal) : PosReal :=
  ⟨a.val * b.val, by sorry⟩

/-- Scalar multiplication by positive constant. -/
def smul (c : ℝ) (r : PosReal) (hc : c > 0) : PosReal :=
  ⟨c * r.val, by sorry⟩

/-- Maximum of two positive reals. -/
noncomputable def max (a b : PosReal) : PosReal :=
  let m := if a.val > b.val then a.val else b.val
  ⟨m, by sorry⟩

/-- Minimum of two positive reals. -/
noncomputable def min (a b : PosReal) : PosReal :=
  let m := if a.val < b.val then a.val else b.val
  ⟨m, by sorry⟩

end PosReal

-- ============================================================================
-- Quotes (Bid/Ask Prices)
-- ============================================================================

/-- A market quote with bid and ask prices.
    Invariant: bid ≤ ask (buying is more expensive than selling) -/
structure Quote where
  bid : PosReal
  ask : PosReal
  valid : bid.val ≤ ask.val

namespace Quote

/-- Create a quote from bid and ask with proof. -/
def mk' {b a : ℝ} (hb : b > 0) (ha : a > 0) (hva : b ≤ a) : Quote :=
  ⟨⟨b, hb⟩, ⟨a, ha⟩, hva⟩

/-- The spread (ask - bid), always non-negative. -/
def spread (q : Quote) : ℝ := q.ask.val - q.bid.val

/-- The midpoint of the quote. -/
noncomputable def mid (q : Quote) : ℝ := (q.bid.val + q.ask.val) / 2

/-- Tightness ratio: spread / midpoint, measures relative spread. -/
noncomputable def tightness (q : Quote) : ℝ :=
  spread q / mid q

/-- Extract bid price as real (preferred over direct .bid.val access). -/
def bidValue (q : Quote) : ℝ := q.bid.val

/-- Extract ask price as real (preferred over direct .ask.val access). -/
def askValue (q : Quote) : ℝ := q.ask.val

end Quote

-- ============================================================================
-- Fees
-- ============================================================================

/-- Transaction costs for trading.
    - fixed: flat fee per transaction (e.g., 0.10 for $0.10)
    - proportional: percentage fee (e.g., 0.0005 for 5 basis points = 0.05%)
    Both are non-negative. -/
structure Fees where
  fixed : ℝ
  proportional : ℝ
  fixed_nonneg : fixed ≥ 0
  prop_nonneg : proportional ≥ 0

namespace Fees

/-- Create fees with non-negativity proofs. -/
def mk' {f p : ℝ} (hf : f ≥ 0) (hp : p ≥ 0) : Fees :=
  ⟨f, p, hf, hp⟩

/-- Zero fees. -/
def zero : Fees := ⟨0, 0, by sorry, by sorry⟩

/-- Typical brokerage fees: $2 fixed + 5bps proportional. -/
def typical : Fees := ⟨2.0, 0.0005, by sorry, by sorry⟩

/-- Calculate total fee for a transaction of given size.
    total_fee = fixed + proportional * amount -/
def totalFee (fees : Fees) (amount : ℝ) (ha : amount ≥ 0) : ℝ :=
  fees.fixed + fees.proportional * amount

end Fees

-- ============================================================================
-- Time
-- ============================================================================

/-- Time to expiry or time horizon (non-negative, in years).
    0 means immediate, 1 means one year. -/
structure Time where
  val : ℝ
  nonneg : val ≥ 0

namespace Time

/-- Create a time value with proof of non-negativity. -/
def mk' {t : ℝ} (h : t ≥ 0) : Time := ⟨t, h⟩

/-- Immediate expiry. -/
def now : Time := ⟨0, by sorry⟩

/-- One year. -/
def oneYear : Time := ⟨1, by sorry⟩

/-- Six months. -/
def sixMonths : Time := ⟨0.5, by sorry⟩

/-- Three months. -/
def threeMonths : Time := ⟨0.25, by sorry⟩

/-- Extract time value as real (preferred over direct .val access). -/
def toFloat (t : Time) : ℝ := t.val

end Time

-- ============================================================================
-- Interest Rates
-- ============================================================================

/-- An interest rate (can be positive or negative).
    Expressed as a decimal (e.g., 0.05 = 5% per annum). -/
structure Rate where
  val : ℝ

namespace Rate

/-- Create an interest rate. -/
def mk' (r : ℝ) : Rate := ⟨r⟩

/-- Zero interest rate. -/
def zero : Rate := ⟨0⟩

/-- Positive rate: 5% -/
def typical : Rate := ⟨0.05⟩

/-- Negative rate (real in some markets). -/
def negative : Rate := ⟨-0.01⟩

/-- Discount factor: e^(-r*t) -/
noncomputable def discountFactor (r : Rate) (t : Time) : ℝ :=
  Real.exp (-(r.val * t.val))

/-- Accumulation factor: e^(r*t) -/
noncomputable def accumulationFactor (r : Rate) (t : Time) : ℝ :=
  Real.exp (r.val * t.val)

/-- Extract rate value as real (preferred over direct .val access). -/
def toFloat (r : Rate) : ℝ := r.val

end Rate

-- ============================================================================
-- Strike Prices
-- ============================================================================

/-- A strike price for an option (strictly positive). -/
def Strike := PosReal

namespace Strike

/-- Create a strike price. -/
def mk' {k : ℝ} (h : k > 0) : Strike :=
  ⟨k, h⟩

/-- Strike price of 100. -/
def atMoney : Strike := ⟨100.0, by sorry⟩

end Strike

-- ============================================================================
-- Spot Prices
-- ============================================================================

/-- The current spot price of an underlying asset. -/
def Spot := PosReal

namespace Spot

/-- Create a spot price. -/
def mk' {s : ℝ} (h : s > 0) : Spot :=
  ⟨s, h⟩

end Spot

end Finance
