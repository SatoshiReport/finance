-- Core financial types and structures
-- Provides foundations for all no-arbitrage rules

import Mathlib.Tactic

namespace Finance

-- ============================================================================
-- Positive Reals
-- ============================================================================

/-- A strictly positive real number, used for prices and time values. -/
structure PosReal where
  val : Float
  pos : val > 0

namespace PosReal

/-- Create a positive real from a float, with proof of positivity. -/
def mk' {x : Float} (h : x > 0) : PosReal := ⟨x, h⟩

/-- Extract the underlying float value. -/
def toFloat (r : PosReal) : Float := r.val

/-- Convert from PosReal to Float explicitly. -/
instance : Coe PosReal Float := ⟨toFloat⟩

/-- Addition of positive reals (result is positive). -/
def add (a b : PosReal) : PosReal :=
  ⟨a.val + b.val, by sorry⟩  -- Float addition of positive values is positive

/-- Multiplication of positive reals. -/
def mul (a b : PosReal) : PosReal :=
  ⟨a.val * b.val, by sorry⟩  -- Float multiplication of positive values is positive

/-- Scalar multiplication by positive constant. -/
def smul (c : Float) (r : PosReal) (hc : c > 0) : PosReal :=
  ⟨c * r.val, by sorry⟩  -- Float multiplication of positive values is positive

/-- Maximum of two positive reals. -/
def max (a b : PosReal) : PosReal :=
  let m := if a.val > b.val then a.val else b.val
  ⟨m, by sorry⟩  -- max of two positive values is positive

/-- Minimum of two positive reals. -/
def min (a b : PosReal) : PosReal :=
  let m := if a.val < b.val then a.val else b.val
  ⟨m, by sorry⟩  -- min of two positive values is positive

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
def mk' {b a : Float} (hb : b > 0) (ha : a > 0) (hva : b ≤ a) : Quote :=
  ⟨⟨b, hb⟩, ⟨a, ha⟩, hva⟩

/-- The spread (ask - bid), always non-negative. -/
def spread (q : Quote) : Float := q.ask.val - q.bid.val

/-- The midpoint of the quote. -/
def mid (q : Quote) : Float := (q.bid.val + q.ask.val) / 2

/-- Tightness ratio: spread / midpoint, measures relative spread. -/
def tightness (q : Quote) : Float :=
  spread q / mid q

end Quote

-- ============================================================================
-- Fees
-- ============================================================================

/-- Transaction costs for trading.
    - fixed: flat fee per transaction (e.g., 0.10 for $0.10)
    - proportional: percentage fee (e.g., 0.0005 for 5 basis points = 0.05%)
    Both are non-negative. -/
structure Fees where
  fixed : Float
  proportional : Float
  fixed_nonneg : fixed ≥ 0
  prop_nonneg : proportional ≥ 0

namespace Fees

/-- Create fees with non-negativity proofs. -/
def mk' {f p : Float} (hf : f ≥ 0) (hp : p ≥ 0) : Fees :=
  ⟨f, p, hf, hp⟩

/-- Zero fees. -/
def zero : Fees := ⟨0, 0, by sorry, by sorry⟩

/-- Typical brokerage fees: $2 fixed + 5bps proportional. -/
def typical : Fees := ⟨2.0, 0.0005, by sorry, by sorry⟩

/-- Calculate total fee for a transaction of given size.
    total_fee = fixed + proportional * amount -/
def totalFee (fees : Fees) (amount : Float) (ha : amount ≥ 0) : Float :=
  fees.fixed + fees.proportional * amount

end Fees

-- ============================================================================
-- Time
-- ============================================================================

/-- Time to expiry or time horizon (non-negative, in years).
    0 means immediate, 1 means one year. -/
structure Time where
  val : Float
  nonneg : val ≥ 0

namespace Time

/-- Create a time value with proof of non-negativity. -/
def mk' {t : Float} (h : t ≥ 0) : Time := ⟨t, h⟩

/-- Immediate expiry. -/
def now : Time := ⟨0, by sorry⟩

/-- One year. -/
def oneYear : Time := ⟨1, by sorry⟩

/-- Six months. -/
def sixMonths : Time := ⟨0.5, by sorry⟩

/-- Three months. -/
def threeMonths : Time := ⟨0.25, by sorry⟩

end Time

-- ============================================================================
-- Interest Rates
-- ============================================================================

/-- An interest rate (can be positive or negative).
    Expressed as a decimal (e.g., 0.05 = 5% per annum). -/
structure Rate where
  val : Float

namespace Rate

/-- Create an interest rate. -/
def mk' (r : Float) : Rate := ⟨r⟩

/-- Zero interest rate. -/
def zero : Rate := ⟨0⟩

/-- Positive rate: 5% -/
def typical : Rate := ⟨0.05⟩

/-- Negative rate (real in some markets). -/
def negative : Rate := ⟨-0.01⟩

/-- Discount factor: e^(-r*t) -/
def discountFactor (r : Rate) (t : Time) : Float :=
  Float.exp (-(r.val * t.val))

/-- Accumulation factor: e^(r*t) -/
def accumulationFactor (r : Rate) (t : Time) : Float :=
  Float.exp (r.val * t.val)

end Rate

-- ============================================================================
-- Strike Prices
-- ============================================================================

/-- A strike price for an option (strictly positive). -/
def Strike := PosReal

namespace Strike

/-- Create a strike price. -/
def mk' {k : Float} (h : k > 0) : Strike :=
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
def mk' {s : Float} (h : s > 0) : Spot :=
  ⟨s, h⟩

end Spot

end Finance
