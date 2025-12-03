# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Lean 4 project for financial mathematics and computational finance. It uses Lake (Lean's package manager) for building and testing.

- **Language**: Lean 4 (v4.26.0-rc2)
- **Main Library**: `Finance/` - core financial definitions and theorems
- **Demo**: `Demo/Main.lean` - executable demo program
- **Tests**: `Test/Basic.lean` - test runner
- **Dependency**: mathlib4 (master branch)

## Build and Development Commands

```bash
# Build the project
lake build

# Run tests
lake test

# Run the demo
lake exe demo

# Format code (if available in your Lean version)
lake fmt

# Run mathlib style lints
lake env lean --run .lake/packages/mathlib/scripts/lint-style.lean
```

## Process Cleanup (Critical for Development)

⚠️ **Lake and Lean processes can become CPU-hungry and hang in the background**. Always clean them up after runs:

```bash
# Kill all stray lake and lean processes immediately after builds complete
pkill -9 lake lean

# Use this especially after:
# - lake build (if it takes > 30 seconds after output)
# - lake test (always, even if tests pass)
# - Interrupted builds (Ctrl+C)
# - Multiple consecutive builds

# Check for stray processes:
ps aux | grep -E '(lake|lean)' | grep -v grep
```

**When to run cleanup:**
- ✅ **Always** after `lake test` completes (even if successful)
- ✅ **Always** after interrupted builds (Ctrl+C)
- ✅ **Always** between major build runs (Phases 1-5 conversions, full rebuilds)
- ✅ After running background builds with `run_in_background`
- ✅ Before long operations (proofs, multifile edits)

**Why this matters:**
- Lean and Lake spawn child processes that don't always exit cleanly
- Multiple stray processes can consume 80-90% CPU indefinitely
- This blocks the terminal and slows down subsequent builds
- `pkill -9` forcefully terminates (use -9 flag, not just -15)

**DO NOT use:** `pkill -9 -f lake` (too broad, may kill other processes). Use `pkill -9 lake lean` instead (targets specific names).

## CI Pipeline

The repository includes `ci.sh` which:
1. Runs `lake fmt` (if available)
2. Builds the project with `lake build`
3. Runs mathlib style lints (exits gracefully if lint-style fails)
4. Runs tests with `lake test`
5. Uses Claude to generate a commit message from the staged diff
6. Commits and pushes changes

**Important**: Do not modify CI pipeline code without explicit instructions.

**After CI runs:** Always run `pkill -9 lake lean` to clean up background processes before next iteration.

## Project Structure

- `lakefile.lean` - Lake project configuration defining the `finance` package
- `Finance.lean` - Library entry point (imports `Finance.Basic`)
- `Finance/Basic.lean` - Core library definitions
- `Demo/Main.lean` - Executable demo entry point
- `Test/Basic.lean` - Test runner entry point
- `lean-toolchain` - Specifies Lean compiler version

## Key Development Notes

- The project uses mathlib4 from the master branch for mathematical definitions and theorems
- Tests are run via `lake test` which executes `Test.Basic`
- The CI script requires both `claude` and `lake` CLIs to be installed
- Lean 4 uses 2-space indentation by default (enforced by `lake fmt`)

## Theorem Development Standards

All theorems must include these four elements to be production-ready:

### 1. **Bid/Ask Spreads**
- Use `Quote` type (with `bid` and `ask` fields) instead of scalar prices
- Example: `call : Quote` instead of `call_price : Float`
- Represents real market microstructure

### 2. **Explicit Fees**
- Deduct transaction costs via `Fees.totalFee` function
- Include fees for ALL instruments in the arbitrage strategy
- Formula: `actual_cost = quote.ask + Fees.totalFee fees quote.ask`
- Formula: `actual_proceeds = quote.bid - Fees.totalFee fees quote.bid`

### 3. **Analytical Formulas**
- All constraints must be **closed-form inequalities** (no numerical integration or iteration)
- Should be directly computable from inputs
- Example: `call_ask - put_bid - stock_bid + bond_ask ≤ threshold`
- Not: approximations requiring iterative solutions

### 4. **Real Market Ready**
- Theorem must be directly applicable to live market data
- Should detect actual arbitrage opportunities (not just theoretical bounds)
- Practical tolerance bounds (e.g., `0.01` for basis points) allowed for liquidity/slippage
- Example from `ArbitrageDetection` module shows the production pattern

### 5. **Dual Implementation: Analytical + Computational**
- Every theorem must have BOTH:
  - **Analytical**: The formal proof that the constraint holds mathematically
  - **Computational**: An executable checker that evaluates the constraint against real market data
- The computational version takes `MarketSnapshot` (or equivalent real data) and returns `Bool`
- The theorem mathematically guarantees: if the computational check returns `false`, an arbitrage exists
- Naming pattern: analytical proof in theorem module (e.g., `ArbitrageDetection.lean`), computational checker in parallel detection module (e.g., `*Detection.lean`)
- Both implementations must evaluate the same constraint; the computational version is the executable surface

## Architecture: ℝ for Theorems, Float for Computation

This project uses a **dual-type architecture** to bridge formal verification and practical computation:

### The Design Pattern

**Problem**: IEEE 754 floating-point arithmetic is complex and non-associative. Formally verifying Float theorems requires extensive axiomatization that contradicts practical computation.

**Solution**: Use **ℝ (Real numbers)** for all theorem proofs and **Float** for computational detection functions.

### Type Organization

```lean
-- Finance/Core/Types.lean uses ℝ internally:
structure PosReal where
  val : ℝ        -- Theorem domain
  pos : val > 0

structure Quote where
  bid : PosReal   -- Uses ℝ
  ask : PosReal   -- Uses ℝ
  valid : bid.val ≤ ask.val

structure Fees where
  fixed : ℝ       -- Theorem domain
  proportional : ℝ -- Theorem domain

structure Rate where
  val : ℝ         -- Theorem domain

structure Time where
  val : ℝ         -- Theorem domain
  nonneg : val ≥ 0
```

### Theorem vs Computational Separation

**Analytical Theorems** (use ℝ):
```lean
theorem putcall_parity_with_fees
    (call put stock bond : Quote)  -- Contain ℝ values
    (rate : Rate)                  -- val : ℝ
    (time : Time) :                -- val : ℝ
    call.ask - put.bid - stock.ask + bond.bid ≤ threshold := by
  by_contra h
  push_neg at h
  exfalso
  exact noArbitrage ⟨{...}, trivial⟩
```

**Computational Checkers** (convert ℝ → Float for practical evaluation):
```lean
def checkPutcallParity
    (call put stock bond : Quote)  -- Input types still use ℝ
    (fees : Fees) :
    Bool :=
  let call_cost := call.ask.val + Fees.totalFee fees call.ask.val  -- Extract ℝ values
  -- Use ℝ arithmetic for constraint checking
  call_cost - put.bid.val - stock.ask.val + bond.bid.val ≤ 0.01
```

### Why This Works

1. **Theorems are provable**: ℝ has full associativity, commutativity, and field properties in Lean/mathlib
2. **Computation is practical**: Constraints are written in terms that can be directly checked against real market data
3. **Type safety is maintained**: `Quote`, `Rate`, `Time`, `Fees` structures ensure correct data threading
4. **Conversion is explicit**: No hidden implicit coercions between ℝ and Float

### Key Properties

- **Discount factors**: Use `Real.exp` (defined in mathlib) instead of `Float.exp`
- **Addition and multiplication**: All work naturally on ℝ with mathlib support
- **Inequalities**: Linear constraints use `linarith`, nonlinear use `nlinarith`
- **Fees**: Always computed as `quote.ask + Fees.totalFee fees quote.ask` for costs

### Migration Path (Phase 1-5)

1. **Phase 1** (Foundation): Update `Finance/Core/Types.lean` to use ℝ in all structures ✅
2. **Phase 2** (Options): Convert option bounds theorems to use ℝ
3. **Phase 3** (Monotonicity/Convexity): Linear inequality proofs on ℝ
4. **Phase 4** (Complex Strategies): Multi-leg arbitrage theorems with ℝ
5. **Phase 5** (Advanced): Interest rates, bonds, volatility with Real.exp

## Theorem Proof Pattern

All theorems follow the universal no-arbitrage proof structure:

```lean
theorem name (params) : constraint := by
  by_contra h_contra          -- Assume constraint violated
  push_neg at h_contra        -- Negate the inequality
  exfalso                     -- Prove False
  exact noArbitrage ⟨{
    initialCost := ...,       -- Cash flow at trade initiation
    minimumPayoff := ...,     -- Guaranteed cash flow at exit
    isArb := Or.inl ⟨by norm_num, by linarith⟩  -- Proves arbitrage conditions
  }, trivial⟩
```

This pattern ensures:
- Every theorem is a contrapositive of a no-arbitrage principle
- Detection is automatic (if constraint violated → arbitrage exists)
- Proofs are constructive (builds explicit arbitrage strategy)

## From Theorems to Production Data

### The Problem: Theorems are Proofs, Not Computers

Analytical theorems prove constraints mathematically, but they return `Prop` (a mathematical truth), not `Bool` (an executable result). This creates a gap between proving a constraint and checking it against real market data.

A theorem like `putcall_parity_with_fees` proves:
```lean
theorem putcall_parity_with_fees (call put stock bond : Quote) ... :
    net_cost ≤ maturity_payoff := by ...
```

This is a mathematical proof. To use it in production against real quotes, you cannot directly call:
```lean
theorem putcall_parity_with_fees call_quote put_quote stock_quote bond_quote ...
  → net_cost ≤ maturity_payoff  -- Returns Prop, not Bool
```

### What You Need: Computational Wrappers

For every analytical theorem, create a computational function that evaluates its constraint:

```lean
/-- Evaluate put-call parity constraint with real market data -/
def checkPutCallParity (call put stock bond : Quote)
    (call_fees put_fees stock_fees bond_fees : Fees)
    (rate : Rate) (time : Time) :
    Bool := by
  let call_cost := call.ask + Fees.totalFee call_fees call.ask
  let put_proceeds := put.bid - Fees.totalFee put_fees put.bid
  let stock_proceeds := stock.bid - Fees.totalFee stock_fees stock.bid
  let bond_cost := bond.ask + Fees.totalFee bond_fees bond.ask
  let net_cost := call_cost - put_proceeds - stock_proceeds + bond_cost
  let maturity_payoff := (bond.ask * Float.exp (rate.val * time.val)) -
                         (stock.ask * Float.exp (rate.val * time.val))

  -- Evaluate constraint with real numbers → Bool
  return net_cost ≤ maturity_payoff
```

Then use the computational checker in production detection:

```lean
/-- Detect if put-call parity arbitrage exists -/
def detectPutCallArbitrage (call put stock bond : Quote) ... :
    Option ArbitrageOpportunity := by
  if checkPutCallParity call put stock bond ... then
    none  -- Constraint satisfied, no arbitrage
  else
    some ⟨...⟩  -- Constraint violated → arbitrage exists (guaranteed by theorem)
```

### Recommended Architecture

```lean
-- ANALYTICAL LAYER (Theorems)
theorem putcall_parity_with_fees ... : net_cost ≤ payoff := by ...

-- COMPUTATIONAL LAYER (Real Data Checks)
def checkPutCallParity ... : Bool := ...  -- Evaluates constraint
def checkGammaPositive ... : Bool := ...  -- Evaluates constraint
def checkBoxSpreadConstraint ... : Bool := ...  -- Evaluates constraint

-- PRODUCTION LAYER (Arbitrage Detection)
def detectArbitrages (snapshot : MarketSnapshot) : List Arbitrage := by
  let constraints := [
    checkPutCallParity snapshot.call snapshot.put ...,
    checkGammaPositive snapshot.smile,
    checkBoxSpreadConstraint snapshot.options
  ]
  -- If any check fails, an arbitrage exists (backed by formal theorems)
  constraints.filter (fun c => not c)
```

### Key Relationship

The theorem mathematically guarantees:
- **If** the computational check returns `false` for a constraint
- **Then** an arbitrage truly exists (no market maker can profitably quote)

This means: analytical correctness (theorem) + computational detection (checker) = production-ready arbitrage detection.

### Current Project Pattern

Every theorem module has a dual:
| Module | Purpose |
|--------|---------|
| `ArbitrageDetection.lean` | Analytical theorems proving constraints |
| `*Detection.lean` (paired) | Computational checkers evaluating constraints with real data |

This separation ensures theorems remain pure mathematical proofs while detection functions remain executable and practical.

## Common Lean 4 Pitfalls and Best Practices (Learned from Experience)

### Theorem Goal Structure: AVOID Let-Bindings in Goal Types

❌ **WRONG** - Causes type class synthesis failures:
```lean
theorem forward_swap_parity (forward_swap spot_swap : Quote) ... :
    let forward_cost := forward_swap.ask + ...  -- ❌ Let-binding in goal type
    let spot_proceeds := spot_swap.bid - ...
    let net := forward_cost - spot_proceeds
    net ≤ threshold := by ...
```

✅ **CORRECT** - Move let-bindings into proof body:
```lean
theorem forward_swap_parity (forward_swap spot_swap : Quote) ... :
    (forward_swap.ask + ... - (spot_swap.bid - ...)) ≤ threshold := by
  sorry  -- or complete proof here
```

**Why?** Lean 4's type checker struggles with let-bindings in goal positions. The type of a let-bound variable becomes part of the goal type, creating ambiguous type class instances.

**Better approach**: Use computational `def` functions for complex intermediate calculations instead of theorem let-bindings.

### PosReal/Float Type Class Issues

**Problem**: `PosReal` (for quote prices) and `Float` (for calculations) don't freely interact. Adding a `PosReal` to a `Float` fails.

```lean
let call_cost := call.ask + Fees.totalFee ...  -- ❌ Error: HAdd PosReal Float ?m.4
```

**Solution 1 - Use explicit coercion (preferred)**:
```lean
instance : Coe PosReal Float := ⟨PosReal.toFloat⟩
let call_cost := (call.ask : Float) + Fees.totalFee ...  -- ✅ Works via coercion
```

**Solution 2 - Use .val accessor explicitly**:
```lean
let call_cost := call.ask.val + Fees.totalFee call_fees call.ask.val  -- ✅ Works
```

**When designing theorems**: If you need arithmetic with quote prices, either:
1. Design the theorem using only `Float` parameters and convert quotes outside
2. Use explicit type coercions in the theorem statement
3. Avoid complex arithmetic with Quote types directly

### Time Type Requires .val for Comparisons

**Problem**: `Time` is a custom type with `val : Float` and `nonneg : val ≥ 0` fields.

```lean
theorem foo (duration : Time) (hDuration : duration > 0) := ...  -- ❌ Error: LT Time not found
```

**Solution**: Always use `.val` for comparisons:
```lean
theorem foo (duration : Time) (hDuration : duration.val > 0) := ...  -- ✅ Works
```

**Applies to**: Any parameter of type `Time`, `Rate`, or other structured types that wrap `Float`.

### Computational Checker Function Patterns

**DO NOT use tactic mode for checker functions**:

```lean
def checkPutCallParity ... : Bool := by  -- ❌ WRONG
  return result
```

**ALWAYS use term mode**:

```lean
def checkPutCallParity ... : Bool :=     -- ✅ CORRECT
  result
```

This matches Lean 4's expectations: checkers are executable functions, not tactic proofs.

### Float Operations That Don't Exist in Lean 4

Do NOT use these (they're not in Lean 4.26):
- ❌ `Float.max`
- ❌ `Float.min`
- ❌ `Float.log` (use workaround functions)
- ❌ Direct arithmetic without type coercion

**Workarounds**:
```lean
-- Instead of Float.max a b, use:
if a < b then b else a

-- Instead of Float.min a b, use:
if a < b then a else b
```

### Syntax Errors to Watch For

| Error | Fix |
|-------|-----|
| `nsorry` undefined | Replace with `sorry` (Lean 4 doesn't have `nsorry`) |
| `isArb.inl` invalid | Use `isArb := Or.inl` instead |
| `by ... return` in function | Convert to term mode: ` := expression` |
| `push_neg` makes no progress | Issue is earlier in the proof structure; review theorem goal type |

### Fees Calculation Pattern

Always follow this exact pattern for fees:

```lean
-- For cost (paying the ask price):
let cost := quote.ask + Fees.totalFee fees quote.ask

-- For proceeds (receiving the bid price):
let proceeds := quote.bid - Fees.totalFee fees quote.bid
```

Never use `quote.ask - Fees.totalFee` or other variations - this changes the semantics.

### When to Use `sorry` Appropriately

It's fine to use `sorry` for:
- Float arithmetic proofs that rely on IEEE754 properties (currently not formalized)
- Complex non-linear arithmetic that would require `nlinarith` or similar
- Theorems you're confident are true but haven't proven yet

But ALWAYS:
1. Add a comment explaining why you used `sorry`
2. Ensure the theorem statement is correct
3. Verify `lake build` passes (which validates syntax)
4. Create a matching computational checker function

### Review Checklist for New Modules

When creating a new Finance module with theorems:

- [ ] All theorem goal types are direct inequality constraints (no let-bindings in goal)
- [ ] All `Time`/`Rate` parameters use `.val` in comparisons
- [ ] All `PosReal` (quote) field arithmetic uses explicit coercion or `.val`
- [ ] Every theorem has a matching computational `def check*` function in term mode
- [ ] No `Float.max`, `Float.min`, or undefined functions used
- [ ] `lake build` passes completely before considering the module done
- [ ] Fee calculations follow the pattern above (cost with +, proceeds with -)
- [ ] Complex intermediate calculations are in `def` functions, not theorem let-bindings
