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

## CI Pipeline

The repository includes `ci.sh` which:
1. Runs `lake fmt` (if available)
2. Builds the project with `lake build`
3. Runs mathlib style lints (exits gracefully if lint-style fails)
4. Runs tests with `lake test`
5. Uses Claude to generate a commit message from the staged diff
6. Commits and pushes changes

**Important**: Do not modify CI pipeline code without explicit instructions.

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
