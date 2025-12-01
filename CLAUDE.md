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
