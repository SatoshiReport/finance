# Repository Guidelines

## Project Structure & Module Organization
- `Chess/` contains the Lean 4 core: `Core.lean` defines geometry and state, `Movement.lean` encodes moves, `Game.lean` tracks updates, and `Demo.lean` exposes `#eval` samples plus the `chessDemo` executable.
- `Test/Main.lean` holds the lightweight smoke tests that assert target counts and move updates against curated boards.
- `lakefile.lean`, `lake-manifest.json`, and `lean-toolchain` steer Lake, and `ci.sh` sequences fmt/build/test/commit/push for contributors.
- Keep new modules near `Chess/` so the default Lake workspace picks them up automatically.

## Build, Test, and Development Commands
- `lake fmt` (when available) applies Lean formatting to all modules before commits.
- `lake build` compiles the project, ensuring all Lean dependencies resolve and eliminates module errors.
- `lake exe run chessDemo` builds and runs the demo executable described in `Chess/Demo.lean`.
- `lake test` executes `Test/Main.lean`, covering target counts and move-state checks.
- `./ci.sh` runs fmt/build/test, stages changes, asks Claude for a commit message, and pushes (use only when you want the scripted flow).

## Coding Style & Naming Conventions
- Follow Lean 4 idioms: two-space indentation, line-wrapped `do` blocks, and `def`/`theorem` alignment so pattern-matching is readable.
- Prefer `PascalCase` for structures/enums (`GameState`, `PieceType`), and `camelCase` for functions/predicates (`movePiece`, `kingTargets`).
- Keep files per module (one `*.lean` per high-level concern), and comment only when the intent is non-obvious—Lean proofs tend to self-document.
- Let `#eval` snippets live in `Demo.lean` and avoid embedding executable tests inside core files.

## Testing Guidelines
- `Test/Main.lean` contains hand-rolled assertions (`expectNat`, `expectPieceOption`) that describe expected behavior; add helpers there before expanding test coverage.
- Name new tests with descriptive prefixes (`test...`, `run...`) so they can be found easily in the single `main`.
- Always run `lake test` locally; the CI script mirrors that command.
- When adding fixtures, reuse `Sample` data (e.g., `sampleBoard`, `sampleMove`) to keep expectations deterministic.
- Strive for full coverage: new features should be accompanied by tests exercising both positive and boundary conditions so the Lean model remains well-specified.

## Commit & Pull Request Guidelines
- Commits follow a present-tense, imperative style without trailing periods (`Fix kingside move set`, `Document CI script`); the history leans descriptive (`Migrate chess codebase…`, `Fix compilation errors…`).
- PRs should explain the change, list the commands you ran (e.g., `lake fmt`, `lake test`), and link related issues or RFCs.
- Include screenshots or textual outputs only when demonstrating behavior not covered by the tests; add them to PR descriptions if helpful.

## Agent Tips
- The repository is Lean-centric, so lean on Lake commands for formatting and verification.
- Use the existing `ci.sh` script for a reproducible workflow, but run it only when the `claude` CLI is configured and you intend to push automatically.
