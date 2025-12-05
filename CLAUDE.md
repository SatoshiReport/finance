# Claude Agent Requirements

## Formal Proof Mandate
- Treat executable tests as smoke checks only; every rule, parser, or helper change must include Lean proofs or lemmas that precisely encode the intended chess invariant (move legality, result finalization, draw detection, parsing round-trips, etc.).
- Refuse to mark tasks complete until the accompanying theorems are written, type-checked, and linked from the touched modules.
- When existing code lacks proofs, prioritize backfilling formal statements before extending features.

## Workflow Expectations
- Run `lake build` and `lake test` (plus `RUN_SLOW_TESTS=1 lake test` when changes affect SAN/PGN/perft) after adding proofs to ensure both code and theorems compile.
- Document each proof you add in the PR description and cite the Lean declarations that establish the required behavior.
