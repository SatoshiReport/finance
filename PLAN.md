# Chess Formalization Roadmap

This document tracks every open item on the path from the current Lean implementation to a provably correct, fully featured chess engine and beyond.

## 1. Reach Full Chess Feature Compliance
- **Move application consistency**
  - Route every state transition (`perft`, generators, demos) through `GameState.playMove` so checkmate/stalemate/draw flags are always finalized.
  - Audit for direct `movePiece` usage and either prove safety lemmas or eliminate the calls.
- **Rule coverage gaps**
  - Ensure en-passant availability requires a legal double-push history, not just board plausibility.
  - Enforce half-move/full-move counters inside SAN/PGN play to detect 50/75-move draws mid-game.
  - Verify automatic draw detection triggers when PGN-specified results disagree with board state.
  - Model underpromotion captures from SAN/PGN with explicit target piece validation.
  - Guarantee history snapshots include the starting position loaded via FEN/PGN for repetition checks.
- **Parsing robustness**
  - FEN: validate impossible castling flags (king/rook moved) via move history metadata where available.
  - SAN: support all suffix annotations (`!!`, `!?`, `??`), “ep” markers, and algebraic ambiguity detection for rare piece counts (e.g., four knights).
  - PGN: handle comments/variations deterministically (reject unsupported constructs with clear errors).
- **Demo polish**
  - Demonstrate FEN/SAN/PGN round-trips, en-passant, castling, and draw detection in `chessDemo`.
  - Provide CLI flags or inputs to load PGN/FEN from files for reproducible showcases.
- **Documentation**
  - Expand README with end-to-end usage examples (parsing a PGN, running perft, invoking slow tests).
  - Record FEN/SAN/PGN specification references and any deviations.

## 2. Testing Requirements
- **Fast suite (`lake test`)**
  - Cover every rule branch: castling legality, en-passant validation, promotion variants, repetition/draw triggers, parser round-trips.
  - Assert that SAN check/mate hints are validated across diverse boards.
  - Include canonical perft depths (1–3 for start, plus tactical FENs) with expected node counts.
- **Slow/extended suite (`RUN_SLOW_TESTS=1`)**
  - Deeper perft baselines (depth 4–5) for start and edge-case FENs (castling, en-passant).
  - Full PGN game corpus samples (e.g., famous games, tablebase endings) to exercise long sequences.
  - Stress FEN loader with random legal positions (coordinated with generator once available).
- **Continuous integration**
  - Make slow tests part of release workflow or nightly build.
  - Capture test commands/results in PR templates.

## 3. Documentation Artifacts
- **Specification references**
  - Link to official FIDE Laws of Chess, PGN standard, and SAN/FEN documents.
  - Document assumptions (e.g., clocks ignored, no adjournments).
- **Developer guides**
  - Explain how to add new rules plus the proofs/tests required.
  - Checklist for introducing new parsing features or demo scenarios.
- **User/demo docs**
  - Walkthrough for deriving SAN from a board, playing PGN files, and exporting FEN snapshots.

## 4. Formal Proof Backlog
- **Core theorems**
  - Prove `allLegalMoves` exactly matches the encoded rules (piece movement, castling, en-passant, promotions).
  - Show `GameState.playMove` preserves board invariants (single occupancy, piece counts) and updates clocks correctly.
  - Formalize draw detection correctness: 50/75-move rules, repetition counts, insufficient material classifications.
  - Demonstrate SAN/FEN/PGN parser soundness (parsed game reproduces original when serialized) and completeness (all legal SAN strings parse).
- **Perft correctness**
  - Prove `perft` counts match the recursive expansion definition using induction over depth.
- **History/repetition**
  - Establish the link between stored `history` snapshots and the official repetition rules.
- **Documentation of proofs**
  - Record theorem names and statements in README/CLAUDE requirements, ensuring future contributors see the growing proof corpus.

## 5. Beyond Full Compliance: Toward Solving Chess
- **Symmetry reductions**
  - Formally prove color-flip and rotational symmetries so equivalent positions can be collapsed.
- **Recursive decomposition**
  - Identify substructures (pawnless endings, locked pawn files) and prove they reduce to smaller solved cases.
- **Pruning invariants**
  - Prove sufficient conditions for pruning (fortresses, perpetual checks, tablebase draws) during search.
- **Automated strategy**
  - Combine reductions and pruning into a verified search that incrementally increases solved depth.
- **Long-term milestone**
  - Aim for endgame-tablebase proofs, then middlegame strategies, ultimately inching toward a complete solution.
