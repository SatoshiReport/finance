# Chess: Lean 4 Formalization of Standard Chess Rules

This repository builds a Lean 4 model of the classic chess game: board geometry, pieces, movement predicates, and the core game state logic.

## Layout

- `Chess/Core.lean` defines files/ranks, squares, colors, pieces, boards, and the `GameState` structure.
- `Chess/Movement.lean` encodes each piece's legal moves and includes helper lemmas about those movements.
- `Chess/Game.lean` updates the board, tracks turns, and proves board-update invariants.
- `Chess/Demo.lean` exercises the board helpers via `#eval` expressions and powers a lightweight executable.

## Build

```bash
lake build
lake exe run chessDemo
```

The `chessDemo` executable prints algebraic square names, king/knight targets, and move-state details defined in `Chess/Demo.lean`.

## Tests

Run the Lean test suite with:

```bash
lake test
```

Set `RUN_SLOW_TESTS=1` to include the optional SAN/PGN regression tests and deeper perft baselines for en passant and castling edge cases.

## Reference Specifications

- [FIDE Laws of Chess](https://handbook.fide.com/chapter/E012023) for movement, castling, draw, and repetition rules.
- [Portable Game Notation (PGN)](https://www.chessclub.com/user/help/PGN-spec) and [Standard Algebraic Notation (SAN)] for recording and parsing moves.
- [Forsyth–Edwards Notation (FEN)](https://www.chessprogramming.org/Forsyth-Edwards_Notation) for serializing positions.

All new rules or helpers must cite the relevant specification and link their Lean proofs to those references.

## Usage Examples

- `#eval Parsing.toFEN standardGameState` — emit the canonical starting FEN.
- `#eval Parsing.playPGN demoScholarsPGN` — load a SAN move list, verify legality, and surface the recorded result.
- `#eval perft standardGameState 3` — count all legal move trees to depth 3 (set `RUN_SLOW_TESTS=1` before `lake test` to exercise deeper baselines).

## Formal Proof Requirement

Executable tests give fast feedback, but the repository goal is a fully proven model of chess. Every new rule, parser, or helper must ultimately be justified by Lean theorems that state the intended invariant (legal move equivalence, draw conditions, parser soundness, etc.) and prove it from first principles. When you add behavior, pair it with the necessary definitions/lemmas so that the proof footprint grows alongside the implementation—tests alone are no longer sufficient.
