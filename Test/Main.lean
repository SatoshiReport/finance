import Chess.Core
import Chess.Movement
import Chess.Game
import Chess.Rules
import Chess.Parsing

open Chess
open Movement
open Game

open Rules
open Parsing

def check (desc : String) (cond : Bool) : IO Unit := do
  unless cond do
    throw <| IO.userError s!"Test failed: {desc}"

def expectNat (desc : String) (actual expected : Nat) : IO Unit :=
  check desc (actual == expected)

def expectStr (desc actual expected : String) : IO Unit :=
  check desc (actual == expected)

def expectBool (desc : String) (actual expected : Bool) : IO Unit :=
  check desc (actual == expected)

def expectPieceOption (desc : String) (actual expected : Option Piece) : IO Unit :=
  check desc (actual == expected)

def expectColor (desc : String) (actual expected : Color) : IO Unit :=
  check desc (actual == expected)

def expectInt (desc : String) (actual expected : Int) : IO Unit :=
  check desc (actual == expected)

def runSuite (label : String) (action : IO Unit) : IO Unit := do
  IO.println s!"[START] {label}"
  action
  IO.println s!"[DONE]  {label}"

def kingCenter : Square := Square.mkUnsafe 4 4
def knightCorner : Square := Square.mkUnsafe 0 0
def knightCenter : Square := Square.mkUnsafe 4 4

def testTargetCounts : IO Unit := do
  expectNat "king targets from center" (kingTargets kingCenter |>.length) 8
  expectNat "knight targets from corner" (knightTargets knightCorner |>.length) 2
  expectNat "knight targets from center" (knightTargets knightCenter |>.length) 8
  expectNat "board square count" allSquares.length 64
  expectStr "algebraic a1" (Square.mkUnsafe 0 0).algebraic "a1"
  expectStr "algebraic h8" (Square.mkUnsafe 7 7).algebraic "h8"
  expectInt "pawn direction white" (pawnDirection Color.White) 1
  expectInt "pawn direction black" (pawnDirection Color.Black) (-1)

def sampleWhitePawn : Piece := { pieceType := PieceType.Pawn, color := Color.White }
def sampleBlackPawn : Piece := { pieceType := PieceType.Pawn, color := Color.Black }
def sampleBlackKing : Piece := { pieceType := PieceType.King, color := Color.Black }
def sampleWhiteKing : Piece := { pieceType := PieceType.King, color := Color.White }
def sampleWhiteRook : Piece := { pieceType := PieceType.Rook, color := Color.White }
def sampleBlackRook : Piece := { pieceType := PieceType.Rook, color := Color.Black }

def originSquare : Square := Square.mkUnsafe 4 1
def captureSquare : Square := Square.mkUnsafe 4 3
def otherSquare : Square := Square.mkUnsafe 7 7
def clearSquare : Square := Square.mkUnsafe 2 2

def sampleBoard : Board :=
  emptyBoard
    |>.update originSquare (some sampleWhitePawn)
    |>.update captureSquare (some sampleBlackPawn)
    |>.update otherSquare (some sampleBlackKing)

def sampleMove : Move :=
  { piece := sampleWhitePawn
    fromSq := originSquare
    toSq := captureSquare
    isCapture := true
    promotion := none }

def sampleNonCaptureMove : Move :=
  { piece := sampleWhitePawn
    fromSq := originSquare
    toSq := clearSquare
    isCapture := false
    promotion := none }

def enPassantFrom : Square := Square.mkUnsafe 4 4 -- e5
def enPassantTarget : Square := Square.mkUnsafe 3 5 -- d6
def enPassantPawnSq : Square := Square.mkUnsafe 3 4 -- d5

def enPassantBoard : Board :=
  emptyBoard
    |>.update enPassantFrom (some sampleWhitePawn)
    |>.update enPassantPawnSq (some sampleBlackPawn)

def enPassantMove : Move :=
  { piece := sampleWhitePawn
    fromSq := enPassantFrom
    toSq := enPassantTarget
    isCapture := true
    isEnPassant := true }

def promotionFrom : Square := Square.mkUnsafe 4 6 -- e7
def promotionTo : Square := Square.mkUnsafe 4 7 -- e8

def promotionBoard : Board :=
  emptyBoard.update promotionFrom (some sampleWhitePawn)

def promotionMove : Move :=
  { piece := sampleWhitePawn
    fromSq := promotionFrom
    toSq := promotionTo
    isCapture := false
    promotion := some PieceType.Queen }

def castleKingFrom : Square := whiteKingStart
def castleKingTo : Square := Square.mkUnsafe 6 0 -- g1
def castleRookFrom : Square := whiteKingRookStart
def castleRookTo : Square := Square.mkUnsafe 5 0 -- f1

def castleBoard : Board :=
  emptyBoard
    |>.update castleKingFrom (some sampleWhiteKing)
    |>.update castleRookFrom (some sampleWhiteRook)

def castleBlockedBoard : Board :=
  castleBoard.update (Square.mkUnsafe 5 0) (some sampleWhitePawn)

def castleAttackedBoard : Board :=
  emptyBoard
    |>.update castleKingFrom (some sampleWhiteKing)
    |>.update castleRookFrom (some sampleWhiteRook)
    |>.update (Square.mkUnsafe 5 7) (some sampleBlackRook)

def castleMove : Move :=
  { piece := sampleWhiteKing
    fromSq := castleKingFrom
    toSq := castleKingTo
    isCapture := false
    isCastle := true
    castleRookFrom := some castleRookFrom
    castleRookTo := some castleRookTo }

def initialState : GameState :=
  { board := sampleBoard, toMove := Color.White, halfMoveClock := 5, fullMoveNumber := 1 }

def runMoveTests : IO Unit := do
  let after := GameState.playMove initialState sampleMove
  expectPieceOption "source cleared after move" (after.board originSquare) none
  expectPieceOption "destination receives pawn" (after.board captureSquare) (some sampleWhitePawn)
  expectPieceOption "unrelated square preserved" (after.board otherSquare) (some sampleBlackKing)
  expectColor "turn flips" after.toMove Color.Black
  expectNat "half-move clock reset on capture" after.halfMoveClock 0
  expectNat "full-move number stays when white moves" after.fullMoveNumber 1

def runRuleTests : IO Unit := do
  expectBool "basic move legality bool" (basicMoveLegalBool initialState sampleMove) true
  expectBool "capture flag consistent with occupied target" (captureFlagConsistent initialState sampleMove) true
  expectBool "capture flag consistent with empty target" (captureFlagConsistent initialState sampleNonCaptureMove) true
  expectBool "destination avoids friendly piece" (destinationFriendlyFree initialState sampleMove) true
  expectBool "square difference helper" (squaresDiffer sampleMove) true

def runSpecialMoveTests : IO Unit := do
  let enPassantState : GameState :=
    { board := enPassantBoard
      toMove := Color.White
      enPassantTarget := some enPassantTarget
      castlingRights := {} }
  let afterEp := GameState.playMove enPassantState enPassantMove
  expectPieceOption "en passant destination" (afterEp.board enPassantTarget) (some sampleWhitePawn)
  expectPieceOption "en passant capture removed" (afterEp.board enPassantPawnSq) none

  let promoState : GameState := { board := promotionBoard, toMove := Color.White }
  let afterPromo := GameState.playMove promoState promotionMove
  expectPieceOption "promotion creates new piece" (afterPromo.board promotionTo)
    (some { pieceType := PieceType.Queen, color := Color.White })

  let castleState : GameState := { board := castleBoard, toMove := Color.White, castlingRights := {} }
  let afterCastle := GameState.playMove castleState castleMove
  expectPieceOption "castle king destination" (afterCastle.board castleKingTo) (some sampleWhiteKing)
  expectPieceOption "castle rook destination" (afterCastle.board castleRookTo) (some sampleWhiteRook)
  expectPieceOption "castle clears origin" (afterCastle.board castleKingFrom) none
  expectPieceOption "castle clears rook origin" (afterCastle.board castleRookFrom) none

unsafe def runCastlingGenerationTests : IO Unit := do
  let castleState : GameState := { board := castleBoard, toMove := Color.White, castlingRights := {} }
  let moves := legalMovesFor castleState castleKingFrom
  check "castling generated" (moves.any (fun m => m.isCastle && m.toSq = castleKingTo))

  let blockedState : GameState := { board := castleBlockedBoard, toMove := Color.White, castlingRights := {} }
  let blockedMoves := legalMovesFor blockedState castleKingFrom
  check "castling blocked when path occupied" (blockedMoves.all (fun m => ¬ m.isCastle))

  let attackedState : GameState := { board := castleAttackedBoard, toMove := Color.White, castlingRights := {} }
  let attackedMoves := legalMovesFor attackedState castleKingFrom
  check "castling blocked when passing through attack" (attackedMoves.all (fun m => ¬ m.isCastle))

unsafe def runDrawTests : IO Unit := do
  let fifty : GameState := { board := emptyBoard, halfMoveClock := 100 }
  expectBool "fifty-move draw" (isFiftyMoveDraw fifty) true

  let kingOnlyBoard : Board := emptyBoard
    |>.update whiteKingStart (some sampleWhiteKing)
    |>.update blackKingStart (some sampleBlackKing)
  let insufficient : GameState := { board := kingOnlyBoard }
  expectBool "insufficient material king v king" (insufficientMaterial insufficient) true

def runStartingPositionTests : IO Unit := do
  expectPieceOption "white pawn a2" (startingBoard (Square.mkUnsafe 0 1))
    (some { pieceType := PieceType.Pawn, color := Color.White })
  expectPieceOption "black knight b8" (startingBoard (Square.mkUnsafe 1 7))
    (some { pieceType := PieceType.Knight, color := Color.Black })
  expectPieceOption "empty center" (startingBoard (Square.mkUnsafe 4 4)) none
  expectBool "white kingside castling right" standardGameState.castlingRights.whiteKingSide true
  expectBool "black queenside castling right" standardGameState.castlingRights.blackQueenSide true

def runFenTests : IO Unit := do
  match Parsing.parseFEN Parsing.startFEN with
  | .ok gs =>
      expectStr "start FEN roundtrip" (Parsing.toFEN gs) Parsing.startFEN
  | .error e => throw <| IO.userError s!"Failed to parse start FEN: {e}"
  expectStr "standard game renders start FEN" (Parsing.toFEN standardGameState) Parsing.startFEN
  match Parsing.parseFEN "8/8/8/8/8/8/8/8 w - - 0 1" with
  | .ok _ => throw <| IO.userError "FEN without kings should fail"
  | .error _ => pure ()
  match Parsing.parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e4 0 1" with
  | .ok _ => throw <| IO.userError "Invalid en passant rank should fail"
  | .error _ => pure ()
  match Parsing.parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/1NBQKBNR w KQkq - 0 1" with
  | .ok _ => throw <| IO.userError "Castling right without rook/king should fail"
  | .error _ => pure ()
  match Parsing.parseFEN "4k3/4b3/8/3Pp3/8/8/8/4K3 w - e6 0 1" with
  | .ok _ => throw <| IO.userError "En passant origin check should fail when origin occupied"
  | .error _ => pure ()
  match Parsing.parseFEN "2r1k3/4R3/8/8/8/8/8/2K5 w - - 0 1" with
  | .ok _ => throw <| IO.userError "Positions with both kings in check should fail"
  | .error _ => pure ()

def runSanAndPgnTests : IO Unit := do
  let simpleBoard : Board :=
    emptyBoard
      |>.update (Square.mkUnsafe 4 1) (some { pieceType := PieceType.Pawn, color := Color.White }) -- e2
      |>.update (Square.mkUnsafe 7 0) (some { pieceType := PieceType.King, color := Color.White }) -- h1
      |>.update (Square.mkUnsafe 0 7) (some { pieceType := PieceType.King, color := Color.Black }) -- a8
  let simpleState : GameState :=
    { board := simpleBoard, toMove := Color.White, castlingRights := {} }

  let afterE4 ←
    match Parsing.applySAN simpleState "e4" with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"SAN e4 failed: {e}"
  expectPieceOption "e4 occupied by white pawn" (afterE4.board (Square.mkUnsafe 4 3))
    (some { pieceType := PieceType.Pawn, color := Color.White })
  match Parsing.applySAN simpleState "e5" with
  | .ok _ => throw <| IO.userError "Illegal SAN e5 for white should fail"
  | .error _ => pure ()

  let underFen := "7k/6P1/8/8/8/8/8/6K1 w - - 0 1"
  let underState ←
    match Parsing.parseFEN underFen with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"Underpromotion FEN parse failed: {e}"
  let afterKnightPromo ←
    match Parsing.applySAN underState "g8=N" with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"Underpromotion SAN failed: {e}"
  expectPieceOption "underpromotion to knight" (afterKnightPromo.board (Square.mkUnsafe 6 7))
    (some { pieceType := PieceType.Knight, color := Color.White })

  let rookCheckBoard : Board :=
    emptyBoard
      |>.update (Square.mkUnsafe 4 0) (some { pieceType := PieceType.Rook, color := Color.White }) -- e1 rook
      |>.update (Square.mkUnsafe 7 0) (some sampleWhiteKing) -- h1 king
      |>.update (Square.mkUnsafe 4 7) (some sampleBlackKing) -- e8 king
  let rookCheckState : GameState := { board := rookCheckBoard, toMove := Color.White, castlingRights := {} }
  let afterRe7Plus ←
    match Parsing.applySAN rookCheckState "Re7+" with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"SAN Re7+ failed: {e}"
  expectBool "Re7+ reports check" (inCheck afterRe7Plus.board afterRe7Plus.toMove) true
  match Parsing.applySAN rookCheckState "Re7#" with
  | .ok _ => throw <| IO.userError "Re7# should fail when position is not mate"
  | .error _ => pure ()
  let nonCheckBoard :=
    rookCheckBoard
      |>.update (Square.mkUnsafe 4 7) none
      |>.update (Square.mkUnsafe 6 7) (some sampleBlackKing)
  let nonCheckState : GameState := { rookCheckState with board := nonCheckBoard }
  match Parsing.applySAN nonCheckState "Re7+" with
  | .ok _ => throw <| IO.userError "Re7+ should fail when the move is not check"
  | .error _ => pure ()

  let matePrep ←
    match Parsing.applySANs standardGameState ["e4", "e5", "Qh5", "Nc6", "Bc4", "Nf6"] with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"SAN prefix for mate failed: {e}"
  let mateAfter ←
    match Parsing.applySAN matePrep "Qxf7#" with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"SAN mate Qxf7# failed: {e}"
  expectBool "Qxf7# claims mate" (isCheckmate mateAfter) true
  match Parsing.applySAN matePrep "Qxf7+" with
  | .ok _ => throw <| IO.userError "Qxf7+ should fail because position is mate"
  | .error _ => pure ()

  let scholarsPGN :=
    "[Event \"Scholars\"]\n\n1. e4 e5 2. Qh5 Nc6 3. Bc4 Nf6 4. Qxf7#"
  let finalState ←
    match Parsing.playPGN scholarsPGN with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"PGN failed: {e}"
  expectBool "rook mate detected" (isCheckmate finalState) true
  expectBool "play move sets result" (interpretResult finalState = GameResult.whiteWin) true
  match Parsing.playPGN "[Event \"Mismatch\"]\n\n1. e4 e5 2. Qh5 Nc6 3. Bc4 Nf6 4. Qxf7# 0-1" with
  | .ok _ => throw <| IO.userError "PGN with mismatched result should fail"
  | .error _ => pure ()
  let resignationPGN := "[Event \"Resign\"]\n\n1. e4 1-0"
  let resignState ←
    match Parsing.playPGN resignationPGN with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"PGN resignation parse failed: {e}"
  expectBool "resignation sets recorded result" (resignState.result = some "1-0") true

def runPgnMetadataTests : IO Unit := do
  let pgn :=
    "[Event \"TagTest\"]\n[Site \"Somewhere\"]\n\n1. e4 $1 e5 $2 2. Nf3! 1-0"
  let parsed ←
    match Parsing.playPGNStructured pgn with
    | .ok p => pure p
    | .error e => throw <| IO.userError s!"PGN structured parse failed: {e}"
  expectNat "tag count" parsed.tags.length 2
  expectBool "event tag present" (parsed.tags.any (fun t => t.fst = "Event" ∧ t.snd = "TagTest")) true
  expectNat "moves parsed" parsed.moves.length 3
  match parsed.moves with
  | m1 :: m2 :: m3 :: _ =>
      expectNat "nags on e4" m1.nags.length 1
      expectNat "nags on e5" m2.nags.length 1
      expectNat "annotations carried" m3.nags.length 1
  | _ => throw <| IO.userError "missing moves for nag checks"
  expectBool "pgn result captured" (parsed.result = some "1-0") true
  expectBool "structured result matches state" (interpretResult parsed.finalState = GameResult.whiteWin) true

def runEndgameTests : IO Unit := do
  let stalemateFen := "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1"
  let stalemateState ←
    match Parsing.parseFEN stalemateFen with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"Stalemate FEN parse failed: {e}"
  expectBool "stalemate flagged" (isStalemate stalemateState) true

  let bishopFen := "7k/8/8/8/8/8/6B1/6K1 w - - 0 1"
  let bishopState ←
    match Parsing.parseFEN bishopFen with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"Bishop endgame FEN parse failed: {e}"
  expectBool "insufficient material K+B vs K" (insufficientMaterial bishopState) true
  expectBool "dead position same-color bishops" (deadPosition bishopState) true

  let snap := positionSnapshot standardGameState
  let repetitionState : GameState := { standardGameState with history := [snap, snap] }
  expectBool "threefold repetition detected" (threefoldRepetition repetitionState) true

def runPerftTests : IO Unit := do
  expectNat "perft depth 1" (perft standardGameState 1) 20
  -- Depth 2+ are expensive; keep optional for manual runs

def expectPerftFromFEN (desc : String) (fen : String) (depth expected : Nat) : IO Unit := do
  let state ←
    match Parsing.parseFEN fen with
    | .ok gs => pure gs
    | .error e => throw <| IO.userError s!"Failed to parse FEN for perft ({desc}): {e}"
  expectNat desc (perft state depth) expected

def runEdgePerftTests : IO Unit := do
  expectPerftFromFEN "en passant perft depth 1" "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 2" 1 6
  expectPerftFromFEN "en passant perft depth 2" "4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 2" 2 36
  expectPerftFromFEN "castling perft depth 1" "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" 1 26
  expectPerftFromFEN "castling perft depth 2" "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" 2 568

-- Optional deeper perft checks (expensive)
unsafe def runPerftDepth3 : IO Unit := do
  expectNat "perft depth 3" (perft standardGameState 3) 8902

unsafe def runPerftDepth4 : IO Unit := do
  expectNat "perft depth 4" (perft standardGameState 4) 197281

def lowercaseAscii (s : String) : String :=
  s.map fun c =>
    if 'A' ≤ c ∧ c ≤ 'Z' then
      Char.ofNat (c.toNat + ('a'.toNat - 'A'.toNat))
    else
      c

def shouldRunSlowTests : IO Bool := do
  match ← IO.getEnv "RUN_SLOW_TESTS" with
  | some flag =>
      let normalized := lowercaseAscii flag.trim
      pure (normalized = "1" ∨ normalized = "true" ∨ normalized = "yes")
  | none => pure false

def runDrawStatusTests : IO Unit := do
  let seventyFive : GameState := { board := emptyBoard, halfMoveClock := 150 }
  let (_, autoDraw, _, autoReasons) := drawStatus seventyFive
  expectBool "75-move automatic draw" autoDraw true
  expectBool "75-move reason tagged" (autoReasons.any (· = "75-move")) true

unsafe def runAllTests : IO Unit := do
  runSuite "Target counts" testTargetCounts
  runSuite "Basic move updates" runMoveTests
  runSuite "Rule helpers" runRuleTests
  runSuite "Special moves" runSpecialMoveTests
  runSuite "Castling generation" runCastlingGenerationTests
  runSuite "Draw helpers" runDrawTests
  runSuite "Starting position" runStartingPositionTests
  runSuite "FEN parsing" runFenTests
  runSuite "SAN/PGN basics" runSanAndPgnTests
  runSuite "PGN metadata" runPgnMetadataTests
  runSuite "Endgame conditions" runEndgameTests
  runSuite "Perft smoke" runPerftTests
  runSuite "Draw status" runDrawStatusTests
  let slow ← shouldRunSlowTests
  if slow then
    runSuite "Edge-case perft" runEdgePerftTests
  else
    IO.println "Skipping slow edge-case perft suite (set RUN_SLOW_TESTS=1 to include it)"
  IO.println s!"All chess tests completed successfully"
  pure ()

namespace Test
unsafe def main : IO Unit := runAllTests
end Test

unsafe def main : IO Unit := runAllTests
