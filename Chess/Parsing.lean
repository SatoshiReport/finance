import Chess.Core
import Chess.Game
import Chess.Movement
import Chess.Rules

namespace Chess
namespace Parsing

open Rules

structure PGNMove where
  move : Move
  nags : List String := []

structure PGNGame where
  tags : List (String × String)
  moves : List PGNMove
  finalState : GameState
  result : Option String := none

inductive SanCheckHint where
  | check
  | mate
deriving Repr, DecidableEq

structure SanToken where
  raw : String
  san : String
  checkHint : Option SanCheckHint := none
  nags : List String := []
deriving Repr

def SanToken.addNag (tok : SanToken) (nag : String) : SanToken :=
  { tok with nags := tok.nags ++ [nag] }

def pieceFromChar (c : Char) : Option Piece :=
  match c with
  | 'K' => some { pieceType := PieceType.King, color := Color.White }
  | 'Q' => some { pieceType := PieceType.Queen, color := Color.White }
  | 'R' => some { pieceType := PieceType.Rook, color := Color.White }
  | 'B' => some { pieceType := PieceType.Bishop, color := Color.White }
  | 'N' => some { pieceType := PieceType.Knight, color := Color.White }
  | 'P' => some { pieceType := PieceType.Pawn, color := Color.White }
  | 'k' => some { pieceType := PieceType.King, color := Color.Black }
  | 'q' => some { pieceType := PieceType.Queen, color := Color.Black }
  | 'r' => some { pieceType := PieceType.Rook, color := Color.Black }
  | 'b' => some { pieceType := PieceType.Bishop, color := Color.Black }
  | 'n' => some { pieceType := PieceType.Knight, color := Color.Black }
  | 'p' => some { pieceType := PieceType.Pawn, color := Color.Black }
  | _ => none

def pieceToChar (p : Piece) : Char :=
  let base :=
    match p.pieceType with
    | PieceType.King => 'k'
    | PieceType.Queen => 'q'
    | PieceType.Rook => 'r'
    | PieceType.Bishop => 'b'
    | PieceType.Knight => 'n'
    | PieceType.Pawn => 'p'
  if p.color = Color.White then base.toUpper else base

def parsePlacementRank (rank : Nat) (row : String) : Except String (List (Square × Piece)) := do
  let rec walk (file : Nat) (chars : List Char) (acc : List (Square × Piece)) : Except String (List (Square × Piece)) := do
    match chars with
    | [] =>
        if file = 8 then
          return acc
        else
          throw s!"FEN rank ended early on rank {rank}"
    | c :: cs =>
        if c.isDigit then
          let skip := c.toNat - '0'.toNat
          if skip = 0 then
            throw s!"FEN digit zero invalid"
          else
            walk (file + skip) cs acc
        else
          match pieceFromChar c with
          | some p =>
              if file ≥ 8 then
                throw s!"Too many squares on rank {rank}"
              else
                let sq := Square.mkUnsafe file rank
                walk (file + 1) cs ((sq, p) :: acc)
          | none => throw s!"Unknown FEN piece {c}"
  walk 0 row.toList []

def parsePlacement (placement : String) : Except String (List (Square × Piece)) := do
  let rows := placement.splitOn "/"
  if rows.length ≠ 8 then
    throw s!"Expected 8 ranks in FEN, found {rows.length}"
  let pairs := List.zip (List.range 8) rows
  pairs.foldlM
    (fun acc entry => do
      let (idx, row) := entry
      let rank := 7 - idx
      let rowPieces ← parsePlacementRank rank row
      pure (acc ++ rowPieces))
    []

def parseActiveColor (s : String) : Except String Color :=
  match s.trim with
  | "w" => return Color.White
  | "b" => return Color.Black
  | _ => throw s!"Invalid active color in FEN: {s}"

def parseCastlingRights (s : String) : CastlingRights :=
  if s = "-" then
    { whiteKingSide := false, whiteQueenSide := false, blackKingSide := false, blackQueenSide := false }
  else
    { whiteKingSide := s.contains 'K'
      whiteQueenSide := s.contains 'Q'
      blackKingSide := s.contains 'k'
      blackQueenSide := s.contains 'q' }

def parseEnPassant (s : String) : Except String (Option Square) :=
  let trimmed := s.trim
  if trimmed = "-" then
    return none
  else
    match Square.fromAlgebraic? trimmed with
    | some sq => return some sq
    | none => throw s!"Invalid en passant square {s}"

def parseNatField (s : String) (label : String) : Except String Nat :=
  match s.trim.toNat? with
  | some n => return n
  | none => throw s!"Invalid number for {label}: {s}"

def validateFEN (board : Board) (toMove : Color) (cr : CastlingRights) (ep : Option Square) : Except String Unit := do
  let pieces := allSquares.filterMap fun sq =>
    match board sq with
    | some p => some (sq, p)
    | none => none
  let kings := pieces.filter (fun (_, p) => p.pieceType = PieceType.King)
  let whiteKingCount := (kings.filter (fun (_, p) => p.color = Color.White)).length
  let blackKingCount := (kings.filter (fun (_, p) => p.color = Color.Black)).length
  if whiteKingCount ≠ 1 then throw "FEN must have exactly one white king"
  if blackKingCount ≠ 1 then throw "FEN must have exactly one black king"
  let whiteKingSq? := kingSquare board Color.White
  let blackKingSq? := kingSquare board Color.Black
  match whiteKingSq?, blackKingSq? with
  | some wk, some bk =>
      if Movement.isKingStepBool wk bk then
        throw "Kings cannot be adjacent"
  | _, _ => pure ()
  if inCheck board Color.White && inCheck board Color.Black then
    throw "Both kings cannot be simultaneously in check"
  let pawns := pieces.filter (fun (_, p) => p.pieceType = PieceType.Pawn)
  let whitePawns := (pawns.filter (fun (_, p) => p.color = Color.White)).length
  let blackPawns := (pawns.filter (fun (_, p) => p.color = Color.Black)).length
  if whitePawns > 8 then throw "Too many white pawns"
  if blackPawns > 8 then throw "Too many black pawns"
  if pawns.any (fun (sq, _) => sq.rankNat = 0 ∨ sq.rankNat = 7) then
    throw "Pawns cannot be on first or last rank"
  let castlingSquaresValid (c : Color) (kingSide : Bool) : Except String Unit := do
    if castleRight cr c kingSide then
      let cfg := castleConfig c kingSide
      match board cfg.kingFrom, board cfg.rookFrom with
      | some k, some r =>
          if k.pieceType ≠ PieceType.King ∨ k.color ≠ c then
            throw "Castling right set but king missing"
          if r.pieceType ≠ PieceType.Rook ∨ r.color ≠ c then
            throw "Castling right set but rook missing"
          pure ()
      | _, _ => throw "Castling right set but pieces missing"
    else pure ()
  castlingSquaresValid Color.White true
  castlingSquaresValid Color.White false
  castlingSquaresValid Color.Black true
  castlingSquaresValid Color.Black false
  match ep with
  | some sq =>
      let expectedRank := if toMove = Color.White then 5 else 2
      if sq.rankNat ≠ expectedRank then
        throw "En passant square rank inconsistent with side to move"
      if board sq ≠ none then
        throw "En passant square must be empty"
      let behindRank := if toMove = Color.White then sq.rankNat - 1 else sq.rankNat + 1
      let behindSq ←
        match Square.mk? sq.fileNat behindRank with
        | some s => pure s
        | none => throw "En passant square invalid"
      match board behindSq with
      | some p =>
          if p.pieceType ≠ PieceType.Pawn ∨ p.color ≠ toMove.opposite then
            throw "En passant target missing opposing pawn"
          else pure ()
      | none => throw "En passant target missing opposing pawn"
      let captureOffsets : List Int := [-1, 1]
      let captureExists :=
        captureOffsets.any fun df =>
          let fileInt := Int.ofNat sq.fileNat + df
          if fileInt < 0 ∨ fileInt ≥ 8 then false
          else
            let file := Int.toNat fileInt
            match Square.mk? file behindRank with
            | some cap =>
                match board cap with
                | some p => p.pieceType = PieceType.Pawn ∧ p.color = toMove
                | none => false
            | none => false
      if ¬captureExists then
        throw "No pawn can capture en passant target"
      let opp := toMove.opposite
      let dir := Movement.pawnDirection opp
      let originRankInt := sq.rankInt - dir
      if 0 ≤ originRankInt then
        if originRankInt < 8 then
          let originRank := Int.toNat originRankInt
          let originSq := Square.mkUnsafe sq.fileNat originRank
          if board originSq ≠ none then
            throw "En passant origin square must be empty"
        else
          throw "En passant origin rank invalid"
      else
        throw "En passant origin rank invalid"
  | none => pure ()

def parseFEN (fen : String) : Except String GameState := do
  match fen.trim.splitOn " " with
  | [placement, active, castling, ep, half, full] =>
      let pieces ← parsePlacement placement
      let board := Board.fromList pieces
      let toMove ← parseActiveColor active
      let enPassant ← parseEnPassant ep
      let halfMoveClock ← parseNatField half "half-move clock"
      let fullMoveNumber ← parseNatField full "full-move number"
      let castlingRights := parseCastlingRights castling
      validateFEN board toMove castlingRights enPassant
      return { board := board
               toMove := toMove
               halfMoveClock := halfMoveClock
               fullMoveNumber := fullMoveNumber
               enPassantTarget := enPassant
               castlingRights := castlingRights
               history := [] }
  | _ => throw s!"Invalid FEN field count: {fen}"

def rankToFen (board : Board) (rank : Nat) : String :=
  let step (state : Nat × String) (file : Nat) : Nat × String :=
    let (emptyCount, acc) := state
    let sq := Square.mkUnsafe file rank
    match board sq with
    | some p =>
        let acc' := if emptyCount = 0 then acc else acc.push (Char.ofNat ('0'.toNat + emptyCount))
        (0, acc'.push (pieceToChar p))
    | none => (emptyCount + 1, acc)
  let (trailing, built) := (List.range 8).foldl step (0, "")
  if trailing = 0 then built else built.push (Char.ofNat ('0'.toNat + trailing))

def boardToFenPlacement (board : Board) : String :=
  let ranks := (List.range 8).reverse
    |>.map (fun r => rankToFen board r)
  String.intercalate "/" ranks

def castlingToFen (cr : CastlingRights) : String :=
  let parts : List Char :=
    (if cr.whiteKingSide then ['K'] else []) ++
    (if cr.whiteQueenSide then ['Q'] else []) ++
    (if cr.blackKingSide then ['k'] else []) ++
    (if cr.blackQueenSide then ['q'] else [])
  if parts.isEmpty then "-" else String.ofList parts

def toFEN (gs : GameState) : String :=
  let placement := boardToFenPlacement gs.board
  let active := if gs.toMove = Color.White then "w" else "b"
  let castling := castlingToFen gs.castlingRights
  let ep := gs.enPassantTarget.map (fun sq => sq.algebraic) |>.getD "-"
  s!"{placement} {active} {castling} {ep} {gs.halfMoveClock} {gs.fullMoveNumber}"

def startFEN : String :=
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
def isResultToken (t : String) : Bool :=
  t = "1-0" ∨ t = "0-1" ∨ t = "1/2-1/2" ∨ t = "*"

def resultFromTokens (tok : Option String) : Option String :=
  match tok with
  | some r =>
      if isResultToken r then tok else none
  | none => none

def pieceLetter (pt : PieceType) : String :=
  match pt with
  | PieceType.King => "K"
  | PieceType.Queen => "Q"
  | PieceType.Rook => "R"
  | PieceType.Bishop => "B"
  | PieceType.Knight => "N"
  | PieceType.Pawn => ""

def promotionSuffix (m : Move) : String :=
  match m.promotion with
  | some pt => s!"={pieceLetter pt}"
  | none => ""

def sanDisambiguation (gs : GameState) (m : Move) : String :=
  if m.piece.pieceType = PieceType.Pawn then "" else
  let peers :=
    (allLegalMoves gs).filter fun cand =>
      cand.piece.pieceType = m.piece.pieceType ∧
      cand.piece.color = m.piece.color ∧
      cand.toSq = m.toSq ∧
      cand.fromSq ≠ m.fromSq
  if peers.isEmpty then ""
  else
    let fileConflict := peers.any (fun p => p.fromSq.file = m.fromSq.file)
    let rankConflict := peers.any (fun p => p.fromSq.rank = m.fromSq.rank)
    if !fileConflict then
      String.singleton m.fromSq.fileChar
    else if !rankConflict then
      String.singleton m.fromSq.rankChar
    else
      String.singleton m.fromSq.fileChar ++ String.singleton m.fromSq.rankChar

def moveToSanBase (gs : GameState) (m : Move) : String :=
  if m.isCastle then
    if m.toSq.fileNat = 6 then "O-O" else "O-O-O"
  else
    let capture := m.isCapture || m.isEnPassant
    if m.piece.pieceType = PieceType.Pawn then
      let pre := if capture then String.singleton m.fromSq.fileChar else ""
      let sep := if capture then "x" else ""
      pre ++ sep ++ m.toSq.algebraic ++ promotionSuffix m
    else
      let pre := pieceLetter m.piece.pieceType
      let dis := sanDisambiguation gs m
      let sep := if capture then "x" else ""
      pre ++ dis ++ sep ++ m.toSq.algebraic ++ promotionSuffix m

def moveToSAN (gs : GameState) (m : Move) : String :=
  let base := moveToSanBase gs m
  let next := GameState.playMove gs m
  let suffix :=
    if Rules.isCheckmate next then "#"
    else if Rules.inCheck next.board next.toMove then "+"
    else ""
  base ++ suffix

def normalizeCastleToken (s : String) : String :=
  let mapped := s.map (fun c => if c = '0' then 'O' else c)
  mapped

def parseSanToken (token : String) : Except String SanToken := do
  let trimmed := token.trim.replace "e.p." ""
  if trimmed.isEmpty then
    throw "SAN token cannot be empty"
  let rec peelAnnotations (chars : List Char) (acc : List Char) :
      List Char × List Char :=
    match chars with
    | c :: rest =>
        if c = '!' ∨ c = '?' then
          peelAnnotations rest (c :: acc)
        else
          (chars, acc)
    | [] => ([], acc)
  let rev := trimmed.toList.reverse
  let (revAfterAnn, annRev) := peelAnnotations rev []
  let (afterMate, hasMate) :=
    match revAfterAnn with
    | '#' :: rest => (rest, true)
    | _ => (revAfterAnn, false)
  let dropped := afterMate.dropWhile (fun c => c = '+')
  let (afterChecks, hasCheck) :=
    if hasMate then
      (dropped, false)
    else
      (dropped, dropped.length ≠ afterMate.length)
  let base := String.ofList afterChecks.reverse
  if base.isEmpty then
    throw s!"SAN token missing move description: {token}"
  let nags := if annRev.isEmpty then [] else [String.ofList annRev.reverse]
  let normalized := normalizeCastleToken base
  let hint :=
    if hasMate then some SanCheckHint.mate
    else if hasCheck then some SanCheckHint.check
    else none
  return { raw := token, san := normalized, checkHint := hint, nags := nags }

def validateCheckHint (token : SanToken) (after : GameState) : Except String Unit :=
  match token.checkHint with
  | none => pure ()
  | some SanCheckHint.check => do
      if Rules.isCheckmate after then
        throw s!"SAN {token.raw} indicates check but move is mate"
      if !Rules.inCheck after.board after.toMove then
        throw s!"SAN {token.raw} indicates check but resulting position is not check"
  | some SanCheckHint.mate => do
      if !Rules.isCheckmate after then
        throw s!"SAN {token.raw} indicates mate but resulting position is not mate"

def moveFromSanToken (gs : GameState) (token : SanToken) : Except String Move := do
  let legal := allLegalMoves gs
  let legalFiltered :=
    legal.filter fun m =>
      if m.piece.pieceType = PieceType.Pawn ∧ m.promotion.isSome then
        m.toSq.rankNat = pawnPromotionRank m.piece.color
      else true
  let candidates := legalFiltered.filter (fun m => moveToSanBase gs m = token.san)
  match candidates with
  | [m] =>
      let preview := gs.movePiece m
      validateCheckHint token preview
      return m
  | [] => throw s!"No legal move matches SAN: {token.raw}"
  | _ => throw s!"Ambiguous SAN: {token.raw}"

def moveFromSAN (gs : GameState) (token : String) : Except String Move := do
  let parsed ← parseSanToken token
  moveFromSanToken gs parsed

def applySAN (gs : GameState) (token : String) : Except String GameState := do
  let m ← moveFromSAN gs token
  applyLegalMove gs m

def applySANs (gs : GameState) (tokens : List String) : Except String GameState :=
  tokens.foldlM (fun st t => applySAN st t) gs

structure StripState where
  inBrace : Bool := false
  inParen : Nat := 0
  skipLine : Bool := false

def stripPGNNoise (pgn : String) : String :=
  let step (stateAcc : StripState × List Char) (c : Char) : StripState × List Char :=
    let (state, acc) := stateAcc
    match c with
    | '{' => ({ state with inBrace := true }, acc)
    | '}' => ({ state with inBrace := false }, acc)
    | '(' => ({ state with inParen := state.inParen + 1 }, acc)
    | ')' =>
        let nextParen := if state.inParen = 0 then 0 else state.inParen - 1
        ({ state with inParen := nextParen }, acc)
    | ';' => ({ state with skipLine := true }, acc)
    | '\n' => ({ state with skipLine := false }, ' ' :: acc)
    | _ =>
        if state.inBrace || state.inParen > 0 || state.skipLine then
          (state, acc)
        else
          (state, c :: acc)
  let (_, charsRev) := pgn.foldl step ({}, [])
  String.ofList charsRev.reverse

def parseTags (pgn : String) : List (String × String) :=
  let rec loop : List String → List (String × String)
    | [] => []
    | line :: rest =>
        if line.startsWith "[" && line.contains '\"' then
          let noOpen := line.drop 1
          match noOpen.splitOn "\"" with
          | name :: val :: _ =>
              (name.trim, val) :: loop rest
          | _ => loop rest
        else
          loop rest
  loop (pgn.splitOn "\n")

def tokensFromPGN (pgn : String) : List String :=
  let withoutTags :=
    pgn.splitOn "\n"
      |>.filter (fun line => ¬ line.trim.startsWith "[")
      |> String.intercalate "\n"
  let cleaned := stripPGNNoise withoutTags
  let normalized := cleaned.map (fun c => if c = '\n' ∨ c = '\t' ∨ c = '\r' then ' ' else c)
  normalized.splitOn " "
    |>.filter (fun t => ¬ t.isEmpty)
    |>.filter (fun t => ¬ t.startsWith "[" ∧ ¬ t.endsWith "]")

def splitMoveTokens (tokens : List String) : (List String × Option String) :=
  let moves := tokens.filter (fun t => ¬ t.toList.any (fun c => c = '.') ∧ ¬ isResultToken t)
  let res := tokens.find? isResultToken
  (moves, res)

def collectSanWithNags (tokens : List String) : Except String (List SanToken) :=
  let rec go (acc : List SanToken) (toks : List String) : Except String (List SanToken) := do
    match toks with
    | [] => pure acc.reverse
    | t :: ts =>
        if t.startsWith "$" then
          let nag := t.drop 1
          match acc with
          | [] => throw "NAG appears before any move"
          | entry :: rest =>
              let updated := entry.addNag nag
              go (updated :: rest) ts
        else
          let parsed ← parseSanToken t
          go (parsed :: acc) ts
  go [] tokens

def startFromTags (tags : List (String × String)) : Except String GameState :=
  match tags.find? (fun t => t.fst = "FEN") with
  | some (_, fen) => parseFEN fen
  | none => pure standardGameState

def playPGNStructured (pgn : String) : Except String PGNGame := do
  let tags := parseTags pgn
  let allTokens := tokensFromPGN pgn
  let (moveTokens, resultTok) := splitMoveTokens allTokens
  let gameResult := resultFromTokens resultTok
  let sanWithNags ← collectSanWithNags moveTokens
  let start ← startFromTags tags
  let step (acc : GameState × List PGNMove) (entry : SanToken) :
      Except String (GameState × List PGNMove) := do
    let m ← moveFromSanToken acc.fst entry
    let next ← applyLegalMove acc.fst m
    pure (next, { move := m, nags := entry.nags } :: acc.snd)
  let (finalState, movesRev) ← sanWithNags.foldlM step (start, [])
  let finalState ←
    match gameResult, finalState.result with
    | some declared, some actual =>
        if declared = actual then
          pure finalState
        else
          throw s!"PGN declares result {declared} but board reached {actual}"
    | some declared, none =>
        pure { finalState with result := some declared }
    | none, _ => pure finalState
  pure { tags := tags, moves := movesRev.reverse, finalState := finalState, result := gameResult }

def extractTagValue (pgn : String) (tag : String) : Option String :=
  let rec loop : List String → Option String
    | [] => none
    | line :: rest =>
        if line.startsWith s!"[{tag} " then
          let parts := line.drop (tag.length + 2) -- drop '[' and tag plus space
          match parts.splitOn "\"" with
          | _ :: val :: _ => some val
          | _ => loop rest
        else
          loop rest
  loop (pgn.splitOn "\n")

def playPGN (pgn : String) : Except String GameState := do
  let parsed ← playPGNStructured pgn
  pure parsed.finalState

end Parsing
end Chess
