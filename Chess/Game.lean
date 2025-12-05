import Chess.Core
import Chess.Movement

namespace Chess

def positionSnapshot (gs : GameState) : PositionSnapshot :=
  let pieces :=
    allSquares.filterMap fun sq =>
      match gs.board sq with
      | some p => some (sq, p)
      | none => none
  { pieces := pieces
    toMove := gs.toMove
    castlingRights := gs.castlingRights
    enPassantTarget := gs.enPassantTarget }

def GameState.promotedPiece (_gs : GameState) (m : Move) : Piece :=
  match m.promotion with
  | some pt => { pieceType := pt, color := m.piece.color }
  | none => m.piece

def enPassantCaptureSquare (m : Move) : Option Square :=
  if m.isEnPassant then
    let dir := Movement.pawnDirection m.piece.color
    let capRankInt := m.toSq.rankInt - dir
    if _h : 0 ≤ capRankInt then
      let capRank := Int.toNat capRankInt
      some <| Square.mkUnsafe m.toSq.fileNat capRank
    else
      none
  else
    none

def updateCastlingRights (cr : CastlingRights) (m : Move) (capturedSq : Option Square) : CastlingRights :=
  let cr1 :=
    match m.piece.color, m.piece.pieceType with
    | Color.White, PieceType.King =>
        { cr with whiteKingSide := false, whiteQueenSide := false }
    | Color.Black, PieceType.King =>
        { cr with blackKingSide := false, blackQueenSide := false }
    | Color.White, PieceType.Rook =>
        if m.fromSq = whiteKingRookStart then { cr with whiteKingSide := false }
        else if m.fromSq = whiteQueenRookStart then { cr with whiteQueenSide := false } else cr
    | Color.Black, PieceType.Rook =>
        if m.fromSq = blackKingRookStart then { cr with blackKingSide := false }
        else if m.fromSq = blackQueenRookStart then { cr with blackQueenSide := false } else cr
    | _, _ => cr
  match capturedSq with
  | some sq =>
      if sq = whiteKingRookStart then { cr1 with whiteKingSide := false }
      else if sq = whiteQueenRookStart then { cr1 with whiteQueenSide := false }
      else if sq = blackKingRookStart then { cr1 with blackKingSide := false }
      else if sq = blackQueenRookStart then { cr1 with blackQueenSide := false }
      else cr1
  | none => cr1

def GameState.movePiece (gs : GameState) (m : Move) : GameState :=
  let movingPiece := gs.promotedPiece m
  let captureSq := enPassantCaptureSquare m |>.getD m.toSq
  let boardAfterCapture :=
    if m.isEnPassant then gs.board.update captureSq none else gs.board
  let boardAfterCastle :=
    if m.isCastle then
      match m.castleRookFrom, m.castleRookTo with
      | some rFrom, some rTo =>
          let cleared := boardAfterCapture.update rFrom none
          cleared.update rTo (boardAfterCapture rFrom)
      | _, _ => boardAfterCapture
    else
      boardAfterCapture
  let board' := (boardAfterCastle.update m.fromSq none).update m.toSq (some movingPiece)
  let resetHalf := m.isCapture || m.isEnPassant || m.piece.pieceType == PieceType.Pawn
  let nextFull := if gs.toMove = Color.Black then gs.fullMoveNumber + 1 else gs.fullMoveNumber
  let newEnPassant :=
    if m.piece.pieceType == PieceType.Pawn ∧ Int.natAbs (Movement.rankDiff m.fromSq m.toSq) = 2 then
      let dir := Movement.pawnDirection m.piece.color
      let targetRankInt := m.fromSq.rankInt + dir
      if _h : 0 ≤ targetRankInt then
        let targetRank := Int.toNat targetRankInt
        some (Square.mkUnsafe m.fromSq.fileNat targetRank)
      else
        none
    else
      none
  let updatedCastling := updateCastlingRights gs.castlingRights m (if m.isCapture || m.isEnPassant then some captureSq else none)
  let nextResult :=
    if gs.result.isSome then gs.result else none
  { gs with
    board := board'
    toMove := gs.toMove.opposite
    halfMoveClock := if resetHalf then 0 else gs.halfMoveClock + 1
    fullMoveNumber := nextFull
    enPassantTarget := newEnPassant
    castlingRights := updatedCastling
    history := gs.history ++ [positionSnapshot gs]
    result := nextResult
  }

namespace Game

end Game

end Chess
