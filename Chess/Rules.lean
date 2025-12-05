import Chess.Core
import Chess.Movement
import Chess.Game
import Init.Data.Bool
import Init.SimpLemmas

namespace Chess
namespace Rules

def originHasPiece (gs : GameState) (m : Move) : Bool :=
  match gs.board m.fromSq with
  | some _ => true
  | none => false

def originHasPieceProp (gs : GameState) (m : Move) : Prop :=
  originHasPiece gs m = true

def turnMatches (gs : GameState) (m : Move) : Bool :=
  m.piece.color = gs.toMove

def turnMatchesProp (gs : GameState) (m : Move) : Prop :=
  turnMatches gs m = true

def destinationFriendlyFree (gs : GameState) (m : Move) : Bool :=
  match gs.board m.toSq with
  | some p => p.color ≠ m.piece.color
  | none => true

def destinationFriendlyFreeProp (gs : GameState) (m : Move) : Prop :=
  destinationFriendlyFree gs m = true

def captureFlagConsistent (gs : GameState) (m : Move) : Bool :=
  match gs.board m.toSq with
  | some _ => m.isCapture
  | none => ¬ m.isCapture

def squaresDiffer (m : Move) : Bool :=
  m.fromSq ≠ m.toSq

def squaresDifferProp (m : Move) : Prop :=
  squaresDiffer m = true

def basicMoveLegalBool (gs : GameState) (m : Move) : Bool :=
  originHasPiece gs m &&
    turnMatches gs m &&
    destinationFriendlyFree gs m &&
    captureFlagConsistent gs m &&
    squaresDiffer m

def basicMoveLegal (gs : GameState) (m : Move) : Prop :=
  originHasPieceProp gs m ∧
    turnMatchesProp gs m ∧
    destinationFriendlyFreeProp gs m ∧
    captureFlagConsistent gs m ∧
    squaresDifferProp m

def pathClear (b : Board) (source target : Square) : Bool :=
  Movement.squaresBetween source target |>.all fun sq => b sq = none

def pieceAttacksSquare (b : Board) (p : Piece) (source target : Square) : Bool :=
  match p.pieceType with
  | PieceType.King =>
      Movement.isKingStepBool source target
  | PieceType.Queen =>
      decide (Movement.isQueenMove source target) && pathClear b source target
  | PieceType.Rook =>
      decide (Movement.isRookMove source target) && pathClear b source target
  | PieceType.Bishop =>
      decide (Movement.isDiagonal source target) && pathClear b source target
  | PieceType.Knight =>
      Movement.isKnightMoveBool source target
  | PieceType.Pawn =>
      decide (Movement.isPawnCapture p.color source target)

def anyAttacksSquare (b : Board) (target : Square) (byColor : Color) : Bool :=
  allSquares.any fun sq =>
    match b sq with
    | some p => p.color = byColor && pieceAttacksSquare b p sq target
    | none => false

def kingSquare (b : Board) (c : Color) : Option Square :=
  allSquares.find? fun sq =>
    match b sq with
    | some p => p.pieceType = PieceType.King ∧ p.color = c
    | none => false

def inCheck (b : Board) (c : Color) : Bool :=
  match kingSquare b c with
  | some k => anyAttacksSquare b k c.opposite
  | none => false

def basicLegalAndSafe (gs : GameState) (m : Move) : Bool :=
  let next := gs.movePiece m
  basicMoveLegalBool gs m && !(inCheck next.board gs.toMove)

def isEnemyAt (b : Board) (c : Color) (sq : Square) : Bool :=
  match b sq with
  | some p => p.color ≠ c
  | none => false

def isEmpty (b : Board) (sq : Square) : Bool :=
  b sq = none

def pawnStartRank : Color → Nat
  | Color.White => 1
  | Color.Black => 6

def pawnPromotionRank : Color → Nat
  | Color.White => 7
  | Color.Black => 0

structure CastleConfig where
  kingFrom : Square
  kingTo : Square
  rookFrom : Square
  rookTo : Square
  emptySquares : List Square
  checkSquares : List Square

def castleConfig (c : Color) (kingSide : Bool) : CastleConfig :=
  match c, kingSide with
  | Color.White, true =>
      { kingFrom := whiteKingStart
        kingTo := Square.mkUnsafe 6 0
        rookFrom := whiteKingRookStart
        rookTo := Square.mkUnsafe 5 0
        emptySquares := [Square.mkUnsafe 5 0, Square.mkUnsafe 6 0]
        checkSquares := [whiteKingStart, Square.mkUnsafe 5 0, Square.mkUnsafe 6 0] }
  | Color.White, false =>
      { kingFrom := whiteKingStart
        kingTo := Square.mkUnsafe 2 0
        rookFrom := whiteQueenRookStart
        rookTo := Square.mkUnsafe 3 0
        emptySquares := [Square.mkUnsafe 1 0, Square.mkUnsafe 2 0, Square.mkUnsafe 3 0]
        checkSquares := [whiteKingStart, Square.mkUnsafe 3 0, Square.mkUnsafe 2 0] }
  | Color.Black, true =>
      { kingFrom := blackKingStart
        kingTo := Square.mkUnsafe 6 7
        rookFrom := blackKingRookStart
        rookTo := Square.mkUnsafe 5 7
        emptySquares := [Square.mkUnsafe 5 7, Square.mkUnsafe 6 7]
        checkSquares := [blackKingStart, Square.mkUnsafe 5 7, Square.mkUnsafe 6 7] }
  | Color.Black, false =>
      { kingFrom := blackKingStart
        kingTo := Square.mkUnsafe 2 7
        rookFrom := blackQueenRookStart
        rookTo := Square.mkUnsafe 3 7
        emptySquares := [Square.mkUnsafe 1 7, Square.mkUnsafe 2 7, Square.mkUnsafe 3 7]
        checkSquares := [blackKingStart, Square.mkUnsafe 3 7, Square.mkUnsafe 2 7] }

def castleRight (cr : CastlingRights) (c : Color) (kingSide : Bool) : Bool :=
  match c, kingSide with
  | Color.White, true => cr.whiteKingSide
  | Color.White, false => cr.whiteQueenSide
  | Color.Black, true => cr.blackKingSide
  | Color.Black, false => cr.blackQueenSide

def castleMoveIfLegal (gs : GameState) (kingSide : Bool) : Option Move :=
  let c := gs.toMove
  let cfg := castleConfig c kingSide
  if castleRight gs.castlingRights c kingSide then
    match gs.board cfg.kingFrom, gs.board cfg.rookFrom with
    | some k, some r =>
        if k.pieceType = PieceType.King ∧ k.color = c ∧ r.pieceType = PieceType.Rook ∧ r.color = c then
          let pathEmpty := cfg.emptySquares.all fun sq => isEmpty gs.board sq
          let safe := cfg.checkSquares.all fun sq => !(inCheck (gs.board.update cfg.kingFrom none |>.update sq (some k)) c)
          if pathEmpty && safe then
            some { piece := k
                   fromSq := cfg.kingFrom
                   toSq := cfg.kingTo
                   isCastle := true
                   castleRookFrom := some cfg.rookFrom
                   castleRookTo := some cfg.rookTo }
          else
            none
        else
          none
    | _, _ => none
  else
    none

def promotionTargets : List PieceType :=
  [PieceType.Queen, PieceType.Rook, PieceType.Bishop, PieceType.Knight]

def promotionMoves (m : Move) : List Move :=
  if m.piece.pieceType = PieceType.Pawn ∧ m.toSq.rankNat = pawnPromotionRank m.piece.color then
    promotionTargets.map (fun pt => { m with promotion := some pt })
  else
    [m]

def slidingTargets (gs : GameState) (src : Square) (p : Piece) (deltas : List (Int × Int)) : List Move :=
  let board := gs.board
  let color := p.color
  let maxStep := 7
  let rec walk (df dr : Int) (step : Nat) (acc : List Move) : List Move :=
    match step with
    | 0 => acc
    | Nat.succ s =>
        let offset := Int.ofNat (maxStep - s)
        match Movement.squareFromInts (src.fileInt + df * offset) (src.rankInt + dr * offset) with
        | none => acc
        | some target =>
            if isEmpty board target then
              walk df dr s ({ piece := p, fromSq := src, toSq := target } :: acc)
            else if isEnemyAt board color target then
              { piece := p, fromSq := src, toSq := target, isCapture := true } :: acc
            else
              acc
  deltas.foldr
    (fun d acc =>
      let (df, dr) := d
      walk df dr maxStep acc)
    []

def pieceTargets (gs : GameState) (src : Square) (p : Piece) : List Move :=
  let board := gs.board
  let color := p.color
  match p.pieceType with
  | PieceType.King =>
      let standard :=
        Movement.kingTargets src |>.filterMap fun target =>
        if destinationFriendlyFree gs { piece := p, fromSq := src, toSq := target } then
          match board target with
          | some _ => some { piece := p, fromSq := src, toSq := target, isCapture := true }
          | none => some { piece := p, fromSq := src, toSq := target }
        else
          none
      let castles : List Move :=
        ([castleMoveIfLegal gs true, castleMoveIfLegal gs false]).filterMap id
      standard ++ castles
  | PieceType.Queen =>
      slidingTargets gs src p
        [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]
  | PieceType.Rook =>
      slidingTargets gs src p [(1, 0), (-1, 0), (0, 1), (0, -1)]
  | PieceType.Bishop =>
      slidingTargets gs src p [(1, 1), (-1, -1), (1, -1), (-1, 1)]
  | PieceType.Knight =>
      Movement.knightTargets src |>.filterMap fun target =>
        if destinationFriendlyFree gs { piece := p, fromSq := src, toSq := target } then
          match board target with
          | some _ => some { piece := p, fromSq := src, toSq := target, isCapture := true }
          | none => some { piece := p, fromSq := src, toSq := target }
        else
          none
  | PieceType.Pawn =>
      let dir := Movement.pawnDirection color
      let oneStep := Movement.squareFromInts src.fileInt (src.rankInt + dir)
      let twoStep := Movement.squareFromInts src.fileInt (src.rankInt + 2 * dir)
      let forwardMoves : List Move :=
        match oneStep with
        | some target =>
            if isEmpty board target then
              let base : List Move := [{ piece := p, fromSq := src, toSq := target }]
              let doubleStep : List Move :=
                if src.rankNat = pawnStartRank color then
                  match twoStep with
                  | some target2 =>
                      if isEmpty board target2 then
                        [{ piece := p, fromSq := src, toSq := target2 }]
                      else
                        []
                  | none => []
                else
                  []
              base ++ doubleStep
            else
              []
        | none => []
      let captureOffsets : List Int := [-1, 1]
      let captureMoves :=
        captureOffsets.foldr
          (fun df acc =>
            match Movement.squareFromInts (src.fileInt + df) (src.rankInt + dir) with
            | some target =>
                if isEnemyAt board color target then
                  promotionMoves { piece := p, fromSq := src, toSq := target, isCapture := true } ++ acc
                else if gs.enPassantTarget = some target ∧ isEmpty board target then
                  { piece := p, fromSq := src, toSq := target, isCapture := true, isEnPassant := true } :: acc
                else
                  acc
            | none => acc)
          []
      forwardMoves.foldr (fun m acc => promotionMoves m ++ acc) [] ++ captureMoves
def pinnedSquares (gs : GameState) (c : Color) : List (Square × Square) :=
  match kingSquare gs.board c with
  | none => []
  | some k =>
      allSquares.foldr
        (fun sq acc =>
          match gs.board sq with
          | some p =>
              if p.color = c ∧ p.pieceType ≠ PieceType.King ∧ sq ≠ k then
                let fd := Movement.fileDiff k sq
                let rd := Movement.rankDiff k sq
                let aligned := fd = 0 ∨ rd = 0 ∨ Movement.absInt fd = Movement.absInt rd
                if aligned then
                  let between := Movement.squaresBetween k sq
                  if between.all (fun s => gs.board s = none) then
                    let stepFile := Movement.signInt (-fd)
                    let stepRank := Movement.signInt (-rd)
                    let rec search (current : Square) (fuel : Nat) : Bool :=
                      match fuel with
                      | 0 => false
                      | Nat.succ n =>
                          match Movement.squareFromInts (current.fileInt + stepFile) (current.rankInt + stepRank) with
                          | none => false
                          | some nxt =>
                              match gs.board nxt with
                              | none => search nxt n
                              | some opp =>
                                  if opp.color = c.opposite then
                                    if stepFile = 0 ∨ stepRank = 0 then
                                      opp.pieceType = PieceType.Rook ∨ opp.pieceType = PieceType.Queen
                                    else
                                      opp.pieceType = PieceType.Bishop ∨ opp.pieceType = PieceType.Queen
                                  else
                                    false
                    if search sq 7 then (sq, k) :: acc else acc
                  else
                    acc
                else
                  acc
              else
                acc
          | none => acc)
        []

def respectsPin (pins : List (Square × Square)) (m : Move) : Bool :=
  match pins.find? (fun p => p.fst = m.fromSq) with
  | some _ =>
      let fd := Movement.absInt (Movement.fileDiff m.fromSq m.toSq)
      let rd := Movement.absInt (Movement.rankDiff m.fromSq m.toSq)
      fd = 0 ∨ rd = 0 ∨ fd = rd
  | none => true

def legalMovesFor (gs : GameState) (sq : Square) : List Move :=
  match gs.board sq with
  | none => []
  | some p =>
      if p.color ≠ gs.toMove then
        []
      else
        let pins := pinnedSquares gs gs.toMove
        pieceTargets gs sq p
          |>.filter (fun m => respectsPin pins m)
          |>.filter (fun m => basicLegalAndSafe gs m)

def allLegalMoves (gs : GameState) : List Move :=
  allSquares.foldr
    (fun sq acc => legalMovesFor gs sq ++ acc)
    []

def GameState.legalMoves (gs : GameState) : List Move :=
  allLegalMoves gs

def isLegalMove (gs : GameState) (m : Move) : Bool :=
  (allLegalMoves gs).any (fun cand => cand = m)

def noLegalMoves (gs : GameState) : Bool :=
  (allLegalMoves gs).isEmpty

def inCheckStatus (gs : GameState) : Bool :=
  inCheck gs.board gs.toMove

def isCheckmate (gs : GameState) : Bool :=
  inCheckStatus gs && noLegalMoves gs

def isStalemate (gs : GameState) : Bool :=
  !(inCheckStatus gs) && noLegalMoves gs

def squareIsLight (sq : Square) : Bool :=
  sq.sumNat % 2 = 0

def isFiftyMoveDraw (gs : GameState) : Bool :=
  gs.halfMoveClock ≥ 100

def threefoldRepetition (gs : GameState) : Bool :=
  let snap := positionSnapshot gs
  let snaps := gs.history ++ [snap]
  snaps.count snap ≥ 3

def fivefoldRepetition (gs : GameState) : Bool :=
  let snap := positionSnapshot gs
  let snaps := gs.history ++ [snap]
  snaps.count snap ≥ 5

def insufficientMaterial (gs : GameState) : Bool :=
  let snap := positionSnapshot gs
  let pieces := snap.pieces
  let nonKing := pieces.filter (fun (_sq, p) => p.pieceType ≠ PieceType.King)
  let hasHeavy := nonKing.any (fun (_, p) => p.pieceType = PieceType.Queen ∨ p.pieceType = PieceType.Rook ∨ p.pieceType = PieceType.Pawn)
  if hasHeavy then
    false
  else
    let bishops := nonKing.filter (fun (_, p) => p.pieceType = PieceType.Bishop)
    let knights := nonKing.filter (fun (_, p) => p.pieceType = PieceType.Knight)
    let total := nonKing.length
    match total with
    | 0 => true -- K vs K
    | 1 =>
        -- K + minor vs K
        true
    | 2 =>
        if bishops.length = 2 then
          -- Draw only if bishops on same color squares
          let colors := bishops.map (fun (sq, _) => squareIsLight sq)
          match colors with
          | c :: cs => cs.all (fun x => x = c)
          | _ => false
        else if knights.length = 2 then
          true -- K+NN vs K is generally not forceable
        else if knights.length = 1 ∧ bishops.length = 1 then
          false -- KB vs KN can force mate scenarios
        else
          false
    | _ => false

def onlyMinors (pieces : List (Square × Piece)) : Bool :=
  pieces.all (fun (_, p) => p.pieceType = PieceType.Bishop ∨ p.pieceType = PieceType.Knight ∨ p.pieceType = PieceType.King)

def loneBishopsSameColor (pieces : List (Square × Piece)) : Bool :=
  let bishops := pieces.filter (fun (_, p) => p.pieceType = PieceType.Bishop)
  let colors := bishops.map (fun (sq, _) => squareIsLight sq)
  match colors with
  | [] => false
  | c :: cs => cs.all (fun x => x = c)

def deadPosition (gs : GameState) : Bool :=
  let snap := positionSnapshot gs
  let pieces := snap.pieces
  let nonKing := pieces.filter (fun (_sq, p) => p.pieceType ≠ PieceType.King)
  let hasHeavy := nonKing.any (fun (_, p) => p.pieceType = PieceType.Queen ∨ p.pieceType = PieceType.Rook ∨ p.pieceType = PieceType.Pawn)
  if hasHeavy then
    false
  else
    let whiteMinors := nonKing.filter (fun (_, p) => p.color = Color.White)
    let blackMinors := nonKing.filter (fun (_, p) => p.color = Color.Black)
    let whiteBishops := whiteMinors.filter (fun (_, p) => p.pieceType = PieceType.Bishop)
    let blackBishops := blackMinors.filter (fun (_, p) => p.pieceType = PieceType.Bishop)
    let whiteKnights := whiteMinors.filter (fun (_, p) => p.pieceType = PieceType.Knight)
    let blackKnights := blackMinors.filter (fun (_, p) => p.pieceType = PieceType.Knight)
    let bishopColors (bs : List (Square × Piece)) : List Bool := bs.map (fun (sq, _) => squareIsLight sq)
    let diffColorBishops (bs : List (Square × Piece)) : Bool :=
      match bishopColors bs with
      | c :: cs => ¬cs.all (fun x => x = c)
      | _ => false
    let sideCanMate (bishops knights : List (Square × Piece)) : Bool :=
      (bishops.length ≥ 2 ∧ diffColorBishops bishops) ∨ (bishops.length ≥ 1 ∧ knights.length ≥ 1)
    if sideCanMate whiteBishops whiteKnights || sideCanMate blackBishops blackKnights then
      false
    else
      let total := nonKing.length
      match total with
      | 0 => true
      | 1 => true
      | 2 =>
          if whiteBishops.length = 1 ∧ blackBishops.length = 1 then true
          else if whiteKnights.length = 1 ∧ blackKnights.length = 1 then true
          else if whiteBishops.length = 1 ∧ blackKnights.length = 1 then true
          else if whiteKnights.length = 1 ∧ blackBishops.length = 1 then true
          else true
      | 3 => true
      | _ => false

def isDraw (gs : GameState) : Bool :=
  isStalemate gs || isFiftyMoveDraw gs || threefoldRepetition gs || insufficientMaterial gs || deadPosition gs

def isSeventyFiveMoveDraw (gs : GameState) : Bool :=
  gs.halfMoveClock ≥ 150

def drawStatus (gs : GameState) : (Bool × Bool × List String × List String) :=
  let autoReasons :=
    [ (isStalemate gs, "stalemate")
    , (isSeventyFiveMoveDraw gs, "75-move")
    , (fivefoldRepetition gs, "fivefold repetition")
    , (insufficientMaterial gs, "insufficient material") ]
    |>.filterMap (fun (b, msg) => if b then some msg else none)
  let claimReasons :=
    [ (isFiftyMoveDraw gs, "50-move claimable")
    , (threefoldRepetition gs, "threefold claimable") ]
    |>.filterMap (fun (b, msg) => if b then some msg else none)
  (¬autoReasons.isEmpty || ¬claimReasons.isEmpty, ¬autoReasons.isEmpty, claimReasons, autoReasons)

inductive GameResult where
  | ongoing
  | whiteWin
  | blackWin
  | drawAuto (reasons : List String)
  | drawClaim (reasons : List String)
deriving Repr, DecidableEq

def interpretResult (gs : GameState) : GameResult :=
  if let some r := gs.result then
    match r with
    | "1-0" => GameResult.whiteWin
    | "0-1" => GameResult.blackWin
    | "1/2-1/2" => GameResult.drawAuto ["recorded"]
    | _ => GameResult.ongoing
  else
    let (_has, autoDraw, claim, auto) := drawStatus gs
    if autoDraw then GameResult.drawAuto auto
    else if ¬claim.isEmpty then GameResult.drawClaim claim
    else GameResult.ongoing



def finalizeResult (before : GameState) (after : GameState) : GameState :=
  if after.result.isSome then after
  else if isCheckmate after then
    let winner := before.toMove
    let res := if winner = Color.White then "1-0" else "0-1"
    { after with result := some res }
  else if isStalemate after || isSeventyFiveMoveDraw after || fivefoldRepetition after || deadPosition after then
    { after with result := some "1/2-1/2" }
  else after

def GameState.playMove (gs : GameState) (m : Move) : GameState :=
  finalizeResult gs (gs.movePiece m)

def applyLegalMove? (gs : GameState) (m : Move) : Option GameState :=
  if isLegalMove gs m then
    some (GameState.playMove gs m)
  else
    none

def applyLegalMove (gs : GameState) (m : Move) : Except String GameState :=
  match applyLegalMove? gs m with
  | some nxt => Except.ok nxt
  | none => Except.error s!"Illegal move: {repr m}"

def applyLegalMoves (gs : GameState) (moves : List Move) : Except String GameState :=
  moves.foldlM (fun acc m => applyLegalMove acc m) gs
def perft : GameState → Nat → Nat
  | _, 0 => 1
  | gs, Nat.succ d =>
      (allLegalMoves gs).foldl (fun acc m => acc + perft (GameState.playMove gs m) d) 0

end Rules
end Chess
