import Init.Data.List
import Init.Data.Char

namespace Chess

abbrev File := Fin 8
abbrev Rank := Fin 8

def File.toNat (f : File) : Nat := f.val
def Rank.toNat (r : Rank) : Nat := r.val

structure Square where
  file : File
  rank : Rank
deriving DecidableEq, Repr, Inhabited

namespace Square

  def fileNat (s : Square) : Nat := s.file.toNat
  def rankNat (s : Square) : Nat := s.rank.toNat
  def sumNat (s : Square) : Nat := s.file.toNat + s.rank.toNat

  def fileInt (s : Square) : Int := Int.ofNat s.file.toNat
  def rankInt (s : Square) : Int := Int.ofNat s.rank.toNat

  def algebraic (s : Square) : String :=
    let fileChar := Char.ofNat ('a'.toNat + s.fileNat)
    fileChar.toString ++ toString (s.rankNat + 1)

  def mk? (file rank : Nat) : Option Square :=
    if hf : file < 8 then
      if hr : rank < 8 then
        some { file := ⟨file, hf⟩, rank := ⟨rank, hr⟩ }
      else
        none
    else
      none

  def mkUnsafe (file rank : Nat) : Square :=
    match mk? file rank with
    | some sq => sq
    | none => panic! "Square.mkUnsafe: index out of range"

  def fileChar (s : Square) : Char :=
    Char.ofNat ('a'.toNat + s.fileNat)

  def rankChar (s : Square) : Char :=
    Char.ofNat ('1'.toNat + s.rankNat)

  def fromAlgebraic? (coord : String) : Option Square :=
    match coord.toList with
    | f :: r :: [] =>
        let file := f.toNat - 'a'.toNat
        let rank := r.toNat - '1'.toNat
        if file < 8 ∧ rank < 8 then
          mk? file rank
        else
          none
    | _ => none

  def all : List Square :=
    (List.range 8).foldr
      (fun file acc =>
        (List.range 8).foldr
          (fun rank inner => Square.mkUnsafe file rank :: inner)
          acc)
      []

end Square

def allSquares : List Square := Square.all

def whiteKingStart : Square := Square.mkUnsafe 4 0
def whiteQueenRookStart : Square := Square.mkUnsafe 0 0
def whiteKingRookStart : Square := Square.mkUnsafe 7 0
def blackKingStart : Square := Square.mkUnsafe 4 7
def blackQueenRookStart : Square := Square.mkUnsafe 0 7
def blackKingRookStart : Square := Square.mkUnsafe 7 7

inductive Color where
  | White
  | Black
deriving DecidableEq, Repr, Inhabited

namespace Color

  def opposite : Color → Color
    | White => Black
    | Black => White

  theorem opposite_opposite (c : Color) : opposite (opposite c) = c := by
    cases c <;> rfl

end Color

inductive PieceType where
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
deriving DecidableEq, Repr

structure Piece where
  pieceType : PieceType
  color : Color
deriving Repr, DecidableEq

namespace Piece

  instance : Inhabited Piece :=
    ⟨{ pieceType := PieceType.King, color := Color.White }⟩

end Piece

abbrev Board := Square → Option Piece

def emptyBoard : Board := fun _ => none

namespace Board

  def update (b : Board) (sq : Square) (p : Option Piece) : Board :=
    fun target => if target = sq then p else b target

  theorem update_eq (b : Board) (sq : Square) (p : Option Piece) :
      (b.update sq p) sq = p := by
    simp [update]

  theorem update_ne (b : Board) (sq : Square) (p : Option Piece) {target : Square}
      (h : target ≠ sq) : (b.update sq p) target = b target := by
    simp [update, h]

  def fromList (ps : List (Square × Piece)) : Board :=
    ps.foldl (fun b entry => b.update entry.fst (some entry.snd)) emptyBoard

end Board

instance : Inhabited Board := ⟨emptyBoard⟩

structure CastlingRights where
  whiteKingSide : Bool := true
  whiteQueenSide : Bool := true
  blackKingSide : Bool := true
  blackQueenSide : Bool := true
deriving Inhabited, DecidableEq

structure PositionSnapshot where
  pieces : List (Square × Piece)
  toMove : Color
  castlingRights : CastlingRights
  enPassantTarget : Option Square
deriving DecidableEq

structure GameState where
  board : Board := emptyBoard
  toMove : Color := Color.White
  halfMoveClock : Nat := 0
  fullMoveNumber : Nat := 1
  enPassantTarget : Option Square := none
  castlingRights : CastlingRights := {}
  history : List PositionSnapshot := []
  result : Option String := none
deriving Inhabited

structure Move where
  piece : Piece
  fromSq : Square
  toSq : Square
  isCapture : Bool := false
  promotion : Option PieceType := none
  isCastle : Bool := false
  castleRookFrom : Option Square := none
  castleRookTo : Option Square := none
  isEnPassant : Bool := false
deriving Repr, DecidableEq

def startingPieces : List (Square × Piece) :=
  let mk (f r : Nat) (pt : PieceType) (c : Color) : Square × Piece :=
    (Square.mkUnsafe f r, { pieceType := pt, color := c })
  let backRank (r : Nat) (c : Color) : List (Square × Piece) :=
    [ (0, PieceType.Rook), (1, PieceType.Knight), (2, PieceType.Bishop), (3, PieceType.Queen),
      (4, PieceType.King), (5, PieceType.Bishop), (6, PieceType.Knight), (7, PieceType.Rook) ]
      |>.map (fun (f, pt) => mk f r pt c)
  let pawns (r : Nat) (c : Color) : List (Square × Piece) :=
    (List.range 8).map (fun f => mk f r PieceType.Pawn c)
  backRank 0 Color.White ++ pawns 1 Color.White ++ backRank 7 Color.Black ++ pawns 6 Color.Black

def startingBoard : Board :=
  Board.fromList startingPieces

def standardGameState : GameState :=
  { board := startingBoard
    toMove := Color.White
    halfMoveClock := 0
    fullMoveNumber := 1
    enPassantTarget := none
    castlingRights := {}
    history := [] }

end Chess
