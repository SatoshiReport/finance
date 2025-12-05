import Chess.Core

namespace Chess

namespace Movement

def fileDiff (source target : Square) : Int := source.fileInt - target.fileInt
def rankDiff (source target : Square) : Int := source.rankInt - target.rankInt

def absInt (x : Int) : Int := if 0 ≤ x then x else -x

def isRookMove (source target : Square) : Prop :=
  source ≠ target ∧
    ((fileDiff source target = 0 ∧ rankDiff source target ≠ 0) ∨ (rankDiff source target = 0 ∧ fileDiff source target ≠ 0))

instance instDecidableIsRookMove (source target : Square) : Decidable (isRookMove source target) := by
  unfold isRookMove
  infer_instance

def isDiagonal (source target : Square) : Prop :=
  source ≠ target ∧ absInt (fileDiff source target) = absInt (rankDiff source target)

instance instDecidableIsDiagonal (source target : Square) : Decidable (isDiagonal source target) := by
  unfold isDiagonal
  infer_instance

def isQueenMove (source target : Square) : Prop :=
  isRookMove source target ∨ isDiagonal source target
instance instDecidableIsQueenMove (source target : Square) : Decidable (isQueenMove source target) := by
  unfold isQueenMove
  infer_instance

def isKingStep (source target : Square) : Prop :=
  source ≠ target ∧ absInt (fileDiff source target) ≤ 1 ∧ absInt (rankDiff source target) ≤ 1

def isKnightMove (source target : Square) : Prop :=
  source ≠ target ∧
    ((absInt (fileDiff source target) = 1 ∧ absInt (rankDiff source target) = 2) ∨
     (absInt (fileDiff source target) = 2 ∧ absInt (rankDiff source target) = 1))

def isKingStepBool (source target : Square) : Bool :=
  if source = target then false else
    let fileDelta := absInt (fileDiff source target)
    let rankDelta := absInt (rankDiff source target)
    if fileDelta ≤ 1 then
      if rankDelta ≤ 1 then true else false
    else
      false

def isKnightMoveBool (source target : Square) : Bool :=
  if source = target then false else
    let fileDelta := absInt (fileDiff source target)
    let rankDelta := absInt (rankDiff source target)
    if fileDelta = 1 then
      if rankDelta = 2 then true else false
    else if fileDelta = 2 then
      if rankDelta = 1 then true else false
    else
      false

def pawnDirection (c : Color) : Int :=
  if c = Color.White then 1 else -1

def isPawnAdvance (c : Color) (source target : Square) : Prop :=
  source ≠ target ∧ fileDiff source target = 0 ∧
    (rankDiff source target = pawnDirection c ∨ rankDiff source target = 2 * pawnDirection c)

def isPawnCapture (c : Color) (source target : Square) : Prop :=
  source ≠ target ∧ absInt (fileDiff source target) = 1 ∧ rankDiff source target = pawnDirection c
instance instDecidableIsPawnCapture (c : Color) (source target : Square) : Decidable (isPawnCapture c source target) := by
  unfold isPawnCapture
  infer_instance

def kingTargets (source : Square) : List Square :=
  Square.all.filter fun target => isKingStepBool source target

def knightTargets (source : Square) : List Square :=
  Square.all.filter fun target => isKnightMoveBool source target

def signInt (x : Int) : Int :=
  if x = 0 then 0 else if x > 0 then 1 else -1

def squareFromInts (f r : Int) : Option Square :=
  if 0 ≤ f ∧ f < 8 ∧ 0 ≤ r ∧ r < 8 then
    some <| Square.mkUnsafe (Int.toNat f) (Int.toNat r)
  else
    none

def squaresBetween (source target : Square) : List Square :=
  if isDiagonal source target then
    let fd := fileDiff source target
    let rd := rankDiff source target
    let steps := Int.natAbs fd
    let stepFile := signInt (-fd)
    let stepRank := signInt (-rd)
    if steps ≤ 1 then
      []
    else
      (List.range (steps - 1)).filterMap fun idx =>
        let step := Int.ofNat (idx + 1)
        squareFromInts (source.fileInt + stepFile * step) (source.rankInt + stepRank * step)
  else if isRookMove source target then
    let fd := fileDiff source target
    let rd := rankDiff source target
    let stepFile := signInt (-fd)
    let stepRank := signInt (-rd)
    let steps := Int.natAbs fd + Int.natAbs rd
    if steps ≤ 1 then
      []
    else
      (List.range (steps - 1)).filterMap fun idx =>
        let step := Int.ofNat (idx + 1)
        squareFromInts (source.fileInt + stepFile * step) (source.rankInt + stepRank * step)
  else
    []

theorem rook_move_straight {source target : Square} (h : isRookMove source target) :
    fileDiff source target = 0 ∨ rankDiff source target = 0 := by
  cases h.2 with
  | inl hcase => exact Or.inl hcase.left
  | inr hcase => exact Or.inr hcase.left

theorem knight_move_distance {source target : Square} (h : isKnightMove source target) :
    absInt (fileDiff source target) + absInt (rankDiff source target) = 3 := by
  cases h.2 with
  | inl hcase =>
    simp [hcase.left, hcase.right]
  | inr hcase =>
    simp [hcase.left, hcase.right]

theorem king_step_bounds {source target : Square} (h : isKingStep source target) :
    absInt (fileDiff source target) ≤ 1 ∧ absInt (rankDiff source target) ≤ 1 := by
  exact ⟨h.2.1, h.2.2⟩

theorem pawn_advance_direction {c : Color} {source target : Square} (h : isPawnAdvance c source target) :
    rankDiff source target = pawnDirection c ∨ rankDiff source target = 2 * pawnDirection c :=
  h.2.2

theorem pawn_capture_offset {c : Color} {source target : Square} (h : isPawnCapture c source target) :
    absInt (fileDiff source target) = 1 :=
  h.2.1

theorem pawn_capture_forward {c : Color} {source target : Square} (h : isPawnCapture c source target) :
    rankDiff source target = pawnDirection c :=
  h.2.2

end Movement

end Chess
