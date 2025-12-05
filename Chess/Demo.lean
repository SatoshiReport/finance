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

def demoStart : GameState := standardGameState
def demoStartFen : String := toFEN demoStart

def demoScholarsPGN : String :=
  "[Event \"Scholars\"]\n\n1. e4 e5 2. Qh5 Nc6 3. Bc4 Nf6 4. Qxf7#"

def demoAfterE4 : Except String GameState := applySAN demoStart "e4"
def demoAfterScholars : Except String GameState := playPGN demoScholarsPGN

def demoPerftDepths : List Nat := [1, 2, 3]
def demoPerftResults : List (Nat × Nat) :=
  demoPerftDepths.map (fun d => (d, perft standardGameState d))

#eval demoStartFen
#eval demoStart.legalMoves.length
#eval match demoAfterE4 with
  | .ok gs => toFEN gs
  | .error e => s!"SAN apply failed: {e}"
#eval match demoAfterScholars with
  | .ok gs => (toFEN gs, isCheckmate gs, isStalemate gs)
  | .error e => s!"PGN failed: {e}"
#eval demoPerftResults
