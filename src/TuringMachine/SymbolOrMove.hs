module TuringMachine.SymbolOrMove (
    MoveOr (S, M),
    SymbolOrMove,
    ShowedSymbolOrMove,
    getSymbol,
    isSymbol,
    getMove,
    isMove,
    module TuringMachine.Symbol,
    module TuringMachine.ShowedSymbol,
    module TuringMachine.Move,
  ) where

import TuringMachine.Symbol
import TuringMachine.ShowedSymbol
import TuringMachine.Move

import Data.Maybe (isJust)

data MoveOr s =
      S s
    | M Move
    deriving (Eq, Ord)

type SymbolOrMove = MoveOr Symbol

type ShowedSymbolOrMove = MoveOr ShowedSymbol

getSymbol :: MonadFail m => MoveOr s -> m s
getSymbol (S s) = return s
getSymbol _ = fail "Symbol expected, but Move was found"

isSymbol :: MoveOr s -> Bool
isSymbol = isJust . getSymbol

getMove :: MonadFail m => MoveOr s -> m Move
getMove (M m) = return m
getMove _ = fail "Move expected, but Symbol was found"

isMove :: MoveOr s -> Bool
isMove = isJust . getMove
