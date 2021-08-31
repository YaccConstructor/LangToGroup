module TuringMachine.SymbolOrMove (
    SymbolOrMove (S, M),
    isSymbol,
    isMove,
    module TuringMachine.Symbol,
    module TuringMachine.Move,
  ) where

import TuringMachine.Symbol
import TuringMachine.Move

data SymbolOrMove = S Symbol | M Move
    deriving (Eq, Ord, Show)

isSymbol :: SymbolOrMove -> Bool
isSymbol (S _) = True
isSymbol _ = False

isMove :: SymbolOrMove -> Bool
isMove = not . isSymbol
