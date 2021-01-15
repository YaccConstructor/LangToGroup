module TMTypes where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map (fromList)

newtype State = Q Int
    deriving (Eq, Ord, Show)

startState :: State
startState = Q 1

finalState :: State
finalState = Q 0

errorState :: State
errorState = Q (-1)

newtype Symbol = S Int
    deriving (Eq, Ord, Show)

emptySymbol :: Symbol
emptySymbol = S 0

data SymbolMove = C Symbol | L | R
    deriving (Eq, Ord, Show)

type Quadruple = ((State, Symbol), (SymbolMove, State))

type Quadruples = Map (State, Symbol) (SymbolMove, State)

newtype TuringMachine = TM Quadruples
    deriving (Show, Eq)

fromList :: [Quadruple] -> TuringMachine
fromList = TM . Map.fromList
