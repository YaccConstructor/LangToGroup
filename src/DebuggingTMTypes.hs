module DebuggingTMTypes where

import qualified Data.Map as Map
import Data.Bifunctor as Bifunctor
import Data.Char as Char
import TMTypes

-- firstly generating descriptional states (name of states describes, which activities TM does in this state),
-- after convert it to state Q_index from TMTypes module via bijective transformation
-- c |-> chr $ c + (ord 'a' - 1)
-- (transformation is bijective, since will use only chars 'A'..'Z','a'..'z', '1'..'9', '-' with disjoint 
-- prefixes for debugging names)
newtype DebuggingState = DState String
        deriving (Eq, Ord, Show)

newtype DebuggingSymbol = DSymbol String
        deriving (Eq, Ord, Show)

data DebuggingMove = D DebuggingSymbol | L | R
    deriving (Eq, Ord, Show)

newtype DebuggingQuadruples = DQuadruples (Map.Map (DebuggingState, DebuggingSymbol) (DebuggingMove, DebuggingState))
        deriving (Eq, Ord, Show)
        

newtype DebuggingTuringMachine = DTM DebuggingQuadruples
    deriving (Show, Eq)

finalDState :: DebuggingState
finalDState = DState " "

convertToTuringMachine :: DebuggingTuringMachine -> TuringMachine
convertToTuringMachine (DTM (DQuadruples quadruplesMap)) = let
    quadruplesMap' = Map.mapKeys (bimap convertToState convertToSymbol) quadruplesMap
    quadruplesMap'' = Map.map (bimap convertToSymbolMove convertToState) quadruplesMap'
    in TM quadruplesMap''

convertToSymbolMove :: DebuggingMove -> SymbolMove
convertToSymbolMove (D (DSymbol symbol)) = C $ S (foldCodesToNumber $ map ord symbol)
convertToSymbolMove DebuggingTMTypes.L = TMTypes.L
convertToSymbolMove DebuggingTMTypes.R = TMTypes.R

convertToSymbol :: DebuggingSymbol -> Symbol
convertToSymbol (DSymbol " ") = emptySymbol
convertToSymbol (DSymbol symbol) = let
    symbol' = foldCodesToNumber $ map Char.ord symbol
    in S symbol'    

convertToState :: DebuggingState -> State
convertToState (DState state) = let
    state' = foldCodesToNumber $ map Char.ord state
    in Q state'    

-- overflow
foldCodesToNumber :: [Int] -> Int
foldCodesToNumber codes = read (concatMap show codes) :: Int    