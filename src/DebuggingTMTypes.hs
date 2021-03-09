{-# LANGUAGE LambdaCase #-}

module DebuggingTMTypes where

import qualified Data.Map as Map
import qualified Data.List as List
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
finalDState = DState "Done"

convertToTuringMachine :: DebuggingTuringMachine -> TuringMachine
convertToTuringMachine tm@(DTM (DQuadruples quadruplesMap)) = let
    states = getStates tm
    symbols = getSymbols tm
    quadruplesMap' = Map.mapKeys (\(state, symbol) -> let
        stateIndex = getStateIndex state states
        symbolIndex = getSymbolIndex symbol symbols
        in (Q stateIndex, S symbolIndex)) quadruplesMap
    quadruplesMap'' = Map.map (\(move, state) -> let
        stateIndex = getStateIndex state states
        newMove = case move of
            DebuggingTMTypes.L -> TMTypes.L
            DebuggingTMTypes.R -> TMTypes.R
            D s -> C $ S $ getSymbolIndex s symbols
        in (newMove, Q stateIndex)) quadruplesMap'
    in TM quadruplesMap''

getStateIndex :: DebuggingState -> [DebuggingState] -> Int
getStateIndex state states =
    case List.elemIndex state states of
      Just index -> index
      Nothing -> error "No such state. Something went wrong during convertation to Turing machine."

getSymbolIndex :: DebuggingSymbol -> [DebuggingSymbol] -> Int
getSymbolIndex symbol symbols =
    case List.elemIndex symbol symbols of
      Just index -> index
      Nothing -> error "No such state. Something went wrong during convertation to Turing machine."

getStates :: DebuggingTuringMachine -> [DebuggingState]
getStates (DTM (DQuadruples qdrs)) = let
    states' = map (\(_, (_, DState state)) -> state) $ Map.toList qdrs
    states'' = map (\((DState state, _), _) -> state) $ Map.toList qdrs
    in map DState $ List.nub $ states' ++ states''

getSymbols :: DebuggingTuringMachine -> [DebuggingSymbol]
getSymbols (DTM (DQuadruples qdrs)) = let
    symbols' = map (\case
        (_, (D (DSymbol s), _)) -> s
        _ -> "") $ Map.toList qdrs
    filteredEmpty = filter (/= "") symbols'
    symbols'' = map (\((_, DSymbol s), _) -> s) $ Map.toList qdrs
    in map DSymbol $ List.nub $ filteredEmpty ++ symbols''

