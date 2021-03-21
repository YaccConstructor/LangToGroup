{-# LANGUAGE LambdaCase #-}

module DebuggingTMTypes where

import TMTypes
import GrammarType
  
import qualified Data.Map as Map
import qualified Data.List as List

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
finalDState = DState "accepted"

errorDState :: DebuggingState
errorDState = DState "notAccepted"

startDState :: DebuggingState
startDState = DState "qWriteStartCounter"

newtype SymbolsPair = SymbolsPair (Nonterminal, Int, Bool, GrammarType.Symbol, GrammarType.Symbol)
    deriving (Eq, Show)

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
      Just index -> index - 1 -- since states numeration starts from -1
      Nothing -> error "No such state. Something went wrong during convertation to Turing machine."

getSymbolIndex :: DebuggingSymbol -> [DebuggingSymbol] -> Int
getSymbolIndex symbol symbols =
    case List.elemIndex symbol symbols of
      Just index -> index
      Nothing -> error "No such symbol. Something went wrong during convertation to Turing machine."

getStates :: DebuggingTuringMachine -> [DebuggingState]
getStates (DTM (DQuadruples qdrs)) = let
    states' = map (\(_, (_, DState state)) -> state) $ Map.toList qdrs
    states'' = map (\((DState state, _), _) -> state) $ Map.toList qdrs
    (DState final') = finalDState
    (DState start') = startDState
    (DState error') = errorDState
    in map DState $ List.nub $ error' : final' : start' : states' ++ states''

getSymbols :: DebuggingTuringMachine -> [DebuggingSymbol]
getSymbols (DTM (DQuadruples qdrs)) = let
    blank = "."
    symbols' = map (\case
        (_, (D (DSymbol s), _)) -> s
        _ -> "") $ Map.toList qdrs
    filteredEmpty = filter (/= "") symbols'
    symbols'' = map (\((_, DSymbol s), _) -> s) $ Map.toList qdrs
    list' = List.sort $ List.nub $ filteredEmpty ++ symbols''
    list = List.delete blank list'
    in map DSymbol $ blank : list

