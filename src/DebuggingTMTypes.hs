{-# LANGUAGE LambdaCase #-}

module DebuggingTMTypes where

import TuringMachine
import GrammarType
  
import qualified Data.List as List
import Data.Containers.ListUtils (nubOrd)
import Control.Monad (forM)

newtype DebuggingState = DState String
        deriving (Eq, Ord, Show)

newtype DebuggingSymbol = DSymbol String
        deriving (Eq, Ord, Show)

data DebuggingMove = D DebuggingSymbol | L | R
    deriving (Eq, Ord, Show)

newtype DebuggingQuadruples = DQuadruples (Map (DebuggingState, DebuggingSymbol) (DebuggingMove, DebuggingState))
        deriving (Eq, Ord, Show)

newtype DebuggingTuringMachine = DTM DebuggingQuadruples
    deriving (Show, Eq)

finalDState :: DebuggingState
finalDState = DState "accepted"

startDState :: DebuggingState
startDState = DState "qWriteStartCounter"

newtype SymbolsPair = SymbolsPair (Nonterminal, Int, Bool, GrammarType.Symbol, GrammarType.Symbol)
    deriving (Eq, Show)

convertToTuringMachine :: MonadFail m => DebuggingTuringMachine -> m TuringMachine
convertToTuringMachine tm@(DTM (DQuadruples quadruplesMap)) = do
    let states' = getStates tm
        (alphabet', symbols') = getSymbols tm
    quadruplesList <- forM (toList quadruplesMap) $
        \((state1, symbol1), (symbolOrMove, state2)) -> do
            qf <- states'  !? state1
            s  <- symbols' !? symbol1
            sm <- case symbolOrMove of
                    L -> return $ M toLeft
                    R -> return $ M toRight
                    D symbol2 -> S <$> symbols' !? symbol2
            qt <- states'  !? state2
            return ((qf, s), (sm, qt))
    return $ turingMachine (fromList quadruplesList) emptyC alphabet'

getStates :: DebuggingTuringMachine -> IsoMap DebuggingState State
getStates (DTM (DQuadruples qdrs)) = let
    states' = concatMap (\((DState s1, _), (_, DState s2)) -> [s1, s2]) $
        toList qdrs
    (DState final') = finalDState
    (DState start') = startDState
    dstates = map DState $ nubOrd $ final' \> start' \> states'
    in fromList $ zip (finalDState : startDState : dstates) [minBound]

getSymbols :: DebuggingTuringMachine -> (Alphabet, IsoMap DebuggingSymbol TuringMachine.Symbol)
getSymbols (DTM (DQuadruples qdrs)) = let
    blank = [blankChar]
    dsymbols' = concatMap (\case
        ((_, DSymbol s), (D (DSymbol s'), _)) -> [s, s']
        ((_, DSymbol s), _) -> [s]) $ toList qdrs
    dsymbols = map DSymbol $ List.sort $ blank \> nubOrd dsymbols'
    in (
        fromList $ zip (Nothing : map Just ['a'..]) [minBound..],
        fromList $ zip (DSymbol blank : dsymbols)   [minBound..]
      )
