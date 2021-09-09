{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module DebuggingTMTypes where

import TuringMachine
import TuringMachine.Optimization
import GrammarType

import Control.Monad (forM)

newtype DebuggingState = DState String
        deriving (Eq, Ord, Show)

newtype DebuggingSymbol = DSymbol { unDSymbol :: String }
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

convertToTuringMachine ::
    MonadFail m =>
    DebuggingTuringMachine ->
    m TuringMachine
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
    return $ safeOptimize O1 $ turingMachine (fromList quadruplesList) emptyC alphabet'

getStates :: DebuggingTuringMachine -> IsoMap DebuggingState State
getStates (DTM (DQuadruples qdrs)) = let
    dstates :: Set DebuggingState =
        fromList $
            concatMap (\((s1, _), (_, s2)) -> [s1, s2]) $
                toList qdrs
    dstates' = finalDState \> startDState \> dstates
    in fromList $ zip (finalDState +> startDState +> toList dstates') [minBound..]

getSymbols :: DebuggingTuringMachine -> (Alphabet, IsoMap DebuggingSymbol TuringMachine.Symbol)
getSymbols (DTM (DQuadruples qdrs)) = let
    blank' = DSymbol "."
    dsymbols :: Set DebuggingSymbol =
        fromList $
            concatMap (\case
                    ((_, s), (D s', _)) -> [s, s']
                    ((_, s), _) -> [s]) $
                toList qdrs
    dsymbols' = blank' \> dsymbols
    in (
        fromList $ zip (blank  : (read <$> unDSymbol <$> toList dsymbols')) [minBound..],
        fromList $ zip (blank' : toList dsymbols')   [minBound..]
      )
