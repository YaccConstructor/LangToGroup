module Interpreter where

import TMTypes
import Tape
import qualified Data.Map.Lazy as Map (lookup)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, findIndex)

data WorkingState = WS {
        turingMachine :: TuringMachine,
        currentState :: State,
        tape :: Tape Symbol
    }

step :: WorkingState -> Maybe WorkingState
step (WS tm@(TM qs) q t) = do
    let s = top t
    (sm, q') <- Map.lookup (q, s) qs
    let t' = case sm of
         C s' -> t {top = s'}
         L    -> toLeft  t
         R    -> toRight t
    return $ WS tm q' t'

start :: Ord a => TuringMachine -> [a] -> Int -> (Set (Maybe a), WorkingState)
start tm word ind =
    let charSet = Set.fromList $ Nothing : (Just <$> word)
        toSymbol = \char -> S $ Set.findIndex char charSet
        tape = Tape.fromList (toSymbol <$> Just <$> word) ind emptySymbol
    in  (charSet, WS tm startState tape)

states :: WorkingState -> [WorkingState]
states ws =
    let mws' = step ws
    in  case mws' of
             Nothing  -> [ws]
             Just ws' -> ws : states ws'
