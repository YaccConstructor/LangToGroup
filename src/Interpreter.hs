

module Interpreter where

import TMTypes
import Tape
import qualified Data.Map.Lazy as Map (lookup)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, findIndex, map, elemAt)

newtype MetaString = MS {unMS :: String}
    deriving (Eq)

instance Show MetaString where
    show = unMS

newtype MetaChar = MC {unMC :: Char}
    deriving (Eq, Ord)

instance Show MetaChar where
    show = return . unMC

data WorkingState = WS {
        turingMachine :: TuringMachine,
        currentState :: State,
        tape :: Tape MetaString,
        alphabet :: Set String
    }

blankStr :: String
blankStr = "."

step :: WorkingState -> Maybe WorkingState
step (WS tm@(TM qs) q t a) = do
    let cs = unMS $ top t
        s = if cs == blankStr
            then S 0
            else S (Set.findIndex cs a + 1)
    (sm, q') <- Map.lookup (q, s) qs
    let t' = case sm of
         C (S 0) -> t {top = MS blankStr}
         C (S i) -> t {top = MS $ Set.elemAt (i - 1) a}
         L       -> toLeft  t
         R       -> toRight t
    return $ WS tm q' t' a

start :: (Show a, Ord a) => TuringMachine -> [a] -> Int -> WorkingState
start tm word ind = startWithAlphabet tm word ind $ Set.fromList word

startWithAlphabet :: Show a => TuringMachine -> [a] -> Int -> Set a -> WorkingState
startWithAlphabet tm word ind =
    WS tm startState (Tape.fromList ((MS . show) <$> word) ind (MS blankStr)) . Set.map show

states :: WorkingState -> [WorkingState]
states ws =
    let mws' = step ws
    in  case mws' of
             Nothing  -> [ws]
             Just ws' -> ws : states ws'
