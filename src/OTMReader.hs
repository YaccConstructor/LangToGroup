module OTMReader where

import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

type Pair a = (a, a)

type OTMTriple = (Square, State, Square)

type OTMInfo = (Set Square, Set State, Map (Pair OTMTriple) [Pair OTMTriple], Pair State, Pair State)

type OTMReader a = OTMInfo -> a

runOTMReader :: OTMReader a -> TM -> a
runOTMReader otmr (TM (_, tapeSymbols, MultiTapeStates tapeStates, Commands tapeCommands, StartStates startStates, AccessStates accessStates)) =
    otmr (symbols, states, commands, startState, finalState) where
        unTapeAlphabet (TapeAlphabet ta) = ta
        pair [x, y] = (x, y)
        symbols = Set.unions $ map (Set.insert LBS . Set.insert RBS . unTapeAlphabet) tapeSymbols
        states = Set.unions tapeStates
        commands = Map.fromListWith (++) $ (\[SingleTapeCommand (f1, t1), SingleTapeCommand (f2, t2)] -> ((f1, f2), [(t1, t2)])) <$> Set.toList tapeCommands
        startState = pair startStates
        finalState = pair accessStates

getSymbols :: OTMReader (Set Square)
getSymbols (symbols, _, _, _, _) = symbols

getStates :: OTMReader (Set State)
getStates (_, states, _, _, _) = states

getCommands :: OTMReader (Map (Pair OTMTriple) [Pair OTMTriple])
getCommands (_, _, commands, _, _) = commands

getStartState :: OTMReader (Pair State)
getStartState (_, _, _, startState, _) = startState

getFinalState :: OTMReader (Pair State)
getFinalState (_, _, _, _, finalState) = finalState

forFstTape :: OTMReader (Pair a) -> OTMReader a
forFstTape = fmap fst

forSndTape :: OTMReader (Pair a) -> OTMReader a
forSndTape = fmap snd
