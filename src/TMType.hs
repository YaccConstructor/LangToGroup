module TMType where

import Data.Set (Set)
import qualified Data.Set as Set
-- input, tape alphabets
newtype InputAlphabet = InputAlphabet (Set String)
    deriving (Eq, Ord, Show)
newtype TapeAlphabet = TapeAlphabet (Set String)
    deriving (Eq, Ord, Show)
-- state of TM
newtype State = State String
    deriving (Eq, Ord, Show)
-- k - vector of tapes states
newtype MultiTapeStates = MultiTapeStates [Set State]
    deriving (Eq, Ord, Show)
-- k - vector of start states
newtype StartStates = StartStates [State]
    deriving (Eq, Ord, Show)
-- k - vector of end states
newtype AccessStates = AccessStates [State]
    deriving (Eq, Ord, Show)
-- command of TM for single tape
data SingleTapeCommand = SingleTapeCommand ((String, State, String), (String, State, String)) | NoCommand
    deriving (Eq, Ord, Show)
-- commands
newtype Commands = Commands (Set [SingleTapeCommand])
    deriving (Eq, Ord, Show)
-- leftmost square on every tape
leftBoundingLetter = "α"
-- rightmost square on every tape
rightBoundingLetter = "ω"
emptySymbol = "ε"
-- TM
newtype TM = TM (InputAlphabet, [TapeAlphabet], MultiTapeStates, Commands, StartStates, AccessStates)
    deriving (Eq, Ord, Show)