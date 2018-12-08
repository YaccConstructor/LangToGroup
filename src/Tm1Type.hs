module Tm1Type where

import Data.Set (Set)
import qualified Data.Set as Set
-- letter
newtype Letter = Letter Char
    deriving (Eq, Ord, Show)
-- input, tape alphabets
newtype InputAlphabet = InputAlphabet (Set Letter)
    deriving (Eq, Ord, Show)
newtype TapeAlphabet = TapeAlphabet (Set Letter)
    deriving (Eq, Ord, Show)
-- state of TM1
newtype State = State String
    deriving (Eq, Ord, Show)
-- TM1 set states
newtype States = States (Set State)
    deriving (Eq, Ord, Show)
-- k - vector of tapes states
newtype MultiTapeStates = MultiTapeStates [States]
    deriving (Eq, Ord, Show)
-- k - vector of start states
newtype StartStates = StartStates [State]
    deriving (Eq, Ord, Show)
-- k - vector of end states
newtype AccessStates = AccessStates [State]
    deriving (Eq, Ord, Show)
-- command of TM1 for single tape
data SingleTapeCommand = SingleTapeCommand ((Letter, State, Letter), (Letter, State, Letter)) | NoCommand
    deriving (Eq, Ord, Show)
-- command of TM1
newtype Command = Command [SingleTapeCommand]
    deriving (Eq, Ord, Show)
-- commands
newtype Commands = Commands (Set Command)
    deriving (Eq, Ord, Show)
-- leftmost square on every tape
leftBoundingLetter = Letter 'α'
-- rightmost square on every tape
rightBoundingLetter = Letter 'ω'
emptySymbol = Letter 'ε'
-- TM1
newtype TM1 = TM1 (InputAlphabet, TapeAlphabet, MultiTapeStates, Commands, StartStates, AccessStates)
    deriving (Eq, Ord, Show)