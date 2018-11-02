module Tm1Type where

import Data.Set (Set)
import qualified Data.Set as Set
-- letter
newtype Letter = Letter Char
    deriving (Eq, Ord)
-- input, tape alphabets
newtype InputAlphabet = InputAlphabet (Set Letter)
newtype TapeAlphabet = TapeAlphabet (Set Letter)
-- state of TM1
newtype State = State String
    deriving (Eq, Ord)
-- TM1 set states
newtype States = States (Set State)
-- k - vector of tapes states
newtype MultiTapeStates = MultiTapeStates [States]
-- k - vector of start states
newtype StartStates = StartStates [State]
-- k - vector of end states
newtype AccessStates = AccessStates [State]
-- command of TM1 for single tape
data SingleTapeCommand = SingleTapeCommand ((Letter, State, Letter), (Letter, State, Letter)) | NoCommand
    deriving (Eq, Ord)
-- command of TM1
newtype Command = Command [SingleTapeCommand]
    deriving (Eq, Ord)
-- commands
newtype Commands = Commands (Set Command)
-- leftmost square on every tape
leftBoundingLetter = Letter('α')
-- rightmost square on every tape
rightBoundingLetter = Letter('ω')
emptySymbol = Letter 'ε'
-- configuration of tape
newtype Configuration = Configuration ([Letter], State, [Letter])
-- TM1
newtype TM1 = TM1 (InputAlphabet, TapeAlphabet, MultiTapeStates, Commands, StartStates, AccessStates)