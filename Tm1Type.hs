module Tm1Type where

import Data.Set (Set)
import qualified Data.Set as Set
-- letter
newtype Letter = Letter Char
-- input, tape alphabets
newtype InputAlphabet = InputAlphabet (Set Letter)
newtype TapeAlphabet = TapeAlphabet (Set Letter)
-- state of TM1
newtype State = State Char
-- TM1 set states
newtype States = States (Set State)
-- k - vector of tapes states
newtype MultiTapeStates = MultiTapeStates [States]
-- k - vector of start states
newtype StartStates = StartStates [State]
-- k - vector of end states
newtype AccessStates = AccessStates [State]
-- right, left, none shift after end of command
data Shift = R | L | None
-- command of TM1
newtype Command = Command ((Letter, State, Letter) -> (Letter, State, Letter, Shift))
-- commands
newtype Commands = Commands (Set Command)
-- leftmost square on every tape
leftBoundingLetter = 'α'
-- rightmost square on every tape
rightBoundingLetter = 'ω'
-- configuration of tape
newtype Configuration = Configuration ([Letter], State, [Letter])
-- TM1
newtype TM1 = TM1 (InputAlphabet, TapeAlphabet, MultiTapeStates, Commands, StartStates, AccessStates)