import Data.Set (Set)
import qualified Data.Set as Set
-- letter
type Letter = Char
-- input, tape alphabets
type InputAlphabet = Set Letter
type TapeAlphabet = Set Letter
-- state of TM1
type State = Char
-- TM1 set states
type States = Set State
-- k - vector of tapes states
type MultiTapeStates = [States]
-- k - vector of start states
type StartStates = [State]
-- k - vector of end states
type AccessStates = [State]
-- right, left, none shift after end of command
data Shift = R | L | None
-- command of TM1
type Command = (Letter, State, Letter) -> (Letter, State, Letter, Shift) 
-- commands
type Commands = Set Command
-- leftmost square on every tape
leftBoundingLetter = 'α'
-- rightmost square on every tape
rightBoundingLetter = 'ω'
-- configuration of tape
type Configuration = ([Letter], State, [Letter])
-- TM1
type TM1 = (InputAlphabet, TapeAlphabet, MultiTapeStates, Commands, StartStates, AccessStates)