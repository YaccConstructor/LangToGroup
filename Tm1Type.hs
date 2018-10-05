import Data.Set (Set)
import qualified Data.Set as Set
-- letter
type Letter = Char
-- input, tape alphabets
type InputAlphabet = Set Letter
type TapeAlphabet = Set Letter
-- state of tm1
type State = Char
-- tm set states
type States = Set State
-- k - vector of tapes states
type MultiTapeStates = Set States
-- k - vector of start states
type StartStates = Set State
-- k - vector of end states
type AccessStates = Set State
-- right, left, none shift after end of command
data Shift = R | L | None
-- command of TM1
type Command = (Letter, State, Letter) -> (Letter, State, Letter, Shift) 
-- commands
type Commands = Set Command
-- leftmost square on every tape
type LeftBoundingLetter = Letter
-- rightmost square on every tape
type RightBoundingLetter = Letter
-- configuration of tape
type Configuration = (LeftBoundingLetter, [Letter], State, [Letter], RightBoundingLetter)
-- TM1
type TM1 = (InputAlphabet, TapeAlphabet, MultiTapeStates, Commands, StartStates, AccessStates)