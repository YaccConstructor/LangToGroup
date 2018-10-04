import Data.Set (Set)
import qualified Data.Set as Set
-- Не знаю как лучше обозвать, пока буду придерживаться обозначений из определения
-- input, tape, states alphabets
type InputAlphabet = Set Char
type TapeAlphabet = Set Char
type States = Set Char
type StartStates = Set Char
type AccessStates = Set Char
-- right, left, none shift after end of command
data Shift = R | L | None
-- command of TM
type Command = (States, TapeAlphabet) -> (States, TapeAlphabet, Shift) 
-- commands
type Commands = Set Command
-- TM
type TM = (InputAlphabet, TapeAlphabet, States, Commands, StartStates, AccessStates)