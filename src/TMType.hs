
-- |This module represents a types of Turing machine.
module TMType where

import Data.Set (Set)

-- |This type represents a state of the Turing machine 'TM'.
newtype State = State String
    deriving (Eq, Ord, Show)

-- |Type of a k-vector of tapes states, where k is a count of tapes. 
--
-- Type represents all states of the Turing machine 'TM'.
newtype MultiTapeStates = MultiTapeStates [Set State]
    deriving (Eq, Ord, Show)

-- |Type of a k-vector of a start states of the Turing machine 'TM'.
newtype StartStates = StartStates [State]
    deriving (Eq, Ord, Show)

-- |Type of a k-vector of a final states of the Turing machine 'TM'.
newtype AccessStates = AccessStates [State]
    deriving (Eq, Ord, Show)

-- |Type of a state in a changed Turing machine, which has a form F_q, where q is a 'State'.
newtype StateOmega = StateOmega {state :: State}
    deriving (Eq, Ord)
instance Show StateOmega where
    show s = "F_{" ++ q ++ "}" 
        where (State q) = state s

-- |Type of a command of the Turing machine 'TM', which applicable on a single tape.
--
-- 'SingleTapeCommand' is a command for normal Turing machine.
--
-- 'PreSMCommand' is a command for Turing machine after change.
data TapeCommand = SingleTapeCommand ((Square, State, Square), (Square, State, Square)) | PreSMCommand ((Square, StateOmega), (Square, StateOmega))
    deriving (Eq, Ord, Show)

-- |This is a data type of square of the Turing machine 'TM'. 
--
-- 'Value' is a square of string.
--
-- 'E' is a square, which we use after change of Turing machine, for representing empties of a tape, and its argument marks the tape number. 
--
-- 'RBS' is a right bounding square. 
--
-- 'LBS' is a left bounding square.
--
-- 'ES' is a empty square.
--
-- 'PCommand' is a square of command with parentheses surroundings.
--
-- 'BCommand' is a square of command with brackets surroundings.
data Square = Value {val_name :: String, val_quote_cnt :: Int} | E Int | RBS | LBS | ES | PCommand [TapeCommand] | BCommand [TapeCommand]
    deriving (Eq, Ord, Show)

-- |Type of input alphabet of the Turing machine 'TM'.
newtype InputAlphabet = InputAlphabet (Set Square)
    deriving (Eq, Ord, Show)

-- |Type of tape alphabet of the Turing machine 'TM'.
newtype TapeAlphabet = TapeAlphabet (Set Square)
    deriving (Eq, Ord, Show)

-- |Type of commands of a Turing machine 'TM'.
newtype Commands = Commands (Set [TapeCommand])
    deriving (Eq, Ord, Show)

-- |This type represents a Turing macine. 
newtype TM = TM (InputAlphabet, [TapeAlphabet], MultiTapeStates, Commands, StartStates, AccessStates)
    deriving (Eq, Ord, Show)