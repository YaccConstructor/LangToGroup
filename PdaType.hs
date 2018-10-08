module PdaType where

import Data.Set (Set)
import qualified Data.Set as Set

newtype State = State Char
    deriving (Eq, Ord)

newtype States = States (Set State)

-- letter
newtype Letter = Letter Char
    deriving (Eq, Ord)
-- input, tape alphabets
newtype InputAlphabet = InputAlphabet (Set Letter)
newtype StackAlphabet = StackAlphabet (Set Letter)

type StartState = State

newtype InitialStackSymbols = InitialStackSymbols [Letter]

newtype AcceptingStates = AcceptingStates States

newtype TransitionRelation = TransitionRelation ((State, Letter, Letter), (State, [Letter]))
    deriving (Eq, Ord)

newtype TransitionRelations = TransitionRelations (Set TransitionRelation)

emptySymbol = Letter 'ε'

newtype Pda = Pda (States, InputAlphabet, StackAlphabet, TransitionRelations, StartState, InitialStackSymbols, AcceptingStates)