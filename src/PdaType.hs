module PdaType where

import Data.Set (Set)
import qualified Data.Set as Set

newtype State = State Char
    deriving (Eq, Ord, Show)

newtype States = States (Set State)
    deriving (Eq, Ord, Show)

-- letter
newtype Letter = Letter Char
    deriving (Eq, Ord, Show)
-- input, stack alphabets
newtype InputAlphabet = InputAlphabet (Set Letter)
    deriving (Eq, Ord, Show)
newtype StackAlphabet = StackAlphabet (Set Letter)
    deriving (Eq, Ord, Show)
type StartState = State

newtype InitialStackSymbols = InitialStackSymbols [Letter]
    deriving (Eq, Ord, Show)

newtype AcceptingStates = AcceptingStates States
    deriving (Eq, Ord, Show)

newtype TransitionRelation = TransitionRelation ((State, Letter, Letter), (State, [Letter]))
    deriving (Eq, Ord, Show)

newtype TransitionRelations = TransitionRelations (Set TransitionRelation)
    deriving (Eq, Ord, Show)
emptySymbol = Letter 'ε'

newtype Pda = Pda (States, InputAlphabet, StackAlphabet, TransitionRelations, StartState, InitialStackSymbols, AcceptingStates)
    deriving (Eq, Ord)

instance Show Pda where
    show (Pda (states, inputAlphabet, stackAlphabet, transitionRelations, startState, initialStackSymbols, acceptingStates)) = 
            "\tStates: " ++ show states ++
                    "\n\tInputAlphabet: " ++ show inputAlphabet ++
                            "\n\tStackAlphabet: " ++ show stackAlphabet ++
                                    "\n\tTransitionRelations: " ++ show transitionRelations ++
                                            "\n\tStartState: " ++ show startState ++
                                                    "\n\tInitialStackSymbols: " ++ show initialStackSymbols ++
                                                            "\n\tAcceptingStates: " ++ show acceptingStates