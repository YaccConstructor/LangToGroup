{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TuringMachine.State (
    State,
    state,
    numState,
    startState,
    finalState,
  ) where

newtype State = Q { numState :: Int }
    deriving (Eq, Ord, Num, Enum, Show)

instance Bounded State where
    minBound = Q 0
    maxBound = Q maxBound

state :: Int -> State
state = Q

startState :: State
startState = Q 1

finalState :: State
finalState = Q 0
