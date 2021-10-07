-- |Module `TuringMachine.Optimization.Level` include levels of optimization of
--  Turing machine.
module TuringMachine.Optimization.Level (
    Level (..),
    maxO,
    levelGradation,
  ) where

data Level =
      O0
    | O1
    | O2
    deriving (Eq, Ord, Enum, Bounded)

maxO :: Level
maxO = maxBound

levelGradation :: [a -> a] -> Level -> a -> a
levelGradation [] _ = id
levelGradation (f:fs) l
    | l == O0 = f
    | otherwise = levelGradation fs (pred l) . f
