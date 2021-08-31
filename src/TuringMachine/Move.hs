module TuringMachine.Move (
    Move,
    toLeft,
    toRight,
    versa,
  ) where

data Move = ToLeft | ToRight
    deriving (Eq, Ord)

instance Show Move where
    show ToLeft  = "<-"
    show ToRight = "->"

toLeft :: Move
toLeft = ToLeft

toRight :: Move
toRight = ToRight

versa :: Move -> Move
versa ToLeft = ToRight
versa ToRight = ToLeft
