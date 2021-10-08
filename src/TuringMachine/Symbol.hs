{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TuringMachine.Symbol (
    Symbol,
    symbol,
    numSymbol,
    blankSymbol,
  ) where

newtype Symbol = S { numSymbol :: Int }
    deriving (Eq, Ord, Num, Enum)

instance Bounded Symbol where
    minBound = S 0
    maxBound = S maxBound

symbol :: Int -> Symbol
symbol = S

blankSymbol :: Symbol
blankSymbol = S 0
