{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GroupPresentation.Generator (
    Generator,
    generator,
    numGenerator,
    GWord,
  ) where

newtype Generator = G { numGenerator :: Int }
    deriving (Eq, Ord, Enum)

instance Bounded Generator where
    minBound = G 0
    maxBound = G maxBound

generator :: Int -> Generator
generator = G

type GWord = [Generator]
