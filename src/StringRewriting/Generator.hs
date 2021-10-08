module StringRewriting.Generator (
    Generator,
    generator,
    numGenerator,
    GWord,
  ) where

newtype Generator = G { numGenerator :: Int }
    deriving (Eq, Ord)

generator :: Int -> Generator
generator = G

type GWord = [Generator]
