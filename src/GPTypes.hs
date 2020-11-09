module GPTypes where

import Set (Set)

newtype Generator = G Int
    deriving (Eq, Ord, Show)

data Element = Positive Generator | Negative Generator
    deriving (Eq, Ord, Show)

type EWord = [Element]

data Relation = EWord `Equals` EWord
    deriving (Eq, Ord, Show)

type Generators = Set Generator

type Relations = Set Relation

newtype GroupPresentation = GP Relations
    deriving (Show)
