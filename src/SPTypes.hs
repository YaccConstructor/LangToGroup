module SPTypes where

import Set (Set)

newtype Generator = G Int
    deriving (Eq, Ord, Show)

type GWord = [Generator]

data Relation = GWord `Equals` GWord
    deriving (Eq, Ord, Show)

type Generators = Set Generator

type Relations = Set Relation

newtype SemigroupPresentation = SP Relations
    deriving (Show)
