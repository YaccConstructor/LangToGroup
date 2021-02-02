module GPTypes where

import Set (Set)

newtype Generator = G Int
    deriving (Eq, Ord, Show)

data Element = Positive Generator | Negative Generator
    deriving (Eq, Show)

instance Ord Element where
    (Negative x) <= (Negative y) = x <= y
    (Negative x) <= (Positive y) =
        if x == y
        then False
        else x <= y
    (Positive x) <= (Negative y) =
        if x == y
        then True
        else x <= y
    (Positive x) <= (Positive y) = x <= y

type EWord = [Element]

data Relation = EWord `Equals` EWord
    deriving (Eq, Ord, Show)

type Generators = Set Generator

type Relations = Set Relation

newtype GroupPresentation = GP Relations
    deriving (Show)
