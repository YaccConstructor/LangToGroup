module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set

newtype A = A (Set Char) deriving Show
newtype Relation = Relation (A, A) deriving Show

newtype Relations = Relations (Set Relation) deriving Show

newtype GR =  GR (A, Relations) deriving Show
