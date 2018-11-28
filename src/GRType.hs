module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set

newtype A = A ([Char]) deriving Show
newtype Relation = Relation (A, A) deriving Show

newtype Relations = Relations ([Relation]) deriving Show

newtype GR =  GR (A, Relations) deriving Show