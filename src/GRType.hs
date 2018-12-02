module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set

newtype A = A ([String]) deriving Show
newtype Relation = Relation ([A], [A]) deriving Show

newtype GR =  GR (A, [Relation]) deriving Show