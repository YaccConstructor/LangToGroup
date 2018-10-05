module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set

type A = Set Char
--type Relation = (A, A)
type Relation = ((A, A), (A, A))

data Relations = Set Relation

type GR = (A, Relations)
