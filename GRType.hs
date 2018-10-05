module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set

type Transition = (Set Char) -> (Set Char)
type Auxiliary = (Set Char) -> (Set Char)
type Hub = (Set Char) -> (Set Char)

type A = Set Char
data Relation = Transition | Auxiliary | Hub

data Relations = Set Relation

type GR = (A, Relations)
