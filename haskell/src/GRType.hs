module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set
import SMType

data A = A_Y Y| A_K Int | A_R SRule | A_Q State
   deriving (Show, Eq, Ord)

data SmbR = SmbA A | SmbA' A
   deriving (Show, Eq)

data GrRelation = Relation ([SmbR], [SmbR]) | Relator [SmbR]
   deriving (Show, Eq)

newtype GR =  GR ([A], [GrRelation])
instance Show GR where
   show (GR (a, r)) = "A {" ++ show a ++ "}" ++ "\n" ++ "R {" ++ show r ++ "}"
