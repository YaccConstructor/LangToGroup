module GRType where

import Data.Set (Set)
import SMType

data A = A_Y Y| A_K Int | A_R SRule | A_Q State
   deriving (Show, Eq, Ord)

data SmbR = SmbA A | SmbA' A
   deriving (Show, Eq, Ord)

data GrRelation = Relation ([SmbR], [SmbR]) | Relator [SmbR]
   deriving (Show, Eq, Ord)

newtype GR =  GR (Set A, Set GrRelation)
   deriving (Eq, Ord)
instance Show GR where
   show (GR (a, r)) = "A {" ++ show a ++ "}" ++ "\n" ++ "R {" ++ show r ++ "}"
