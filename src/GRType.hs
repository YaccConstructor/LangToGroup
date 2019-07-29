module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set
import SMType

data A = A_Y Y| A_K Int | A_S String | A_R SRule | A_Q State
   deriving (Show, Eq)

data SmbR = SmbA A | SmbA' A
   deriving (Show, Eq)

newtype Relation = Relation ([SmbR], [SmbR]) 
   deriving (Show, Eq)

newtype GR =  GR ([A], [Relation])
instance Show GR where
   show (GR (a, r)) = "A {" ++ show a ++ "}" ++ "\n" ++ "R {" ++ show r ++ "}"
