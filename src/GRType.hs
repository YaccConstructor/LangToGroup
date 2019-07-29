module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set
import SMType

data A = A_Y Y| A_K Int | A_S String | A_R SRule | A_Q State
   deriving (Show)

data SmbR = SmbA A | SmbA' A
   deriving (Show)

newtype Relation = Relation ([SmbR], [SmbR]) 
   deriving (Show)

newtype GR =  GR (Set A, Set Relation)
instance Show GR where
   show (GR (a, r)) = "A {" ++ show a ++ "}" ++ "\n" ++ "R {" ++ show r ++ "}"
