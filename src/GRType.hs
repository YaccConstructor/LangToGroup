module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set
import SMType
import Prelude hiding (Word)

--newtype A = A ([String]) deriving (Eq, Ord, Show)

data ASymb = A_Y Y| A_K String | A_S String 
       | A_R SRule | A_R' SRule | A_Q (State String) | A_W Word | A_W' Word deriving (Eq, Ord)

instance Show ASymb where
   show (A_Y y) = show y
   show (A_K k) = show k
   show (A_Q q) = show q   
   show (A_S s) = show s
   show (A_R r) = show r
   show (A_R' r) = show r ++ "-1"
   show (A_W w) = show w
   show (A_W' w) = show w ++ "-1"

data A = W [ASymb] | W' [ASymb] deriving (Eq, Ord)

instance Show A where
   show (W w) = show w
   show (W' w) = show w ++ "-1"

newtype Relation = Relation ([A], [A]) deriving (Show, Eq, Ord)

newtype GR =  GR ([A], [Relation]) deriving (Eq)
instance Show GR where
   show (GR (a, r)) = "A {" ++ show a ++ "}" ++ "\n" ++ "R {" ++ show r ++ "}"
