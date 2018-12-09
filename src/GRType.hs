module GRType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set
import SMType
import Prelude hiding (Word)

--newtype A = A ([String]) deriving Show
data A = A_Y Y| A_K String | A_S String 
       | A_R SRule | A_R' SRule | A_Q (State String) | A_W Word

instance Show A where
   show (A_Y y) = show y
   show (A_K k) = show k
   show (A_Q q) = show q   
   show (A_S s) = show s
   show (A_R r) = show r
   show (A_R' r) = show r ++ "-1"
   show (A_W w) = show w

newtype Relation = Relation ([A], [A]) deriving Show

newtype GR =  GR ([A], [Relation]) deriving Show
