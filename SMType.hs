module SMType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector
import qualified Data.Vector as Vector

newtype N = N Int deriving Show

newtype Y' = Y' (Set Char) deriving Show
newtype Q' = Q' (Set Char) deriving Show

newtype Y = Y (Vector Y') deriving Show
newtype Q = Q (Vector Q') deriving Show

newtype SM =  SM (N, Y, Q) deriving Show
