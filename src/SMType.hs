module SMType where

import Data.String
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Either
import Prelude hiding (Word)

newtype N = N Int deriving Show

newtype Y = Y String 

instance Show Y where
   show (Y s) = s

newtype Q = Q { getQ :: String }

instance Show Q where
   show q = getQ q

data Smb = SmbY Y | SmbY' Y | SmbQ Q

instance Show Smb where
   show (SmbY y) = show y
   show (SmbY' y) = show y ++ "-1"
   show (SmbQ q) = show q      

--newtype Yn = Yn ([[Y]]) deriving Show
--newtype Qn = Qn ([[Q]]) deriving Show

newtype Word = Word ([Smb]) 

instance Show Word where
   show (Word l) = show l

newtype SRule = SRule [(Word, Word)] 

instance Show SRule where
   show (SRule s) = "[" ++ (foldr (\(w1,w2) acc -> show w1 ++ "->" ++ show w2 ++ ";") "" s) ++ "]\n" 
--newtype SRules = SRules ([SRule]) deriving Show

data SM =  SM {n :: N, yn :: [[Y]], qn :: [[Q]], srs :: [SRule]} deriving Show

--applySRule :: SRule -> Word -> Word
--applySRule (SRule (a:word1, b:word2)) (w:word) = 
