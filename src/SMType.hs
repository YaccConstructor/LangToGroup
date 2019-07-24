module SMType where

import Data.String
import Data.Set (Set)
import Prelude hiding (Word)
import TMType (TapeCommand, Square)

data Tag = Hat | Quote | Dash 
   deriving (Eq,Ord)

data SMTag = T4 | T9 | TAlpha | TOmega

data StateVal = StateVal {tape :: Int, tmCommand :: Maybe [TapeCommand], smTag :: Maybe SMTag} 

type StateIndex = String

data StateName = E | X | F | P | Q | R | S | T | U 
   deriving (Eq, Show)
    
data State = State {s_name :: StateName, s_idx :: StateIndex, s_tags :: Set Tag, s_val :: Maybe StateVal}
instance Show State where
   show s = "dddd"

newtype Y = Y Square
   deriving (Show)

data Smb = SmbY Y | SmbY' Y | SmbQ State

instance Show Smb where
   show (SmbY y) = show y
   show (SmbY' y) = show y ++ "^{-1}"
   show (SmbQ q) = show q      

newtype Word = Word [Smb] 
   deriving (Show)

newtype SRule = SRule [(Word, Word)]

instance Show SRule where
   show (SRule s) = "[" ++ (foldr (\(w1,w2) acc -> show w1 ++ "->" ++ show w2 ++ ";") "" s) ++ "]\n" 

data SM =  SM {yn :: [[Y]], qn :: [[State]], srs :: [SRule]} deriving Show
