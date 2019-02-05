module SMType where

import Data.String
import Data.Set (Set)
import Prelude hiding (Word)

data TMCMD = CMD

data Tag = Hat | Quote | Dash deriving (Eq,Ord)

data SMTag = T4 | T9 | TAlpha | TOmega

data StateVal = StateVal {tape :: Int, tmCommand :: Maybe TMCMD, smTag :: Maybe SMTag} 

data StateName = X | E | F | P | Q | R | S | T | U deriving (Eq, Show)
    
data State = State {s_name :: StateName, s_id :: Int, s_tags :: Set Tag, s_val :: StateVal}
instance Show State where
   show s = "dddd"

newtype Y = Y String

instance Show Y where
   show (Y s) = s

data Smb = SmbY Y | SmbY' Y | SmbQ State

instance Show Smb where
   show (SmbY y) = show y
   show (SmbY' y) = show y ++ "-1"
   show (SmbQ q) = show q      

newtype Word = Word [Smb] 

instance Show Word where
   show (Word l) = show l

newtype SRule = SRule [(Word, Word)] 

instance Show SRule where
   show (SRule s) = "[" ++ (foldr (\(w1,w2) acc -> show w1 ++ "->" ++ show w2 ++ ";") "" s) ++ "]\n" 

data SM =  SM {yn :: [[Y]], qn :: [[State]], srs :: [SRule]} deriving Show
