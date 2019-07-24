module SMType where

import Data.String
import Data.Set (Set)
import Prelude hiding (Word)
import TMType (TapeCommand, Square)

data Tag = Hat | Quote | Dash 
   deriving (Eq, Ord, Show)

data SMTag = T4 | T9 | TAlpha | TOmega

instance Show SMTag where 
   show tag =
      case tag of
         T4 -> "T_{4}"
         T9 -> "T_{9}"
         TAlpha -> "T_{\\alpha}"
         TOmega -> "T_{\\omega}"

data StateVal = StateVal {tape :: Int, tmCommand :: Maybe [TapeCommand], smTag :: Maybe SMTag} 
   deriving (Show)

data StateName = E | X | F | P | Q | R | S | T | U 
   deriving (Eq)

instance Show StateName where
   show st = 
      case st of
         E -> "E"
         X -> "x"
         F -> "F"
         P -> "p"
         Q -> "q"
         R -> "r"
         S -> "s"
         T -> "t"
         U -> "u"
    
data State = State {s_name :: StateName, s_idx :: String, s_tags :: Set Tag, s_val :: Maybe StateVal}
   deriving (Show)

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

data SM =  SM {yn :: [[Y]], qn :: [[State]], srs :: [SRule]} deriving (Show)