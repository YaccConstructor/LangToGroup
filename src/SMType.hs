{-# LANGUAGE FlexibleInstances #-}

-- |This module represents types of S-machine.
module SMType where

import Data.String
import Data.Set (Set)
import Prelude hiding (Word)
import TMType (TapeCommand, Square)

-- |This is state name data type of 'State'.
--
-- Every constructor here represents name of the 'State'.
data StateName = E | X | F | P | Q | R | S | T | U 
   deriving (Eq, Ord)

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

-- |This is tag data type of 'State'.
--
-- 'Hat' it's like a house roof.
--
-- 'Quote' is a '.
--
-- 'Dash' is a overline.
data Tag = Hat | Quote | Dash 
   deriving (Eq, Ord, Show)

-- |'SMTag' is a tag for S-machines and shows what kind is it.
--
-- 'T4' for S-machine number 4.
--
-- 'T9' for number 9.
--
-- 'TAlpha' for alpha S-machine.
--
-- 'TOmega' for omega S-machine.
data SMTag = T4 | T9 | TAlpha | TOmega
   deriving (Eq, Ord)

instance Show SMTag where 
   show tag =
      case tag of
         T4 -> "T_{4}"
         T9 -> "T_{9}"
         TAlpha -> "T_{\\alpha}"
         TOmega -> "T_{\\omega}"

-- |This is data type of Turing machine commands and has uses in 'StateVal' in order to define for which command belongs to 'StatevVal'.
data TMCMD = Command [TapeCommand] | CommandAlias String 
   deriving (Show, Eq, Ord)

-- |'StateVal' represents belongnes of 'State' to tape, Turing machine commands and S-machine 'SM'.
data StateVal = StateVal {tape :: Int, tmCommand :: Maybe TMCMD, smTag :: Maybe SMTag} 
   deriving (Show, Eq, Ord)
   
-- |This data type represents state of S-machine 'SM'. 
--
-- 's_name' represents a name of 'State'.
--
-- 's_idx' represents a low index of 'State'.
--
-- 's_tags' represents a set of 'Tag' of 'State'.
--
-- 's_val' represents a 'StateVal' of 'State'.
data State = State {s_name :: StateName, s_idx :: String, s_tags :: Set Tag, s_val :: Maybe StateVal}
   deriving (Show, Ord, Eq)

-- |This data type represents a tape letter of S-machine 'SM'.
data Y = Y Square | Alpha | Delta | Omega
   deriving (Show, Eq, Ord)

-- |This is a data type of symbols that can be on 'SM' tape.
--
-- Symbols can be 'Y' ('SmbY'), its invertions ('SmbY'') and 'State' ('SmbQ').
data Smb = SmbY Y | SmbY' Y | SmbQ State
   deriving (Eq, Ord)

instance Show Smb where
   show (SmbY y) = show y
   show (SmbY' y) = show y ++ "^{-1}"
   show (SmbQ q) = show q      

-- |This is a admissible word of the S-machine 'SM', which consist of the 'Smb'.
newtype Word = Word [Smb] 
   deriving (Show, Eq, Ord)

-- |This is a rule of the S-machine 'SM'. 
--
-- Left part of the pair represents what substitute.
--
-- Right part -- for what substitute.
newtype SRule = SRule [(Word, Word)]
   deriving (Eq, Ord)

instance Show SRule where
   show (SRule s) = "[" ++ (foldr (\(w1,w2) acc -> show w1 ++ "->" ++ show w2 ++ ";" ++ acc) "" s) ++ "]\n" 

-- |This is data type of S-machine.
--
-- It consists of a lists of tape letters 'Y', states 'State' and rules 'SRule'.
data SM =  SM {yn :: [[Y]], qn :: [Set State], srs :: [SRule]} deriving (Show, Eq, Ord)