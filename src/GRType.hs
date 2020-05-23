-- | This module represents group presentation and its sub-types.
module GRType where

import Data.Set (Set)
import SMType

-- |This is a data type of 'GR' generators.
--
-- 'A_Y' is a generators witch obtain from alphabet letters 'SMType.Y' of S-machine.
--
-- 'A_K' is a generators witch forms the 'SM2GR.hubRelation'.
--
-- 'A_R' is a generators witch obtain from rules 'SMType.SRule' of S-machine.
--
-- 'A_Q' is a generators witch obtain from states 'SMType.State' of S-machine.
data A = A_Y Y| A_K Int | A_R SRule | A_Q State
   deriving (Show, Eq, Ord)

-- |This is a data type of symbols in 'GrRelation'.
--
-- Symbols can represent generators 'A' and its invertions.
--
-- 'SmbA' represents generators.
--
-- 'SmbA'' represents invertions of generators.
data SmbR = SmbA A | SmbA' A
   deriving (Show, Eq, Ord)
   
-- |This is a data type of 'GR' relations.
-- 
-- 'Relation' represents relation beetwen right and left symbols 'SmbR'. 
--
-- For example, relation ab=ba will coded like
--
-- >>> Relation ([a, b], [b, a])
-- 
-- where a, b are 'SmbR'
--
-- 'Relator' is relation in witch one part is identity. 
--
-- For example, ab=1 is
--
-- >>> Relator [a,b]
data GrRelation = Relation ([SmbR], [SmbR]) | Relator [SmbR]
   deriving (Show, Eq, Ord)

-- |'GR' is a group presentation type. 
-- 
-- It consists of set of generators 'A' and set of relations 'GrRelation'.
newtype GR =  GR (Set A, Set GrRelation)
   deriving (Eq, Ord)
instance Show GR where
   show (GR (a, r)) = "A {" ++ show a ++ "}" ++ "\n" ++ "R {" ++ show r ++ "}"
