{-# LANGUAGE FlexibleInstances #-}

module OTMTSet where

import OTMReader (OTMTriple)
import qualified TMType as OTM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

type OTMTSet = Map OTM.Square (Map OTM.State (Set OTM.Square))

fromList :: [OTMTriple] -> OTMTSet
fromList [] = Map.empty
fromList ((sq1, st, sq2) : ts) =
    let res = fromList ts
    in
        case Map.lookup sq1 res of
            Nothing -> Map.insert sq1 (Map.singleton st (Set.singleton sq2)) res
            Just res' -> flip (Map.insert sq1) res $
                case Map.lookup st res' of
                    Nothing -> Map.insert st (Set.singleton sq2) res'
                    Just res'' -> Map.insert st (Set.insert sq2 res'') res'

class Ord a => Member a where
    member :: a -> OTMTSet -> Bool
    notMember :: a -> OTMTSet -> Bool
    notMember = (not .) . member

instance Member OTM.Square where
    member = Map.member

instance Member (OTM.Square, OTM.State) where
    member (sq, st) sqmap = Just True == do
        stmap <- Map.lookup sq sqmap
        return $ Map.member st stmap

instance Member (OTM.Square, OTM.State, OTM.Square) where
    member (sq1, st, sq2) sq1map = Just True == do
        stmap  <- Map.lookup sq1 sq1map
        sq2set <- Map.lookup st  stmap
        return $ Set.member sq2 sq2set
