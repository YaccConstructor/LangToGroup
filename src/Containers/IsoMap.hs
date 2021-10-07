{-# LANGUAGE FlexibleInstances, FunctionalDependencies, ScopedTypeVariables, IncoherentInstances, LambdaCase #-}

-- |Module `Containers.IsoMap` include type `IsoMap` and useful functions for
--  working with it.
--
--  `IsoMap` is kind of `Map` with isomorphism between keys and values. So,
--  difference between keys ans values is erased, and you can use as keys any
--  of two available sets, and other set will be used as values.
module Containers.IsoMap (
    IsoMap,
    size,
    empty,
    mapFst,
    mapSnd,
    mapFstRev,
    mapSndRev,
    module Containers.IsoMap.Iso,
  ) where

import Containers.IsoMap.Iso
import qualified Containers.Set as Set
import Containers.Map (Map)
import qualified Containers.Map as Map

import Data.Tuple (swap)
import Data.Function (on, (&))
import Data.Maybe (isNothing)
import Data.List.Extra (nubOrdBy)

newtype IsoMap a b = IsoMap { mapsOf :: (Map a b, Map b a) }
    deriving (Eq)

instance (Show a, Show b) => Show (IsoMap a b) where
    show = show . fst . mapsOf

uniqueValues :: Ord a => [a] -> Bool
uniqueValues xs = Set.size (Set.fromList xs) == length xs

instance (Ord a, Ord b) => Iso (IsoMap a b) a b where
    (~&) a =
        maybe (fail "Can't find element in IsoMap") return .
        Map.lookup a . fst . mapsOf
    values = Map.keysSet . fst . mapsOf
    member a (IsoMap (m, _)) = Map.member a m
    filter p (IsoMap (m1, m2)) =
        IsoMap (Map.filterWithKey (const . p) m1, Map.filter p m2)
    insert (a, b) (IsoMap (m1, m2)) =
        if isNothing (Map.lookup a m1) && isNothing (Map.lookup b m2)
        then return $ IsoMap (Map.insert a b m1, Map.insert b a m2)
        else fail "Can't insert element into IsoMap"
    delete a (IsoMap (m, _)) =
        let m' = Map.delete a m
        in  IsoMap (m', Map.fromList $ map swap $ Map.toList m')
    toMap = fst . mapsOf
    fromList ps =
        let (as, bs) = unzip ps
        in
            if uniqueValues as && uniqueValues bs
            then return $ IsoMap (Map.fromList ps, Map.fromList $ map swap ps)
            else fail "Can't create IsoMap from input list"
    fromList' ps' =
        let ps = ps' & nubOrdBy (compare `on` snd) . nubOrdBy (compare `on` fst)
        in  IsoMap (Map.fromList ps, Map.fromList $ map swap ps)
    toList = Map.toList . toMap
    singleton a b = IsoMap (Map.singleton a b, Map.singleton b a)

instance (Ord a, Ord b) => Iso (IsoMap a b) b a where
    (~&) b =
        maybe (fail "Can't find element in IsoMap") return .
        Map.lookup b . snd . mapsOf
    values = Map.keysSet . snd . mapsOf
    member b (IsoMap (_, m)) = Map.member b m
    filter p (IsoMap (m1, m2)) =
        IsoMap (Map.filter p m1, Map.filterWithKey (const . p) m2)
    insert (b, a) (IsoMap (m1, m2)) =
        if isNothing (Map.lookup a m1) && isNothing (Map.lookup b m2)
        then return $ IsoMap (Map.insert a b m1, Map.insert b a m2)
        else fail "Can't insert element into IsoMap"
    delete a (IsoMap (_, m)) =
        let m' = Map.delete a m
        in  IsoMap (Map.fromList $ map swap $ Map.toList m', m')
    toMap = snd . mapsOf
    fromList ps =
        let (bs, as) = unzip ps
        in
            if uniqueValues as && uniqueValues bs
            then return $ IsoMap (Map.fromList $ map swap ps, Map.fromList ps)
            else fail "Can't create IsoMap from input list"
    fromList' ps' =
        let ps = ps' & nubOrdBy (compare `on` fst) . nubOrdBy (compare `on` snd)
        in  IsoMap (Map.fromList $ map swap ps, Map.fromList ps)
    toList = Map.toList . toMap
    singleton a b = IsoMap (Map.singleton b a, Map.singleton a b)

size :: IsoMap a b -> Int
size (IsoMap (m, _)) = Map.size m

empty :: IsoMap a b
empty = IsoMap (Map.empty, Map.empty)

mapFst :: Ord b => (a -> b) -> IsoMap a c -> IsoMap b c
mapFst f (IsoMap (m1, m2)) = IsoMap (Map.mapKeys f m1, Map.map f m2)

mapSnd :: Ord b => (a -> b) -> IsoMap c a -> IsoMap c b
mapSnd f (IsoMap (m1, m2)) = IsoMap (Map.map f m1, Map.mapKeys f m2)

mapFstRev :: Ord b => (a -> b) -> IsoMap a c -> IsoMap c b
mapFstRev f (IsoMap (m1, m2)) = IsoMap (Map.map f m2, Map.mapKeys f m1)

mapSndRev :: Ord b => (a -> b) -> IsoMap c a -> IsoMap b c
mapSndRev f (IsoMap (m1, m2)) = IsoMap (Map.mapKeys f m2, Map.map f m1)
