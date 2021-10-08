{-# LANGUAGE RankNTypes #-}

module SemigroupPresentation.Relation (
    Relation,
    relation,
    relator,
    Relations,
    StrRelation,
    forAll,
    replaceGenerator,
    module SemigroupPresentation.Generator,
  ) where

import SemigroupPresentation.Generator

import Containers
import Lens

type Relation = Pair GWord

relation :: GWord -> GWord -> Relation
relation = curry Pair

relator :: GWord -> Relation
relator gw = relation gw []

type Relations = Set Relation

type StrRelation = Pair StrGWord

forAll :: Ord b => (a -> b) -> Getter (Set (Pair a)) (Set (Pair b))
forAll f = to $ (gmap.fmap) f

replaceGenerator :: (String, String) -> StrGWord -> StrGWord
replaceGenerator (gFrom, gTo) = map (\g -> if g == gFrom then gTo else g)
