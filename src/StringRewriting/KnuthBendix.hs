module StringRewriting.KnuthBendix (
    Order,
    knuthBendix,
    knuthBendixBy,
    module StringRewriting,
  ) where

import StringRewriting

import qualified Math.Algebra.Group.StringRewriting as SR

type Order a = a -> a -> Ordering

type GenOrder = Order Generator

data GenWithOrder = GWO
  { genOrder :: GenOrder
  , gen :: Generator
  }

instance Eq GenWithOrder where
    (GWO f x) == (GWO _ y) = f x y == EQ

instance Ord GenWithOrder where
    compare (GWO f x) (GWO _ y) = f x y

knuthBendix :: Set (Pair GWord) -> GeneratorsDescr -> StringRewriting
knuthBendix = knuthBendixBy (compare `on` numGenerator)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

knuthBendixBy ::
    GenOrder ->
    Set (Pair GWord) ->
    GeneratorsDescr ->
    StringRewriting
knuthBendixBy ord =
    stringRewriting .
    fromList .
    map (uncurry rule) .
    (map.mapPair.map) gen .
    SR.knuthBendix .
    map unPair .
    (map.fmap.map) (GWO ord) .
    toList
