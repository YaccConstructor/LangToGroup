module KnuthBendix where

import Math.Algebra.Group.StringRewriting (knuthBendix)

type Order a = a -> a -> Ordering

newtype WithOrder a = WO (a, Order a)

instance Eq a => Eq (WithOrder a) where
    (WO (x, _)) == (WO (y, _)) = x == y

instance Eq a => Ord (WithOrder a) where
    compare (WO (x, f)) (WO (y, _)) = f x y

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

withOrder :: Eq a => Order a -> a -> WithOrder a
withOrder ord x = WO (x, ord)

withoutOrder :: WithOrder a -> a
withoutOrder (WO (x, _)) = x

knuthBendixBy :: Eq a => Order a -> [([a], [a])] -> [([a], [a])]
knuthBendixBy ord =
    (map $ mapPair $ map $ withoutOrder) .
    knuthBendix .
    (map $ mapPair $ map $ withOrder ord)
