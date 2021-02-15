module Set where

import qualified Data.Set as RealSet

newtype Set a = Set { unSet :: [a] }
    deriving (Show)

empty :: Set a
empty = Set []

size :: Set a -> Int
size = length . unSet

fromList :: [a] -> Set a
fromList = Set

toList :: Set a -> [a]
toList = unSet

map :: (a -> b) -> Set a -> Set b
map f = Set . fmap f . unSet

findMax :: Ord a => Set a -> a
findMax = maximum . unSet

unions :: (Foldable f, Ord a) => f (Set a) -> Set a
unions = Set . RealSet.toList . RealSet.fromList . concatMap unSet
