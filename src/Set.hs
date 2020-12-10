module Set where

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
