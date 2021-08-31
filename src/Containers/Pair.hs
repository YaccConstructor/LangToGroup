{-# LANGUAGE StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, DeriveAnyClass #-}

module Containers.Pair (
    Pair (Pair),
    unPair,
    first,
    second,
    swap,
    toList,
    toZipList,
    fromList,
  ) where

import Control.Lens (Each)
import Control.Applicative (ZipList(..))

newtype Pair a = Pair { unPair :: (a, a) }
    deriving (Eq, Ord)

instance Functor Pair where
    fmap f (Pair (x, y)) = Pair (f x, f y)

instance Applicative Pair where
    pure x = Pair (x, x)
    (Pair (f, g)) <*> (Pair (x, y)) = Pair (f x, g y)

instance Foldable Pair where
    foldr f z = foldr f z . toList

instance Traversable Pair where
    sequenceA (Pair (f, g)) = Pair <$> ((,) <$> f <*> g)

deriving instance Each (Pair a) (Pair b) a b

first :: Pair a -> a
first (Pair (x, _)) = x

second :: Pair a -> a
second (Pair (_, y)) = y

swap :: Pair a -> Pair a
swap (Pair (x, y)) = Pair (y, x)

toList :: Pair a -> [a]
toList (Pair (x, y)) = [x, y]

toZipList :: Pair a -> ZipList a
toZipList (Pair (x, y)) = ZipList [x, y]

fromList :: MonadFail m => [a] -> m (Pair a)
fromList [x, y] = return $ Pair (x, y)
fromList _ = fail "Input list must contain just 2 elements"
