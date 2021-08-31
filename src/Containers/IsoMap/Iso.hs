{-# LANGUAGE FunctionalDependencies #-}

module Containers.IsoMap.Iso (
    Iso (..),
  ) where

import Containers.Set (Set)
import Containers.Map (Map)

infixr 1 ~$, ~&

class Iso i a b | i a -> b, i b -> a where
    (~$) :: MonadFail m => i -> a -> m b
    (~&) :: MonadFail m => a -> i -> m b
    values :: i -> Set a
    member :: a -> i -> Bool
    filter :: (a -> Bool) -> i -> i
    insert :: MonadFail m => (a, b) -> i -> m i
    delete :: a -> i -> i
    toMap :: i -> Map a b
    fromList :: MonadFail m => [(a, b)] -> m i
    fromList' :: [(a, b)] -> i
    toList :: i -> [(a, b)]
    singleton :: a -> b -> i
    (~$) = flip (~&)
    (~&) = flip (~$)
