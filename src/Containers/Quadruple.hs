-- |Module `Containers.Quadruple` include type `Quadruple` and useful functions
--  for work with it.
--
--  `Quadruple a` is just quadruple as `(a, a, a, a)`.
module Containers.Quadruple (
    Quadruple (Quadruple),
    unQuadruple,
    toList,
    toZipList,
    fromList,
  ) where

import Control.Applicative (ZipList(..))

newtype Quadruple a = Quadruple { unQuadruple :: (a, a, a, a) }
    deriving (Eq, Ord)

instance Functor Quadruple where
    fmap f (Quadruple (x, y, z, w)) = Quadruple (f x, f y, f z, f w)

instance Applicative Quadruple where
    pure x = Quadruple (x, x, x, x)
    (Quadruple (f, g, h, q)) <*> (Quadruple (x, y, z, w)) =
        Quadruple (f x, g y, h z, q w)

instance Foldable Quadruple where
    foldr f z = foldr f z . toList

instance Traversable Quadruple where
    sequenceA (Quadruple (f, g, h, q)) =
        Quadruple <$> ((,,,) <$> f <*> g <*> h <*> q)

toList :: Quadruple a -> [a]
toList (Quadruple (x, y, z, w)) = [x, y, z, w]

toZipList :: Quadruple a -> ZipList a
toZipList (Quadruple (x, y, z, w)) = ZipList [x, y, z, w]

fromList :: MonadFail m => [a] -> m (Quadruple a)
fromList [x, y, z, w] = return $ Quadruple (x, y, z, w)
fromList _ = fail "Input list must contain just 4 elements"
