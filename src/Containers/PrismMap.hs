-- |Module `Containers.PrismMap` include type `PrismMap` and useful functions
--  for working with it.
--
--  `PrismMap` is kind of `Map` with faster access to keys by value. So, it is
--  almost `IsoMap`, but without isomorphism restriction.
module Containers.PrismMap (
    PrismMap,
    size,
    Containers.PrismMap.null,
    empty,
    singleton,
    fromList,
    toList,
    toMap,
    Containers.PrismMap.lookup,
    lookupKeys,
    values,
    keys,
    keysSet,
    value,
    key,
    Containers.PrismMap.map,
    mapKeys,
    Containers.PrismMap.filter,
    filterKeys,
    insert,
    deleteKey,
    deleteValue,
    union,
    intersection,
    difference,
  ) where

import Containers.Set (Set)
import qualified Containers.Set as Set
import Containers.Map (Map)
import qualified Containers.Map as Map
import Lens

import Control.Monad (guard)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)

newtype PrismMap k a = PrismMap (Map k a, Map a (Set k))
    deriving (Eq)

instance (Show k, Show a) => Show (PrismMap k a) where
    show (PrismMap (m, _)) = show m

(??) :: MonadFail m => Maybe a -> String -> m a
ma ?? msg = maybe (fail msg) return $ ma

size :: PrismMap k a -> Int
size (PrismMap (m, _)) = Map.size m

null :: PrismMap k a -> Bool
null (PrismMap (m, _)) = Map.null m

empty :: PrismMap k a
empty = PrismMap (Map.empty, Map.empty)

singleton :: k -> a -> PrismMap k a
singleton k a = PrismMap (Map.singleton k a, Map.singleton a $ Set.singleton k)

sndMapFromList :: (Ord k, Ord a) => [(k, a)] -> Map a (Set k)
sndMapFromList xs =
    Map.fromListWith Set.union $ (swap . (_1 %~ Set.singleton)) <$> xs

fromList :: (Ord k, Ord a) => [(k, a)] -> PrismMap k a
fromList xs = PrismMap (Map.fromList xs, sndMapFromList xs)

toList :: PrismMap k a -> [(k, a)]
toList (PrismMap (m, _)) = Map.toList m

toMap :: PrismMap k a -> Map k a
toMap (PrismMap (m, _)) = m

lookup :: (Ord k, MonadFail m) => k -> PrismMap k a -> m a
lookup k (PrismMap (m, _)) = Map.lookup k m ?? "Key was not found"

lookupKeys :: Ord a => a -> PrismMap k a -> Set k
lookupKeys a (PrismMap (_, w)) = fromMaybe Set.empty $ Map.lookup a w

values :: PrismMap k a -> [a]
values (PrismMap (m, _)) = Map.elems m

keys :: PrismMap k a -> [k]
keys (PrismMap (m, _)) = Map.keys m

keysSet :: PrismMap k a -> Set k
keysSet (PrismMap (m, _)) = Map.keysSet m

value :: Ord a => a -> PrismMap k a -> Bool
value a (PrismMap (_, w)) = Map.member a w

key :: Ord k => k -> PrismMap k a -> Bool
key k (PrismMap (m, _)) = Map.member k m

map :: Ord b => (a -> b) -> PrismMap k a -> PrismMap k b
map f (PrismMap (m, w)) = PrismMap (Map.map f m, Map.mapKeys f w)

mapKeys :: Ord l => (k -> l) -> PrismMap k a -> PrismMap l a
mapKeys f (PrismMap (m, w)) = PrismMap (Map.mapKeys f m, (Map.map.Set.map) f w)

filter :: (a -> Bool) -> PrismMap k a -> PrismMap k a
filter p (PrismMap (m, w)) =
    PrismMap (Map.filter p m, Map.filterWithKey (const.p) w)

filterKeys :: (k -> Bool) -> PrismMap k a -> PrismMap k a
filterKeys p (PrismMap (m, w)) =
    let w' = Map.map (Set.filter p) w
    in  PrismMap (Map.filterWithKey (const.p) m, Map.filter (not.Set.null) w')

insert :: (Ord k, Ord a) => k -> a -> PrismMap k a -> PrismMap k a
insert k a (PrismMap (m, w)) =
    PrismMap (Map.insert k a m, Map.update (Just . Set.insert k) a w)

deleteKey :: (Ord k, Ord a, MonadFail m) =>
    k -> PrismMap k a -> m (PrismMap k a)
deleteKey k (PrismMap (m, w)) = (?? "Key isn't in PrismMap") $ do
    a <- m Map.!? k
    let delKey s = do
            let s' = Set.delete k s
            guard $ not $ Set.null s'
            return $ s'
    return $
        PrismMap (Map.delete k m, Map.update delKey a w)

deleteValue :: (Ord k, Ord a, MonadFail m) =>
    a -> PrismMap k a -> m (PrismMap k a)
deleteValue a (PrismMap (m, w)) = (?? "Value isn't in PrismMap") $ do
    ks <- w Map.!? a
    return $
        PrismMap (Map.withoutKeys m ks, Map.delete a w)

union :: (Ord k, Ord a) => PrismMap k a -> PrismMap k a -> PrismMap k a
union (PrismMap (m1, _)) (PrismMap (m2, _)) =
    let m = Map.union m1 m2
    in  PrismMap (m, sndMapFromList $ Map.toList m)

intersection :: (Ord k, Ord a) => PrismMap k a -> PrismMap k a -> PrismMap k a
intersection (PrismMap (m1, _)) (PrismMap (m2, _)) =
    let m = Map.intersection m1 m2
    in  PrismMap (m, sndMapFromList $ Map.toList m)

difference :: (Ord k, Ord a) => PrismMap k a -> PrismMap k a -> PrismMap k a
difference (PrismMap (m1, _)) (PrismMap (m2, _)) =
    let m = Map.difference m1 m2
    in  PrismMap (m, sndMapFromList $ Map.toList m)
