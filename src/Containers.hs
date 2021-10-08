{-# LANGUAGE FlexibleInstances, FunctionalDependencies, DefaultSignatures, GeneralizedNewtypeDeriving #-}

-- |Module `Containers` include a lot of general methods for working with
--  containers. All methods are scattered by classes.
module Containers (
    S.Set,
    M.Map,
    IM.IsoMap,
    PM.PrismMap,
    P.Pair (..),
    Q.Quadruple (..),
    Index,
    index,
    Sizable (..),
    Nullable (..),
    Singletonable (..),
    Valuable (..),
    Keyable (..),
    Indexable (..),
    Slicable (..),
    Operable (..),
    Listable (..),
    UnsafeListable (..),
    FastUnsafeListable (..),
    Containable (..),
    Gunctor (..),
    Filterable (..),
    Insertable (..),
    Deletable (..),
  ) where

import qualified Containers.Set as S
import qualified Containers.Map as M
import qualified Containers.IsoMap as IM
import qualified Containers.PrismMap as PM
import qualified Containers.Pair as P
import qualified Containers.Quadruple as Q
import Lens

import Control.Monad.State.Lazy (MonadState)
import Data.Maybe (fromMaybe)
import qualified Data.List as L

infixl 9 !?, !<>
infixr 9 .@, .<@>
infixr 7 +>, \>
infixl 6 <+, <\
infixl 5 \/, /\, \\
infixr 4 <+~, <\~, \/~, /\~, \\~, <?>~
infix  4 <+=, <\=, \/=, /\=, \\=, <?>=, <?>, <@>

newtype Index = Index Int
    deriving (Eq, Ord, Num, Enum)

index :: Int -> Index
index = Index

class Sizable c where
    size :: c -> Int

instance Sizable [a] where
    size = length

instance Sizable (S.Set a) where
    size = S.size

instance Sizable (M.Map k a) where
    size = M.size

instance Sizable (IM.IsoMap a b) where
    size = IM.size

instance Sizable (PM.PrismMap k a) where
    size = PM.size

instance Sizable (P.Pair a) where
    size = const 2

instance Sizable (Q.Quadruple a) where
    size = const 4

class Nullable c where
    nullC :: c -> Bool
    default nullC :: Sizable c => c -> Bool
    nullC c = size c == 0
    emptyC :: c

instance Nullable [a] where
    nullC = L.null
    emptyC = []

instance Nullable (S.Set a) where
    nullC = S.null
    emptyC = S.empty

instance Nullable (M.Map k a) where
    nullC = M.null
    emptyC = M.empty

instance Nullable (IM.IsoMap a b) where
    emptyC = IM.empty

instance Nullable (PM.PrismMap k a) where
    nullC = PM.null
    emptyC = PM.empty

class Singletonable c v | c -> v where
    singleton :: v -> c

instance Singletonable [a] a where
    singleton = pure

instance Singletonable (S.Set a) a where
    singleton = S.singleton

instance Singletonable (M.Map k a) (k, a) where
    singleton = uncurry M.singleton

instance (Ord a, Ord b) => Singletonable (IM.IsoMap a b) (a, b) where
    singleton = uncurry IM.singleton

instance Singletonable (PM.PrismMap k a) (k, a) where
    singleton = uncurry PM.singleton

class Valuable c v where
    values :: c -> [v]
    default values :: Ord v => c -> [v]
    values = S.toList . valuesSet
    valuesSet :: Ord v => c -> S.Set v
    valuesSet = S.fromList . values

instance Valuable [a] a where
    values = id

instance Ord a => Valuable (S.Set a) a where
    valuesSet = id

instance Valuable (M.Map k a) a where
    values = M.elems

instance (Ord a, Ord b) => Valuable (IM.IsoMap a b) a where
    valuesSet = IM.values

instance (Ord a, Ord b) => Valuable (IM.IsoMap a b) b where
    valuesSet = IM.values

instance Valuable (PM.PrismMap k a) a where
    values = PM.values

instance Valuable (P.Pair a) a where
    values = P.toList

instance Valuable (Q.Quadruple a) a where
    values = Q.toList

class Keyable c k | c -> k where
    keys :: c -> [k]
    default keys :: Ord k => c -> [k]
    keys = S.toList . keysSet
    keysSet :: Ord k => c -> S.Set k
    keysSet = S.fromList . keys

instance Keyable [a] Index where
    keys = (\n -> index <$> [0..n-1]) . length
    keysSet = S.fromDistinctAscList . keys

instance Ord k => Keyable (M.Map k a) k where
    keys = M.keys
    keysSet = M.keysSet

instance Keyable (PM.PrismMap k a) k where
    keys = PM.keys
    keysSet = PM.keysSet

instance Keyable (P.Pair a) Index where
    keys = const $ index <$> [0, 1]
    keysSet = S.fromDistinctAscList . keys

instance Keyable (Q.Quadruple a) Index where
    keys = const $ index <$> [0..3]
    keysSet = S.fromDistinctAscList . keys

class Indexable c k v | c k -> v, c v -> k where
    (!?) :: MonadFail m => c -> k -> m v
    (!?) c k =
        let vs = c !<> k
        in  case size vs of
                0 -> fail "Key is not in container"
                1 -> return $ head $ S.toList vs
                _ -> fail "There are more than one element by key in container"
    (!<>) :: c -> k -> S.Set v
    (!<>) c k =
        case c !? k of
            Nothing -> S.empty
            Just v  -> S.singleton v
    (.@) :: MonadFail m => Getting x y c -> k -> Getting x y (m v)
    (.@) = (. to . flip (!?)) . (.)
    (.<@>) :: Getting x y c -> k -> Getting x y (S.Set v)
    (.<@>) = (. to . flip (!<>)) . (.)

instance Indexable [a] Index a where
    l !? (Index i)
        | i < 0 = fail "Negative index"
        | otherwise = go l i where
            go [] _ = fail "Index too large"
            go (x:_) 0 = return x
            go (_:xs) n = go xs (n-1)

instance Eq a => Indexable [a] a Index where
    l !? a =
        maybe (fail "Element is not in list") return $
            index <$> L.elemIndex a l
    l !<> a = S.fromDistinctAscList $ index <$> L.elemIndices a l

instance Indexable (S.Set a) Index a where
    s !? (Index i)
        | i < 0       = fail "Negative index"
        | i >= size s = fail "Index too large"
        | otherwise   = return $ S.elemAt i s

instance Ord k => Indexable (M.Map k a) k a where
    m !? k =
        case m M.!? k of
            Nothing -> fail "Key is not found"
            Just v  -> return v

instance (Ord a, Ord b) => Indexable (IM.IsoMap a b) a b where
    (!?) = (IM.~$)

instance (Ord a, Ord b) => Indexable (IM.IsoMap a b) b a where
    (!?) = (IM.~$)

instance Ord k => Indexable (PM.PrismMap k a) k a where
    (!?) = flip PM.lookup

instance Ord a => Indexable (PM.PrismMap k a) a k where
    (!<>) = flip PM.lookupKeys

instance Indexable (P.Pair a) Index a where
    (P.Pair (x, _)) !? (Index 0) = return x
    (P.Pair (_, y)) !? (Index 1) = return y
    _ !? _ = fail "Illegal index"

instance Indexable (Q.Quadruple a) Index a where
    (Q.Quadruple (x, _, _, _)) !? (Index 0) = return x
    (Q.Quadruple (_, y, _, _)) !? (Index 1) = return y
    (Q.Quadruple (_, _, z, _)) !? (Index 2) = return z
    (Q.Quadruple (_, _, _, w)) !? (Index 3) = return w
    _ !? _ = fail "Illegal index"

class Slicable c k where
    takeC :: k -> c -> c
    dropC :: k -> c -> c
    splitAtC :: k -> c -> (c, c)
    splitAtC k c = (takeC k c, dropC k c)

instance Slicable [a] Index where
    takeC (Index i) = L.take i
    dropC (Index i) = L.drop i
    splitAtC (Index i) = L.splitAt i

instance Slicable [a] (a -> Bool) where
    takeC = L.takeWhile
    dropC = L.dropWhile
    splitAtC = L.span

instance Slicable (S.Set a) Index where
    takeC (Index i) = S.take i
    dropC (Index i) = S.drop i
    splitAtC (Index i) = S.splitAt i

instance Slicable (S.Set a) (a -> Bool) where
    takeC = S.takeWhileAntitone
    dropC = S.dropWhileAntitone
    splitAtC = S.spanAntitone

class Operable c where
    union :: c -> c -> c
    (\/) :: c -> c -> c
    (\/) = union
    (\/~) :: ASetter s t c c -> c -> s -> t
    (\/~) = (. flip union) . (%~)
    (\/=) :: MonadState s m => ASetter s s c c -> c -> m ()
    (\/=) = (. flip union) . (%=)
    intersection :: c -> c -> c
    (/\) :: c -> c -> c
    (/\) = intersection
    (/\~) :: ASetter s t c c -> c -> s -> t
    (/\~) = (. flip intersection) . (%~)
    (/\=) :: MonadState s m => ASetter s s c c -> c -> m ()
    (/\=) = (. flip intersection) . (%=)
    difference :: c -> c -> c
    (\\) :: c -> c -> c
    (\\) = difference
    (\\~) :: ASetter s t c c -> c -> s -> t
    (\\~) = (. flip difference) . (%~)
    (\\=) :: MonadState s m => ASetter s s c c -> c -> m ()
    (\\=) = (. flip difference) . (%=)

instance Eq a => Operable [a] where
    union = L.union
    intersection = L.intersect
    difference = (L.\\)

instance Ord a => Operable (S.Set a) where
    union = S.union
    intersection = S.intersection
    difference = S.difference

instance Ord k => Operable (M.Map k a) where
    union = M.union
    intersection = M.intersection
    difference = M.difference

instance (Ord k, Ord a) => Operable (PM.PrismMap k a) where
    union = PM.union
    intersection = PM.intersection
    difference = PM.difference

class Listable c v | c -> v where
    toList :: c -> [v]
    default toList :: Valuable c v => c -> [v]
    toList = values
    fromList_ :: MonadFail m => [v] -> m c
    default fromList_ :: (UnsafeListable c v, MonadFail m) => [v] -> m c
    fromList_ = return . fromList

instance Listable [a] a

instance Ord a => Listable (S.Set a) a

instance Ord k => Listable (M.Map k a) (k, a) where
    toList = M.toList

instance (Ord a, Ord b) => Listable (IM.IsoMap a b) (a, b) where
    toList = IM.toList
    fromList_ = IM.fromList

instance (Ord k, Ord a) => Listable (PM.PrismMap k a) (k, a) where
    toList = PM.toList

instance Listable (P.Pair a) a where
    fromList_ = P.fromList

instance Listable (Q.Quadruple a) a where
    fromList_ = Q.fromList

class UnsafeListable c v | c -> v where
    fromList :: [v] -> c

instance UnsafeListable [a] a where
    fromList = id

instance Ord a => UnsafeListable (S.Set a) a where
    fromList = S.fromList

instance Ord k => UnsafeListable (M.Map k a) (k, a) where
    fromList = M.fromList

instance (Ord a, Ord b) => UnsafeListable (IM.IsoMap a b) (a, b) where
    fromList = IM.fromList'

instance (Ord k, Ord a) => UnsafeListable (PM.PrismMap k a) (k, a) where
    fromList = PM.fromList

class FastUnsafeListable c v | c -> v where
    fromAscList :: [v] -> c
    fromDistinctAscList :: [v] -> c
    fromDescList :: [v] -> c
    fromDistinctDescList :: [v] -> c

instance Eq a => FastUnsafeListable (S.Set a) a where
    fromAscList = S.fromAscList
    fromDistinctAscList = S.fromDistinctAscList
    fromDescList = S.fromDescList
    fromDistinctDescList = S.fromDistinctDescList

instance Eq k => FastUnsafeListable (M.Map k a) (k, a) where
    fromAscList = M.fromAscList
    fromDistinctAscList = M.fromDistinctAscList
    fromDescList = M.fromDescList
    fromDistinctDescList = M.fromDistinctDescList

class Containable c v where
    member :: v -> c -> Bool
    member = (fmap.fmap) not notMember
    notMember :: v -> c -> Bool
    notMember = (fmap.fmap) not member

instance Eq a => Containable [a] a where
    member = elem

instance Containable [a] Index where
    member (Index i) l = i >= 0 && atLeast (i+1) l where
        atLeast 0 _ = True
        atLeast _ [] = False
        atLeast n (_:xs) = atLeast (n-1) xs

instance Ord a => Containable (S.Set a) a where
    member = S.member

instance Ord k => Containable (M.Map k a) k where
    member = M.member

instance Eq a => Containable (M.Map k a) a where
    member = elem

instance (Ord k, Eq a) => Containable (M.Map k a) (k, a) where
    member (k, a) m = m M.!? k == Just a

instance (Ord a, Ord b) => Containable (IM.IsoMap a b) a where
    member = IM.member

instance (Ord a, Ord b) => Containable (IM.IsoMap a b) b where
    member = IM.member

instance Ord k => Containable (PM.PrismMap k a) k where
    member = PM.key

instance Ord a => Containable (PM.PrismMap k a) a where
    member = PM.value

instance (Ord k, Ord a) => Containable (PM.PrismMap k a) (k, a) where
    member (k, a) pm = PM.lookup k pm == Just a

instance Eq a => Containable (P.Pair a) a where
    member = elem

instance Eq a => Containable (Q.Quadruple a) a where
    member = elem

class Gunctor c1 c2 v1 v2 | c1 c2 -> v1 v2 where
    gmap :: (v1 -> v2) -> c1 -> c2
    gmap = (<@>)
    (<@>) :: (v1 -> v2) -> c1 -> c2
    (<@>) = gmap

instance Ord b => Gunctor (S.Set a) (S.Set b) a b where
    gmap = S.map

instance Ord l => Gunctor (M.Map k a) (M.Map l a) k l where
    gmap = M.mapKeys

instance Gunctor (M.Map k a) (M.Map k b) a b where
    gmap = M.map

instance Ord a2 => Gunctor (IM.IsoMap a1 b) (IM.IsoMap a2 b) a1 a2 where
    gmap = IM.mapFst

instance Ord b2 => Gunctor (IM.IsoMap a b1) (IM.IsoMap a b2) b1 b2 where
    gmap = IM.mapSnd

instance Ord a2 => Gunctor (IM.IsoMap a1 b) (IM.IsoMap b a2) a1 a2 where
    gmap = IM.mapFstRev

instance Ord b2 => Gunctor (IM.IsoMap a b1) (IM.IsoMap b2 a) b1 b2 where
    gmap = IM.mapSndRev

instance Ord l => Gunctor (PM.PrismMap k a) (PM.PrismMap l a) k l where
    gmap = PM.mapKeys

instance Ord b => Gunctor (PM.PrismMap k a) (PM.PrismMap k b) a b where
    gmap = PM.map

class Filterable c v where
    filterC :: (v -> Bool) -> c -> c
    (<?>) :: (v -> Bool) -> c -> c
    (<?>) = filterC
    (<?>~) :: ASetter s t c c -> (v -> Bool) -> s -> t
    (<?>~) = (. filterC) . (%~)
    (<?>=) :: MonadState s m => ASetter s s c c -> (v -> Bool) -> m ()
    (<?>=) = (. filterC) . (%=)

instance Filterable [a] a where
    filterC = L.filter

instance Filterable (S.Set a) a where
    filterC = S.filter

instance Filterable (M.Map k a) k where
    filterC = M.filterWithKey . fmap const

instance Filterable (M.Map k a) a where
    filterC = M.filter

instance Filterable (M.Map k a) (k, a) where
    filterC = M.filterWithKey . curry

instance (Ord a, Ord b) => Filterable (IM.IsoMap a b) a where
    filterC = IM.filter

instance (Ord a, Ord b) => Filterable (IM.IsoMap a b) b where
    filterC = IM.filter

instance Filterable (PM.PrismMap k a) k where
    filterC = PM.filterKeys

instance Filterable (PM.PrismMap k a) a where
    filterC = PM.filter

class Insertable c v where
    insert :: MonadFail m => v -> c -> m c
    default insert :: (Containable c v, MonadFail m) => v -> c -> m c
    insert v c =
        if v `member` c
        then fail "Element already is in container"
        else return $ unsafeInsert v c
    unsafeInsert :: v -> c -> c
    unsafeInsert v = fromMaybe <*> insert v
    (+>) :: v -> c -> c
    (+>) = unsafeInsert
    (<+) :: c -> v -> c
    (<+) = flip unsafeInsert
    (<+~) :: ASetter s t c c -> v -> s -> t
    (<+~) = (. unsafeInsert) . (%~)
    (<+=) :: MonadState s m => ASetter s s c c -> v -> m ()
    (<+=) = (. unsafeInsert) . (%=)

instance Insertable [a] a where
    insert = (fmap.fmap) return unsafeInsert
    unsafeInsert = (:)
    (<+) = (. pure) . (++)

instance Ord a => Insertable (S.Set a) a where
    unsafeInsert = S.insert

instance (Ord k, Eq a) => Insertable (M.Map k a) (k, a) where
    unsafeInsert = uncurry M.insert

instance (Ord a, Ord b) => Insertable (IM.IsoMap a b) (a, b) where
    insert = IM.insert

instance (Ord a, Ord b) => Insertable (IM.IsoMap a b) (b, a) where
    insert = IM.insert

instance (Ord k, Ord a) => Insertable (PM.PrismMap k a) (k, a) where
    unsafeInsert = uncurry PM.insert

class Deletable c v where
    delete :: MonadFail m => v -> c -> m c
    default delete :: (Containable c v, MonadFail m) => v -> c -> m c
    delete v c =
        if v `member` c
        then return $ unsafeDelete v c
        else fail "Element is not in container"
    unsafeDelete :: v -> c -> c
    unsafeDelete v = fromMaybe <*> delete v
    (\>) :: v -> c -> c
    (\>) = unsafeDelete
    (<\) :: c -> v -> c
    (<\) = flip unsafeDelete
    (<\~) :: ASetter s t c c -> v -> s -> t
    (<\~) = (. unsafeDelete) . (%~)
    (<\=) :: MonadState s m => ASetter s s c c -> v -> m ()
    (<\=) = (. unsafeDelete) . (%=)

instance Eq a => Deletable [a] a where
    unsafeDelete = L.delete

instance Deletable [a] Index where
    unsafeDelete (Index i) l = take i l ++ drop (i+1) l

instance Ord a => Deletable (S.Set a) a where
    unsafeDelete = S.delete

instance Ord k => Deletable (M.Map k a) k where
    unsafeDelete = M.delete

instance (Ord a, Ord b) => Deletable (IM.IsoMap a b) a where
    unsafeDelete = IM.delete

instance (Ord a, Ord b) => Deletable (IM.IsoMap a b) b where
    unsafeDelete = IM.delete

instance (Ord k, Ord a) => Deletable (PM.PrismMap k a) k where
    delete = PM.deleteKey

instance (Ord k, Ord a) => Deletable (PM.PrismMap k a) a where
    delete = PM.deleteValue
