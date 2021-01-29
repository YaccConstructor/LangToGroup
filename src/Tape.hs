module Tape where

import TMTypes
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, (<|))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.List.NonEmpty as NEList (head, tail)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

head' :: NonEmpty a -> a
head' = NEList.head

tail' :: a -> NonEmpty a -> NonEmpty a
tail' b = nonEmpty' b . NEList.tail

take' :: Int -> a -> [a] -> NonEmpty a
take' i b l = nonEmpty' b $
    if i >= length l
    then l ++ replicate (i - length l) b
    else take i l

drop' :: Int -> a -> [a] -> NonEmpty a
drop' i b l = nonEmpty' b $
    if i < 0
    then replicate (-i) b ++ l
    else drop i l

index' :: Int -> a -> [a] -> a
index' i b l =
    if i < 0 || i >= length l
    then b
    else l !! i

nonEmpty' :: a -> [a] -> NonEmpty a
nonEmpty' b as = fromMaybe (b :| []) $ nonEmpty as

data Tape a = Tape {
        left  :: NonEmpty a,
        top   :: a,
        right :: NonEmpty a,
        blank :: a
    }

toLeft :: Eq a => Tape a -> Tape a
toLeft (Tape l t r b) = Tape (tail' b l) (head' l) (r') b where
    r' = if r == b :| []
         then t :| []
         else t <| r

toRight :: Eq a => Tape a -> Tape a
toRight (Tape l t r b) = Tape (l') (head' r) (tail' b r) b where
    l' = if l == b :| []
         then t :| []
         else t <| l

fromList :: [a] -> Int -> a -> Tape a
fromList as ind b = Tape (take' ind b as) (index' ind b as) (drop' (ind + 1) b as) b

instance Show a => Show (Tape a) where
    show (Tape l t r _) =
        intercalate " " (show <$> reverse (NonEmpty.toList l)) ++
        "[" ++
        show t ++
        "]" ++
        intercalate " " (show <$> NonEmpty.toList r)
