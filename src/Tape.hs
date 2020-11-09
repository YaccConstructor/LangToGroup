module Tape where

import TMTypes
import Data.List.NonEmpty (NonEmpty(..), nonEmpty, (<|), toList)
import qualified Data.List.NonEmpty as NEList (head, tail)
import Data.Maybe (fromMaybe)

head' :: NonEmpty a -> a
head' = NEList.head

tail' :: a -> NonEmpty a -> NonEmpty a
tail' b = nonEmpty' b . NEList.tail

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
fromList as ind b = Tape (take' ind as) (as !! ind) (drop' (ind + 1) as) b where
    take' i = nonEmpty' b . take i
    drop' i = nonEmpty' b . drop i

instance Show a => Show (Tape a) where
    show (Tape l t r _) = concat ((++ " | ") <$> show <$> reverse (toList l)) ++ "[" ++ show t ++ "]" ++ concat ((" | " ++) <$> show <$> toList r)
