module GroupPresentation.Element (
    Signed(..),
    Element,
    element,
    (^~),
    isPositive,
    getGenerator,
    EWord,
    module GroupPresentation.Generator,
  ) where

import GroupPresentation.Generator

infix 9 ^~

data Signed a =
      Positive { unSigned :: a }
    | Negative { unSigned :: a }
    deriving (Eq, Ord)

instance Functor Signed where
    fmap f (Positive x) = Positive $ f x
    fmap f (Negative x) = Negative $ f x

instance Applicative Signed where
    pure = Positive
    (Positive f) <*> (Positive x) = Positive (f x)
    (Positive f) <*> (Negative x) = Negative (f x)
    (Negative f) <*> (Positive x) = Negative (f x)
    (Negative f) <*> (Negative x) = Positive (f x)

instance Foldable Signed where
    foldr f z s = f (unSigned s) z

instance Traversable Signed where
    traverse f (Positive x) = Positive <$> f x
    traverse f (Negative x) = Negative <$> f x

(^~) :: Signed a -> Signed a
(^~) (Positive x) = Negative x
(^~) (Negative x) = Positive x

isPositive :: Signed a -> Bool
isPositive (Positive _) = True
isPositive (Negative _) = False

type Element = Signed Generator

element :: Int -> Element
element = pure . generator

getGenerator :: Element -> Generator
getGenerator = unSigned

type EWord = [Element]
