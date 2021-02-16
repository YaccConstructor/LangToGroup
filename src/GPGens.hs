{-# LANGUAGE LambdaCase #-}

module GPGens where

import SPReader
import SPGens (toString)
import qualified SPTypes as SP
import GPTypes
import Control.Monad ((>=>), (=<<))

element :: Int -> Element
element = Positive . G

neg :: Element -> Element
neg (Positive g) = Negative g
neg (Negative g) = Positive g

(^~) :: SPReader Element -> SPReader Element
(^~) = fmap neg

q :: SPReader Element
q = return $ element 0

q_ :: Int -> SPReader Element
q_ i = return $ element (i + 1)

s_ :: Int -> SPReader Element
s_ i = do
    n <- getN
    return $ element (i + n + 2)

r_ :: Int -> SPReader Element
r_ i = do
    n <- getN
    m <- getM
    return $ element (i + n + m + 3)

x :: SPReader Element
x = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (n + m + l + 4)

t :: SPReader Element
t = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (n + m + l + 5)

k :: SPReader Element
k = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (n + m + l + 6)

convertG :: SP.Generator -> SPReader Element
convertG = (fromTMReader . toString) >=> fromString where
    fromString :: String -> SPReader Element
    fromString = \case
        "q"         -> q
        "h"         -> s_ =<< getM
        's':'_':num -> s_ (read num)
        'q':'_':num -> q_ (read num)
        other       -> error $ "Can not convert '" ++ other ++ "' to Element"

convertW :: SP.GWord -> SPReader EWord
convertW = mapM convertG
