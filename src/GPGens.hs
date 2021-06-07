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

q_ :: Int -> SPReader Element
q_ x = return $ element x

p_ :: Int -> SPReader Element
p_ x = do
    n <- getN
    return $ element (x + n + 1)

h :: SPReader Element
h = do
    n <- getN
    return $ element (2*n + 2)

s_ :: Int -> SPReader Element
s_ i = do
    n <- getN
    return $ element (i + 2*n + 3)

r_ :: Int -> SPReader Element
r_ i = do
    n <- getN
    m <- getM
    return $ element (i + 2*n + m + 3)

x :: SPReader Element
x = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (2*n + m + l + 4)

t :: SPReader Element
t = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (2*n + m + l + 5)

k :: SPReader Element
k = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (2*n + m + l + 6)

convertG :: SP.Generator -> SPReader Element
convertG = (fromTMReader . toString) >=> fromString where
    fromString :: String -> SPReader Element
    fromString = \case
        'q':'_':num -> q_ (read num)
        'p':'_':num -> p_ (read num)
        "h"         -> h
        's':'_':num -> s_ (read num)
        other       -> error $ show other ++ " isn't valid generator"

convertW :: SP.GWord -> SPReader EWord
convertW = sequence . map convertG
