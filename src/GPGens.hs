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

h :: SPReader Element
h = do
    n <- getN
    return $ element (n + 1)

s_ :: Int -> SPReader Element
s_ i = do
    n <- getN
    return $ element (i + n + 2)

r_ :: Int -> SPReader Element
r_ i = do
    n <- getN
    m <- getM
    return $ element (i + n + m + 2)

x :: SPReader Element
x = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (n + m + l + 3)

t :: SPReader Element
t = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (n + m + l + 4)

k :: SPReader Element
k = do
    n <- getN
    m <- getM
    l <- getL
    return $ element (n + m + l + 5)

convertG :: SP.Generator -> SPReader Element
convertG = (fromTMReader . toString) >=> fromString where
    fromString :: String -> SPReader Element
    fromString = \case
        'q':'_':num -> q_ (read num)
        "h"       -> h
        's':'_':num -> s_ (read num)
        other       -> error $ show other ++ " isn't valid generator"

convertW :: SP.GWord -> SPReader EWord
convertW = sequence . map convertG
