{-# LANGUAGE LambdaCase #-}

module SPGens where

import TMReader
import SPTypes

q :: TMReader Generator
q = return $ G 0

h :: TMReader Generator
h = return $ G 1

s_ :: Int -> TMReader Generator
s_ x = return $ G (x + 2)

q_ :: Int -> TMReader Generator
q_ x = do
    m <- getM
    return $ G (x + m + 3)

isQ :: TMReader (Generator -> Bool)
isQ = do
    m <- getM
    return $
        \(G x) -> x == 0 || x >= m + 3

toString :: Generator -> TMReader String
toString = flip $ do
    m <- getM
    return $ \case
        G 0             -> "q"
        G 1             -> "h"
        G x | x < m + 3 -> "s_" ++ show (x - 2)
        G x | otherwise -> "q_" ++ show (x - m - 3)
