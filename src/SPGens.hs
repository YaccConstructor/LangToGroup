{-# LANGUAGE LambdaCase #-}

module SPGens where

import TMReader
import SPTypes

q_ :: Int -> TMReader Generator
q_ x = return $ G x

h :: TMReader Generator
h = do
    n <- getN
    return $ G (n + 1)

s_ :: Int -> TMReader Generator
s_ x = do
    n <- getN
    return $ G (x + n + 2)

isQ :: TMReader (Generator -> Bool)
isQ = do
    n <- getN
    return $
        \(G x) -> x >= 0 && x <= n

toString :: Generator -> TMReader String
toString = flip $ do
    n <- getN
    m <- getM
    return $ \case
        G x | x >= 0     && x <= n         -> "q_" ++ show x
        G x | x == (n + 1)                 -> "h"
        G x | x >= n + 2 && x <= n + m + 2 -> "s_" ++ show (x - n - 2)
        G x | otherwise -> error $ "G " ++ show x ++ " isn't valid generator"
