{-# LANGUAGE LambdaCase #-}

module SPGens where

import TMReader
import SPTypes

q_ :: Int -> TMReader Generator
q_ x = return $ G x

p_ :: Int -> TMReader Generator
p_ x = do
    n <- getN
    return $ G (x + n + 1)

h :: TMReader Generator
h = do
    n <- getN
    return $ G (2*n + 2)

s_ :: Int -> TMReader Generator
s_ x = do
    n <- getN
    return $ G (x + 2*n + 3)

isQ :: TMReader (Generator -> Bool)
isQ = do
    n <- getN
    return $
        \(G x) -> x >= 0 && x < 2*n + 2

toString :: Generator -> TMReader String
toString = flip $ do
    n <- getN
    m <- getM
    return $ \case
        G x | x >= 0       && x <= n           -> "q_" ++ show x
        G x | x >= n + 1   && x <= 2*n + 1     -> "p_" ++ show (x - n - 1)
        G x | x == (2*n + 2)                   -> "h"
        G x | x >= 2*n + 3 && x <= 2*n + m + 3 -> "s_" ++ show (x - 2*n - 3)
        G x | otherwise -> error $ "G " ++ show x ++ " isn't valid generator"
