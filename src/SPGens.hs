{-# LANGUAGE LambdaCase #-}

module SPGens where

import TMReader
import SPTypes

q_ :: Int -> TMReader Generator
q_ x = return $ G x

h_0 :: TMReader Generator
h_0 = do
    n <- getN
    return $ G (n + 1)

h_1 :: TMReader Generator
h_1 = do
    n <- getN
    return $ G (n + 2)

s_ :: Int -> TMReader Generator
s_ x = do
    n <- getN
    return $ G (x + n + 3)

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
        G x | x == (n + 1)                 -> "h_0"
        G x | x == (n + 2)                 -> "h_1"
        G x | x >= n + 3 && x <= n + m + 3 -> "s_" ++ show (x - n - 3)
        G x | otherwise -> error $ "G " ++ show x ++ " isn't valid generator"
