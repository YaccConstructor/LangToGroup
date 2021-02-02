module TMReader where

import TMTypes
import qualified Data.Map.Lazy as Map

type TMInfo = (TuringMachine, Int, Int, Bool)

type TMReader a = TMInfo -> a

runTMReader :: TMReader a -> TuringMachine -> a
runTMReader tmr tm@(TM qs) = tmr (tm, n, m, e) where
    (n, m, e) = foldr maxAmountQandS (1, 0, False) (Map.toList qs)
    maxAmountQandS ((Q w, S x), (C (S y), Q z)) (n', m', e') =
        (max n' $ max w z, max m' $ max x y, e' || w == (-1) || z == (-1))
    maxAmountQandS ((Q w, S x), (_,       Q z)) (n', m', e') =
        (max n' $ max w z, max m' x, e' || w == (-1) || z == (-1))

getT :: TMReader TuringMachine
getT (t, _, _, _) = t

getN :: TMReader Int
getN (_, n, _, _) = n

getM :: TMReader Int
getM (_, _, m, _) = m

getE :: TMReader Bool
getE (_, _, _, e) = e
