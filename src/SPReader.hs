module SPReader where

import TMTypes
import TMReader (TMReader, runTMReader)
import qualified TMReader
import SPTypes
import TM2SP (relations)
import qualified Set

type SPInfo = (SemigroupPresentation, Int, Int, Int)

type SPReader a = SPInfo -> a

runSPReader :: SPReader a -> TuringMachine -> a
runSPReader spr = spr . (runTMReader $ do
    rs <- relations
    n <- TMReader.getN
    m <- TMReader.getM
    let l = Set.size rs
    return (SP rs, n, m, l))

getS :: SPReader SemigroupPresentation
getS (s, _, _, _) = s

getN :: SPReader Int
getN (_, n, _, _) = n

getM :: SPReader Int
getM (_, _, m, _) = m

getL :: SPReader Int
getL (_, _, _, l) = l

fromTMReader :: TMReader a -> SPReader a
fromTMReader tmr = do
    n <- getN
    m <- getM
    return $ tmr (undefined, n, m, undefined)
