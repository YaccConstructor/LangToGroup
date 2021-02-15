module SPKnuthBendix where

import TMTesting
import SPTypes
import TM2SP
import qualified Set
import System.Timeout (timeout)
import Data.Maybe (catMaybes)
import Math.Algebra.Group.StringRewriting (knuthBendix)
import Control.Exception (evaluate)

test :: IO [Int]
test = fmap catMaybes $ sequence
    [
        timeout 10000000 $ testTM i >> return i
    | i <- [1 .. length testingSet]
    ]

testTM :: Int -> IO ()
testTM tmi =
    let tm = testingTM tmi
        -- maxGi =
        --     flip runTMReader tm $ ((+ 3) . sum) <$> sequence [getN, getM]
        SP rs = semigroupGamma tm
        rules = map (\(gw1 `Equals` gw2) -> (gw1, gw2)) (Set.unSet rs)
    in  evaluate (knuthBendix rules) >> return ()
