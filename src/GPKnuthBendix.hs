module GPKnuthBendix where

import TMTesting
import SPReader
import GPTypes
import GPGens
import SP2GP
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
        maxGi =
            flip runSPReader tm $ ((+ 6) . sum) <$> sequence [getN, getM, getL]
        GP rs = groupBeta tm
        rules =
            map (\(ew1 `Equals` ew2) -> (ew1, ew2)) (Set.unSet rs) ++
            [
                ([a, neg a], [])
            |
                i <- [0..maxGi],
                a <- [Positive, Negative] <*> [G i]
            ]
    in  evaluate (knuthBendix rules) >> return ()
