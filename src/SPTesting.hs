module SPTesting where

import TMTypes
import TMReader
import TMTesting
import SPTypes
import SPGens
import TM2SP
import qualified Set
import qualified Math.Algebra.Group.StringRewriting as SR
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

testSPFull :: TuringMachine -> Map Char Int -> String -> Bool
testSPFull tm alphabet word = flip runTMReader tm $ do
    let SP rs = semigroupGamma tm
        rules = filter (\(x, y) -> x /= y) $
                (\(gw1 `Equals` gw2) -> (gw1, gw2)) <$> Set.toList rs
    initWord <- sequence $
        [h, q_ 1] ++
        (
            if null word
            then [s_ 0]
            else (s_ . (Map.!) alphabet) <$> word
        ) ++
        [h]
    finalWord <- sequence [q]
    return $ SR.rewrite rules initWord == finalWord

testSP :: Int -> String -> Bool
testSP tmi = testSPFull tm alphabet where
    (_, allChars, Just tm) = testingSet !! (tmi - 1)
    alphabet = Map.fromList $ zip allChars [1..]
