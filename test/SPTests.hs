module SPTests where

import TMTestSet
import TMReader
import TMTesting
import SPTypes
import SPGens
import TM2SP
import qualified Set
import SPSolver (solve)
import Test.HUnit
import qualified Data.Map.Lazy as Map

tests :: Test
tests = TestLabel "Correctness of constructing semigroup from machine" $
    TestList $ do
        (tmi, testsTM) <- testSet
        let (msg, alphabet, tm) = testingTMInfo tmi
            SP rs = semigroupGamma tm
            q_0 = runTMReader (q_ 0) tm
            rules = flip filter (Set.toList rs) $
                \(gw1 `Equals` gw2) ->
                    gw1 /= gw2 && q_0 `notElem` gw1
        return $ TestLabel msg $
            TestList $ do
                (word, answer) <- testsTM
                return $ TestLabel word $
                    flip runTMReader tm $ do
                        let depth =
                                if answer == True
                                then Nothing
                                else Just 242
                        initWord <- sequence $
                            [h_0, q_ 1] ++
                            (
                                if null word
                                then [s_ 0]
                                else (s_ . (Map.!) alphabet) <$> word
                            ) ++
                            [h_1]
                        return $ (~=?) answer $
                            any (q_0 `elem`) $
                                solve depth rules initWord
