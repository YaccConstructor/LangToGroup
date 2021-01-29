module SPTests where

import Test.HUnit
import TMTypes
import TMReader
import TMTesting
import SPTypes
import SPGens
import TM2SP
import qualified Set
import SPSolver (solve)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

testSet :: [(Int, [(String, Bool)])]
testSet = [
    (1, [
        ("", True),
        ("a", True),
        ("aa", True),
        ("aaa", True),
        ("aaaa", True)
    ]),
    (2, [
        ("", False),
        ("a", True),
        ("aa", True),
        ("aaa", True),
        ("aaaa", True)
    ]),
    (3, [
        ("", True),
        ("a", True),
        ("aa", False)
    ]),
    (4, [
        ("", False),
        ("a", True),
        ("b", True),
        ("ab", False),
        ("bba", False)
    ]),
    (5, [
        ("", False),
        ("ac", False),
        ("bc", False),
        ("aba", False),
        ("abc", True)
    ]),
    (6, [
        ("", False),
        ("aba", False),
        ("baba", False),
        ("ababa", True),
        ("ababaa", False)
    ]),
    (7, [
        ("", False),
        ("a", False),
        ("abc", False),
        ("aba", True),
        ("abca", False),
        ("abcb", False),
        ("abcba", True),
        ("abcbcba", True)
    ]),
    (8, [
        ("", True),
        ("a", False),
        ("b", False),
        ("ab", True),
        ("bab", False),
        ("abab", True),
        ("abba", False),
        ("aabb", True)
    ])
  ]

tests :: Test
tests = TestLabel "Correctness of constructing semigroup from machine" $
    TestList $ do
        (tmi, testsTM) <- testSet
        let (msg, allChars, Just tm) = testingSet !! (tmi - 1)
            sp = semigroupGamma tm
            alphabet = Map.fromList $ zip allChars [1..]
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
                            [h, q_ 1] ++
                            (
                                if null word
                                then [s_ 0]
                                else (s_ . (Map.!) alphabet) <$> word
                            ) ++
                            [h]
                        finalWord <- sequence [q]
                        return $
                            finalWord `elem` solve depth sp initWord ~?= answer
