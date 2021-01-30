module SPTests where

import Test.HUnit
import TMReader
import TMTesting
import SPTypes
import SPGens
import TM2SP
import qualified Set
import SPSolver (solve)
import qualified Data.Map.Lazy as Map

testSet :: [(Int, [(String, Bool)])]
testSet = [
    (1, [
        ("", True),
        ("a", True),
        ("aa", True),
        ("aaa", True),
        ("aaaa", True),
        ("aaaaa", True)
    ]),
    (2, [
        ("", False),
        ("a", True),
        ("aa", True),
        ("aaa", True),
        ("aaaa", True),
        ("aaaaa", True)
    ]),
    (3, [
        ("", True),
        ("a", True),
        ("aa", False),
        ("aaa", False)
    ]),
    (4, [
        ("", False),
        ("a", True),
        ("b", True),
        ("ab", False),
        ("bb", False),
        ("baa", False)
    ]),
    (5, [
        ("", False),
        ("ac", False),
        ("bc", False),
        ("aba", False),
        ("abc", True),
        ("abca", False),
        ("abcc", False)
    ]),
    (6, [
        ("", False),
        ("a", False),
        ("b", False),
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
        ("abcbcba", True),
        ("abcbaba", False),
        ("abcbcbca", False),
        ("abcbcbcba", True),
        ("abcbcbcbcba", True)
    ])
  ] ++ [
    (i, [
        ("", True),
        ("a", False),
        ("b", False),
        ("ab", True),
        ("bab", False),
        ("abab", True),
        ("abba", False),
        ("aabb", True),
        ("ababa", False),
        ("ababab", True),
        ("abbaab", False),
        ("aabbab", True),
        ("aaabbb", True),
        ("aababbab", True),
        ("aabaababbb", True)
    ])
    | i <- [8..12]
  ] ++ [
    (i, [
        ("", True),
        ("a", False),
        ("aa", True),
        ("ab", False),
        ("aba", False),
        ("abab", False),
        ("abba", True),
        ("bbbb", True),
        ("aaba", False),
        ("aaaaa", False),
        ("babbab", True),
        ("aaaaaa", True)
    ])
    | i <- [13,14]
  ]

tests :: Test
tests = TestLabel "Correctness of constructing semigroup from machine" $
    TestList $ do
        (tmi, testsTM) <- testSet
        let (msg, allChars, Just tm) = testingSet !! (tmi - 1)
            SP rs = semigroupGamma tm
            q_0 = runTMReader (q_ 0) tm
            rules = flip filter (Set.toList rs) $
                \(gw1 `Equals` gw2) ->
                    gw1 /= gw2 && q_0 `notElem` gw1
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
                        return $ (~=?) answer $
                            any (q_0 `elem`) $
                                solve depth rules initWord
