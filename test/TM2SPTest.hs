{-# LANGUAGE TupleSections #-}

module TM2SPTest (
    TM2SPTest.test,
  ) where

import TuringMachine.TMs
import TuringMachine.Admiter
import TM2SP
import SemigroupPresentation.Solver

import Test.HUnit
import Data.Maybe (mapMaybe, isJust, isNothing)

addValuesFromMapByTitle :: Map Title b -> [WithTitle a] -> [(WithTitle a, b)]
addValuesFromMapByTitle m = mapMaybe $
    \wta -> do
        let t = title wta
        b <- m !? t
        return (wta, b)

testingSet :: [(WithTitle TuringMachine, [(String, Bool)])]
testingSet = flip addValuesFromMapByTitle testingSetTMs $ fromList $ [
    ("a*", [
        ("", True),
        ("a", True),
        ("b", False),
        ("aa", True),
        ("aaa", True),
        ("aaaa", True),
        ("aaaaa", True),
        ("aaaaaa", True),
        ("aaaaaaa", True),
        ("aaaaaaaa", True)
      ]),
    ("a+", [
        ("", False),
        ("a", True),
        ("b", False),
        ("aa", True),
        ("aaa", True),
        ("aaaa", True),
        ("aaaaa", True),
        ("aaaaaa", True),
        ("aaaaaaa", True),
        ("aaaaaaaa", True)
      ]),
    ("a?", [
        ("", True),
        ("a", True),
        ("b", False),
        ("aa", False),
        ("aaa", False)
      ]),
    ("a|b", [
        ("", False),
        ("a", True),
        ("b", True),
        ("c", False),
        ("ab", False),
        ("ba", False)
      ]),
    ("abc", [
        ("", False),
        ("ab", False),
        ("aaa", False),
        ("abc", True),
        ("abca", False),
        ("abcc", False),
        ("abcabc", False)
      ]),
    ("ababa", [
        ("", False),
        ("aba", False),
        ("ababa", True),
        ("abababa", False),
        ("ababaababa", False)
      ])
  ] ++
    ((, [
        ("", False),
        ("a", False),
        ("b", False),
        ("c", False),
        ("d", False),
        ("aa", False),
        ("aba", True),
        ("abc", False),
        ("ababa", False),
        ("abcba", True),
        ("cbaac", False),
        ("aaaaaa", False),
        ("abcbcba", True),
        ("abcbaba", False)
      ]) <$> [ "a(bc)*ba v." <+ v | v <- "12" ]) ++
    ((, [
        ("", True),
        ("(", False),
        (")", False),
        ("()", True),
        (")(", False),
        ("(()", False),
        ("()(", False),
        ("()()", True),
        ("())(", False),
        ("(())", True),
        ("(())()", True),
        ("()()()", True),
        ("()(())", True),
        ("((()))", True),
        ("(()(()()))", True)
      ]) <$> [ "Dyck v." <+ v | v <- "123" ]) ++
    ((, [
        ("", True),
        ("a", False),
        ("aa", True),
        ("ab", False),
        ("bb", True),
        ("cc", False),
        ("abab", False),
        ("aabb", False),
        ("abba", True),
        ("aaaaa", False),
        ("aabaaa", False),
        ("aabbaa", True),
        ("bbaabb", True),
        ("abaaba", True),
        ("ababba", False),
        ("ababbbbaba", True),
        ("babbaabbab", True),
        ("baabaabbab", False)
      ]) <$> [ "wwR 2s v." <+ v | v <- "12" ]) ++
    ((, [
        ("", True),
        ("a", False),
        ("d", False),
        ("aa", True),
        ("ac", False),
        ("bb", True),
        ("cc", True),
        ("abab", False),
        ("aacc", False),
        ("abba", True),
        ("bddb", True),
        ("dadd", False),
        ("dddd", True),
        ("ccccd", False),
        ("ddadd", False),
        ("aabada", False),
        ("bbccbb", True),
        ("bdaadb", True),
        ("abccba", True),
        ("abcabc", False),
        ("abadda", False),
        ("acddddddca", True),
        ("cadbaabdac", True),
        ("dadbccbaad", False)
      ]) <$> [ "wwR 4s v." <+ v | v <- "12" ])

test :: Test
test = TestLabel "Correctness of constructing semigroup from machine" $
    TestList $ flip map testingSet $ \(testingTM, testingWords) ->
        TestLabel (title testingTM) $
            TestList $ flip map testingWords $ \(word, ans) ->
                TestLabel ("\"" ++ word ++ "\" " ++ if ans then "V" else "X") $
                    let tm = withoutTitle testingTM
                    in  TestList [
                            TestLabel "Turing Machine" $
                                TestCase $ assert $
                                    asAns ans $
                                        admit tm (Just 4242) (word, 0),
                            TestLabel "Original Semigroup Presentation" $
                                TestCase $ assert $
                                    asAns ans $ do
                                        sp <- semigroupGamma tm
                                        input <- convertInput tm (word, 0)
                                        let output = "q"
                                        solve output sp (Just 4242) input,
                            TestLabel "New Semigroup Presentation Version 1" $
                                TestCase $ assert $
                                    asAns ans $ do
                                        SP1 sp <- semigroupGamma_1 tm
                                        input <- convertInput tm (word, 0)
                                        let output = "h q_0 h"
                                        solve output sp (Just 4242) input,
                            TestLabel "New Semigroup Presentation Version 2" $
                                TestCase $ assert $
                                    asAns ans $ do
                                        SP1 sp <- semigroupGamma_2 tm
                                        input <- convertInput tm (word, 0)
                                        let output = "h q_0 h"
                                        solve output sp (Just 4242) input
                          ]
      where
        asAns :: Bool -> Maybe a -> Bool
        asAns True = isJust
        asAns False = isNothing
