{-# LANGUAGE LambdaCase #-}

module Boolean2TMTests where

import Boolean2TM
import Interpreter
import DebuggingTMTypes
import TMTypes
import InterpreterInputData

import qualified Data.Set as Set
import Data.List as List
import Test.HUnit.Base (assertEqual, assertBool)
import Test.HUnit

tests :: Test
tests = TestList [tests'', tests', testsNotAccepted]

tests'' :: Test
tests'' = let
    numbers = [3..10]
    tests1 = map (\t -> let
        word = ["S","("] ++ replicate t "b" ++ ["a", ")"]
        testCase = generateTestCase testData1 word True in
        TestLabel ("Test for gr1 " ++ concat word) testCase) numbers
    tests2 = concatMap (\t -> let
        word = ["S","("] ++ replicate t "b" ++ ["a", "b", ")"]
        testCase1 = generateTestCase testData1 word True
        testCase2 = generateTestCase testData2 word True in
        [TestLabel ("Test for gr1 " ++ concat word) testCase1,
        TestLabel ("Test for gr2 " ++ concat word) testCase2]) numbers
    tests4 = map (\t -> let
        word = ["S","("] ++ replicate t "a" ++ [")"]
        testCase = generateTestCase testData3 word True in
        TestLabel ("Test for gr3 " ++ concat word) testCase) numbers
    allTests = tests1 ++ tests2 ++ tests4
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
        $ TestList allTests

tests' :: Test
tests' = let
    numbers = [1..7]
    tests1 = map (\t -> let
        word = ["S","("] ++ replicate t "c" ++ replicate t "b" ++ [")"]
        testCase = generateTestCase testData4 word True in
        TestLabel ("Test for gr4 " ++ concat word) testCase) numbers
    tests2 = map (\t -> let
        word = ["S","(","b","c"] ++ replicate t "b" ++ [")"]
        testCase = generateTestCase testData5 word True in
        TestLabel ("Test for gr5 " ++ concat word) testCase) numbers
    tests3 = map (\t -> let
        word = ["S","(","c"] ++ replicate t "b" ++ [")"]
        testCase = generateTestCase testData5 word True in
        TestLabel ("Test for gr5 " ++ concat word) testCase) numbers
    tests4 = map (\t -> let
            word = ["S","(","c"] ++ replicate t "c" ++ [")"]
            testCase = generateTestCase testData5 word False in
            TestLabel ("Test for gr5 " ++ concat word) testCase) numbers
    allTests = tests1 ++ tests2 ++ tests3 ++ tests4
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
            $ TestList allTests

testsNotAccepted :: Test
testsNotAccepted = let
    numbers = [3..13]
    testsNotAccepted' = concatMap (\t -> let
        word = ["S","("] ++ replicate t "a" ++ [")"]
        testCase1 = generateTestCase testData1 word False
        testCase2 = generateTestCase testData2 word False in
        [TestLabel ("Test for gr1 " ++ concat word) testCase1,
        TestLabel ("Test for gr2 " ++ concat word) testCase2]) numbers
    testsNotAccepted'' = concatMap (\t -> let
        word = ["S","("] ++ replicate t "b" ++ [")"]
        testCase1 = generateTestCase testData1 word False
        testCase2 = generateTestCase testData2 word False in
        [TestLabel ("Test for gr1 " ++ concat word) testCase1,
        TestLabel ("Test for gr2 " ++ concat word) testCase2]) numbers
    testsNotAccepted''' = concatMap (\t -> let
        word = ["S","("] ++ replicate t "c" ++ [")"]
        testCase4 = generateTestCase testData4 word False
        testCase5 = generateTestCase testData5 word False in
        [TestLabel ("Test for gr4 " ++ concat word) testCase4,
        TestLabel ("Test for gr5 " ++ concat word) testCase5]) numbers
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
                   $ TestList $ testsNotAccepted' ++ testsNotAccepted'' ++ testsNotAccepted'''

generateTestCase :: InterpreterInputData -> [String] -> Bool -> Test
generateTestCase input word accepted = let
    tm = convertToTuringMachine $ boolean2tm $ inGrammar input
    xs = states $ startWithAlphabet' tm word 0 $ inAlphabet input
    description = "Check word " ++ concat word ++ " accepted"
    testCase = if accepted
        then TestCase (assertEqual (concat word) (Q 0) (currentState $ last xs))
        else TestCase (assertEqual (concat word) (Q (-1)) (currentState $ last xs)) in
    TestLabel description testCase

test11 :: IO ()
test11 = do
    let tm = convertToTuringMachine $ boolean2tm $ inGrammar testData1
    let states' = states $ startWithAlphabet' tm ["S","(","b","a","b",")"] 0 $ inAlphabet testData1
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states')

test12 :: IO ()
test12 = do
    let tm = convertToTuringMachine $ boolean2tm $ inGrammar testData1
    let alphabet' = inAlphabet testData1
    let states1 = states $ startWithAlphabet' tm ["S","(","a",")"] 0 alphabet'
    let states2 = states $ startWithAlphabet' tm ["S","(","a","b",")"] 0 alphabet'
    let states3 = states $ startWithAlphabet' tm ["S","(","b","b","b","a",")"] 0 alphabet'
    let states4 = states $ startWithAlphabet' tm ["S","(","b"] 0 alphabet'
    assertEqual "Check that word S(a) is accepted" (Q 0) (currentState $ last states1)
    assertEqual "Check that word S(ab) is accepted" (Q 0) (currentState $ last states2)
    assertEqual "Check that word S(bbba) is accepted" (Q 0) (currentState $ last states3)
    assertBool "Check that word S(b is accepted" (currentState (last states4) /= Q 0)

test22 :: IO()
test22 = do
    let tm = convertToTuringMachine $ boolean2tm $ inGrammar testData2
    let states' = states $ startWithAlphabet' tm ["S","(","b","b","a","b",")"] 0 $ inAlphabet testData2
    --printInfo (boolean2tm testGr5) tm alphabet5 ["S","(","b","c","b","b",")"]
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states')

-- print info
printInfo :: DebuggingTuringMachine -> TuringMachine -> Set.Set String -> [String] -> IO [()]
printInfo dtm tm inputAlphabet word = do
    let states' = states $ startWithAlphabet' tm word 0 inputAlphabet
    let states'' = getStates dtm
    let statesPairs = map (\t -> (elemIndex t states'', t)) states''
    let symbols = getSymbols dtm
    let symbolsPairs = map (\t -> (elemIndex t symbols, t)) symbols
    print symbolsPairs
    print statesPairs
    print tm
    mapM (\(WS _ currentState' tape' alphabet') -> let
        (Q index') = currentState';
        state = snd $ head $ filter (\case
            (Just index, DState _) -> index == index'
            (Nothing, _) -> False) statesPairs;
        (DState stringState) = state
        in do {
            print currentState';
            print stringState;
            print tape';
            print alphabet';}) states'

