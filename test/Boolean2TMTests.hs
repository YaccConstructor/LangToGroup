{-# LANGUAGE LambdaCase #-}

module Boolean2TMTests where

import Boolean2TM
import Interpreter
import DebuggingTMTypes
import TMTypes
import InterpreterInputData

import qualified Data.Set as Set
import Data.List as List
import Test.HUnit

tests :: Test
tests = TestList [testsAccepted,  testsAccepted',  testsNotAccepted,  others]

testsAccepted :: Test
testsAccepted = let
    numbers = [3..10]
    tests1 = map (\t -> let
        word = ["S", "("] ++ replicate t "b" ++ ["a", ")"] in
        generateTestLabel testData1 word True) numbers
    tests2 = concatMap (\t -> let
        word = ["S", "("] ++ replicate t "b" ++ ["a", "b", ")"] in
        [generateTestLabel testData1 word True, 
        generateTestLabel testData2 word True]) numbers
    tests4 = map (\t -> let
        word = ["S", "("] ++ replicate t "a" ++ [")"] in
        generateTestLabel testData3 word True) numbers
    allTests = tests1 ++ tests2 ++ tests4
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
        $ TestList allTests

testsAccepted' :: Test
testsAccepted' = let
    numbers = [1..7]
    tests1 = map (\t -> let
        word = ["S", "("] ++ replicate t "c" ++ replicate t "b" ++ [")"] in
        generateTestLabel testData4 word True) numbers
    tests2 = map (\t -> let
        word = ["S", "(", "b", "c"] ++ replicate t "b" ++ [")"] in
        generateTestLabel testData5 word True) numbers
    tests3 = map (\t -> let
        word = ["S", "(", "c"] ++ replicate t "b" ++ [")"] in
        generateTestLabel testData5 word True) numbers
    allTests = tests1 ++ tests2 ++ tests3
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
            $ TestList allTests

testsNotAccepted :: Test
testsNotAccepted = let
    numbers = [3..13]
    testsNotAccepted1 = concatMap (\t -> let
        word = ["S", "("] ++ replicate t "a" ++ [")"] in
        [generateTestLabel testData1 word False, 
        generateTestLabel testData2 word False]) numbers
    testsNotAccepted2 = concatMap (\t -> let
        word = ["S", "("] ++ replicate t "b" ++ [")"] in
        [generateTestLabel testData1 word False, 
        generateTestLabel testData2 word False]) numbers
    testsNotAccepted3 = concatMap (\t -> let
        word = ["S", "("] ++ replicate t "c" ++ [")"] in
        [generateTestLabel testData4 word False, 
        generateTestLabel testData5 word False]) numbers
    allTests = testsNotAccepted1 ++ testsNotAccepted2
        ++ testsNotAccepted3
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
                   $ TestList allTests

others :: Test
others = let
    allTests = TestList [
        generateTestLabel testData1 ["S", "(", "a", ")"] True, 
        generateTestLabel testData1 ["S", "(", "a", "b", ")"] True, 
        generateTestLabel testData1 ["S", "(", "b", ")"] False, 
        generateTestLabel testData1 ["S", "(", "b", "a", "b", ")"] True, 
        generateTestLabel testData2 ["S", "(", "b", "b", ")"] True, 
        generateTestLabel testData2 ["S", "(", "b", "b", ")"] True, 
        generateTestLabel testData5 ["S", "(", "b", ")"] False, 
        generateTestLabel testData5 ["S", "(", "c", ")"] False, 
        generateTestLabel testData5 ["S", "(", "b", "c", ")"] True]
    in TestLabel "Other tests for checking correctness algorithm building TM by boolean grammar:" allTests

generateTestLabel :: InterpreterInputData -> [String] -> Bool -> Test
generateTestLabel input word accepted = let
    testCase = generateTestCase input word accepted in
    TestLabel ("Test for grammar " ++ inId input) testCase

generateTestCase :: InterpreterInputData -> [String] -> Bool -> Test
generateTestCase input word accepted = let
    tm = convertToTuringMachine $ boolean2tm $ inGrammar input
    xs = states $ startWithAlphabet' tm word 0 $ inAlphabet input
    testCase = if accepted
        then TestCase (assertEqual (concat word) (Q 0) (currentState $ last xs))
        else TestCase (assertEqual (concat word) (Q (-1)) (currentState $ last xs))
    description = if accepted
        then "check word " ++ concat word ++ " accepted"
        else "check word " ++ concat word ++ " not accepted"
    in TestLabel description testCase

-- print info
printInfo :: DebuggingTuringMachine -> TuringMachine -> Set.Set String -> [String] -> IO [()]
printInfo dtm tm inputAlphabet word = do
    let states' = states $ startWithAlphabet' tm word 0 inputAlphabet
    let states'' = getStates dtm
    let statesPairs = map (\t -> (elemIndex t states'',  t)) states''
    let symbols = getSymbols dtm
    let symbolsPairs = map (\t -> (elemIndex t symbols,  t)) symbols
    print symbolsPairs
    print statesPairs
    print tm
    mapM (\(WS _ currentState' tape' alphabet') -> let
        (Q index') = currentState';
        state = snd $ head $ filter (\case
            (Just index,  DState _) -> index == index'
            (Nothing,  _) -> False) statesPairs;
        (DState stringState) = state
        in do {
            print currentState';
            print stringState;
            print tape';
            print alphabet';}) states'

