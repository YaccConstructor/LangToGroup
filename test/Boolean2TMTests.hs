module Boolean2TMTests where

import InterpreterInputData
import TestGenerationHelpers
import Boolean2TM

import Test.HUnit
import TuringMachine (TuringMachine)

tests :: Test
tests = TestList [testsAccepted,  testsAccepted',  testsNotAccepted,  others]

tm1 :: Maybe TuringMachine
tm1 = boolean2tm $ inGrammar testData1
tm2 :: Maybe TuringMachine
tm2 = boolean2tm $ inGrammar testData2
tm3 :: Maybe TuringMachine
tm3 = boolean2tm $ inGrammar testData3
tm4 :: Maybe TuringMachine
tm4 = boolean2tm $ inGrammar testData4
tm5 :: Maybe TuringMachine
tm5 = boolean2tm $ inGrammar testData5

testsAccepted :: Test
testsAccepted = let
    numbers = [3..10]
    tests1 = map (\t -> let
        word = "S(" ++ replicate t 'b' ++ "a)" in
        generateTestLabel tm1 testData1 word True) numbers

    tests2 = concatMap (\t -> let
        word = "S(" ++ replicate t 'b' ++ "ab)" in
        [generateTestLabel tm1 testData1 word True,
        generateTestLabel tm2 testData2 word True]) numbers

    tests3 = map (\t -> let
        word = "S(" ++ replicate t 'a' ++ ")" in
        generateTestLabel tm3 testData3 word True) numbers
    allTests = tests1 ++ tests2 ++ tests3
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
        $ TestList allTests

testsAccepted' :: Test
testsAccepted' = let
    numbers = [1..7]
    tests1 = map (\t -> let
        word = "S(" ++ replicate t 'c' ++ replicate t 'b' ++ ")" in
        generateTestLabel tm4 testData4 word True) numbers

    tests2 = map (\t -> let
        word = "S(bc" ++ replicate t 'b' ++ ")" in
        generateTestLabel tm5 testData5 word True) numbers

    tests3 = map (\t -> let
        word = "S(c" ++ replicate t 'b' ++ ")" in
        generateTestLabel tm5 testData5 word True) numbers
    allTests = tests1 ++ tests2 ++ tests3
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
            $ TestList allTests

testsNotAccepted :: Test
testsNotAccepted = let
    numbers = [3..13]
    testsNotAccepted1 = concatMap (\t -> let
        word = "S(" ++ replicate t 'a' ++ ")" in
        [generateTestLabel tm1 testData1 word False,
        generateTestLabel tm2 testData2 word False]) numbers
    testsNotAccepted2 = concatMap (\t -> let
        word = "S(" ++ replicate t 'b' ++ ")" in
        [generateTestLabel tm1 testData1 word False,
        generateTestLabel tm2 testData2 word False]) numbers
    testsNotAccepted3 = concatMap (\t -> let
        word = "S(" ++ replicate t 'c' ++ ")" in
        [generateTestLabel tm4 testData4 word False,
        generateTestLabel tm5 testData5 word False]) numbers
    allTests = testsNotAccepted1 ++ testsNotAccepted2
        ++ testsNotAccepted3
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
                   $ TestList allTests

others :: Test
others = let
    allTests = TestList [
        generateTestLabel tm1 testData1 "S(a)" True,
        generateTestLabel tm1 testData1 "S(ab)" True,
        generateTestLabel tm1 testData1 "S(b)" False,
        generateTestLabel tm1 testData1 "S(bab)" True,
        generateTestLabel tm2 testData2 "S(bb)" True,
        generateTestLabel tm2 testData2 "S(bb)" True,
        generateTestLabel tm5 testData5 "S(b)" False,
        generateTestLabel tm5 testData5 "S(c)" False,
        generateTestLabel tm5 testData5 "S(bc)" True]
    in TestLabel "Other tests for checking correctness algorithm building TM by boolean grammar:" allTests

