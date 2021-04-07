module Boolean2TMTests where

import InterpreterInputData
import TestGenerationHelpers

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

