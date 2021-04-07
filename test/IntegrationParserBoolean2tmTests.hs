{-# LANGUAGE OverloadedStrings #-}

module IntegrationParserBoolean2tmTests where

import Text.Megaparsec
import Test.HUnit

import qualified Data.Set as Set

import GrammarReader
import InterpreterInputData
import TestGenerationHelpers

tests :: Test
tests = TestList [test1, test2, test3]

test1 :: Test
test1 = let
    input = "S; S B A; a b\nS-> A B\nB-> B B\nB-> b\nA-> a\nA-> b"
    alphabet = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4",
                    "5", "6", "A", "B", "S", "a", "b"]
    result =  parseMaybe GrammarReader.parser input
    in case result of
        Nothing -> TestLabel "Correct input format for parser" $ TestCase $ assertFailure "no parse"
        Just gr -> let
            numbers = [1..10]
            tests1 = map (\t -> let
                word = ["S","(","a"] ++ replicate t "b" ++ [")"]
                testData = IN gr alphabet "integration1.1"
                in generateTestLabel testData word True) numbers
            tests2 = map (\t -> let
                word = ["S","(","b"] ++ replicate t "b" ++ [")"]
                testData = IN gr alphabet "integration1.2"
                in generateTestLabel testData word True) numbers
            tests3 = map (\t -> let
                word = ["S", "(", "b"] ++ replicate t "b" ++ ["a",")"]
                testData = IN gr alphabet "integration1.3"
                in generateTestLabel testData word False) numbers
            allTests = tests1 ++ tests2 ++ tests3
            in TestLabel "Correctness of grammar parser and boolean2tm algorithm integration:" $ TestList allTests

test2 :: Test
test2 = let
    input = "S; S B A C; a b\nS-> A B&! B C&! A C\nB-> A B\nB-> A A\nA-> a\nC-> b"
    alphabet = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4",
                    "5", "6", "7", "A", "B", "C", "S", "a", "b"]
    result =  parseMaybe GrammarReader.parser input
    in case result of
        Nothing -> TestLabel "Correct input format for parser" $ TestCase $ assertFailure "no parse"
        Just gr -> let
            numbers = [1..10]
            tests1 = map (\t -> let
                word = ["S","(","a", "a"] ++ replicate t "a" ++ [")"]
                testData = IN gr alphabet "integration2.1"
                in generateTestLabel testData word True) numbers
            tests2 = map (\t -> let
                word = ["S","(","a"] ++ replicate t "a" ++ ["b", ")"]
                testData = IN gr alphabet "integration2.2"
                in generateTestLabel testData word False) numbers
            allTests = tests1 ++ tests2
            in TestLabel "Correctness of grammar parser and boolean2tm algorithm integration:" $ TestList allTests

test3 :: Test
test3 = let
    input = "S; S Ba Ad C; d b\nS-> Ad Ba& Ba C\nBa-> C Ba\nC-> C Ba\nAd-> b\nBa-> b\nC-> d"
    alphabet = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4",
                        "5", "6", "Ad", "Ba", "C", "S", "b", "d"]
    result =  parseMaybe GrammarReader.parser input
        in case result of
            Nothing -> TestLabel "Correct input format for parser" $ TestCase $ assertFailure "no parse"
            Just gr -> let
                numbers = [1..10]
                tests1 = map (\t -> let
                    word = ["S","(","b"] ++ replicate t "d" ++ ["b",")"]
                    testData = IN gr alphabet "integration3.1"
                    in generateTestLabel testData word True) numbers
                tests2 = map (\t -> let
                    word = ["S","("] ++ replicate t "d" ++ [")"]
                    testData = IN gr alphabet "integration3.2"
                    in generateTestLabel testData word False) numbers
                tests3 = map (\t -> let
                    word = ["S","("] ++ replicate t "b" ++ [")"]
                    testData = IN gr alphabet "integration3.3"
                    in generateTestLabel testData word False) numbers
                allTests = tests1 ++ tests2 ++ tests3
                in TestLabel "Correctness of grammar parser and boolean2tm algorithm integration:" $ TestList allTests