{-# LANGUAGE OverloadedStrings #-}

module IntegrationParserBoolean2tmTests where

import Text.Megaparsec
import Test.HUnit

import qualified Data.Set as Set
import Data.Text (pack)

import GrammarReader
import InterpreterInputData
import TestGenerationHelpers
import Boolean2TM

tests :: Test
tests = TestList [test1, test2, test3, test4, test5, test6]

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
            tm = boolean2tm gr
            tests1 = map (\t -> let
                word = "S(a" ++ replicate t 'b' ++ ")"
                testData = IN gr alphabet "integration1.1"
                in generateTestLabel tm testData word True) numbers
            tests2 = map (\t -> let
                word = "S(b" ++ replicate t 'b' ++ ")"
                testData = IN gr alphabet "integration1.2"
                in generateTestLabel tm testData word True) numbers
            tests3 = map (\t -> let
                word = "S(b" ++ replicate t 'b' ++ "a)"
                testData = IN gr alphabet "integration1.3"
                in generateTestLabel tm testData word False) numbers
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
            tm = boolean2tm gr
            tests1 = map (\t -> let
                word = "S(aa" ++ replicate t 'a' ++ ")"
                testData = IN gr alphabet "integration2.1"
                in generateTestLabel tm testData word True) numbers
            tests2 = map (\t -> let
                word = "S(a" ++ replicate t 'a' ++ "b)"
                testData = IN gr alphabet "integration2.2"
                in generateTestLabel tm testData word False) numbers
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
                tm = boolean2tm gr
                tests1 = map (\t -> let
                    word = "S(b" ++ replicate t 'd' ++ "b)"
                    testData = IN gr alphabet "integration3.1"
                    in generateTestLabel tm testData word True) numbers
                tests2 = map (\t -> let
                    word = "S(" ++ replicate t 'd' ++ ")"
                    testData = IN gr alphabet "integration3.2"
                    in generateTestLabel tm testData word False) numbers
                tests3 = map (\t -> let
                    word = "S(" ++ replicate t 'b' ++ ")"
                    testData = IN gr alphabet "integration3.3"
                    in generateTestLabel tm testData word False) numbers
                allTests = tests1 ++ tests2 ++ tests3
                in TestLabel "Correctness of grammar parser and boolean2tm algorithm integration:" $ TestList allTests

numbers' :: [Int]
numbers' = [2..4]

test4 :: Test
test4 = let
    input = pack $ "S; S A A1 B D C C1 B1 B2 D1; a b c\nS-> A B&! D C\n"
            ++ "A-> A1 A\nA-> a\nA1-> a\nB-> B1 B2\nB1-> b\nB2-> B C1\nC1-> c\nB-> B1 C1\n"
            ++ "C-> C1 C\nC-> c\nD-> A1 D1\nD1-> D B1\nD-> A1 B1\nB1-> b"
    alphabet = inAlphabet testData6
    result =  parseMaybe GrammarReader.parser input
        in case result of
            Nothing -> TestLabel "Correct input format for parser" $ TestCase $ assertFailure "no parse"
            Just gr -> let
                tm = boolean2tm gr
                assertion = assertEqual "a^mb^nc^n" gr $ inGrammar testData6
                checkGr = TestLabel "Check grammar parsed correctly" $ TestCase assertion
                tests1 = map (\t -> let
                    word = "S(a" ++ replicate t 'b' ++ replicate t 'c' ++ ")"
                    testData = IN gr alphabet "integration4.1 a^mb^nc^n (m /= n)"
                    in generateTestLabel tm testData word True) numbers'
                tests2 = map (\t -> let
                    word = "S(" ++ replicate t 'a' ++ replicate t 'b' ++ replicate t 'c' ++ ")"
                    testData = IN gr alphabet "integration4.2 a^mb^nc^n (m /= n)"
                    in generateTestLabel tm testData word False) numbers'
                allTests = checkGr : tests1 ++ tests2
                in TestLabel "Correctness of grammar parser and boolean2tm algorithm integration" $ TestList allTests

test5 :: Test
test5 = let
    input = pack $ "S; S A A1 B D C C1 B1 B2 D1; a b c\nS-> A B& D C\n"
            ++ "A-> A1 A\nA-> a\nA1-> a\nB-> B1 B2\nB1-> b\nB2-> B C1\nC1-> c\nB-> B1 C1\n"
            ++ "C-> C1 C\nC-> c\nD-> A1 D1\nD1-> D B1\nD-> A1 B1\nB1-> b"
    alphabet = inAlphabet testData7
    result =  parseMaybe GrammarReader.parser input
        in case result of
            Nothing -> TestLabel "Correct input format for parser" $ TestCase $ assertFailure "no parse"
            Just gr -> let
                tm = boolean2tm gr
                assertion = assertEqual "a^mb^nc^n" gr $ inGrammar testData7
                checkGr = TestLabel "Check grammar parsed correctly" $ TestCase assertion
                tests1 = map (\t -> let
                    word = "S(a" ++ replicate t 'b' ++ replicate t 'c' ++ ")"
                    testData = IN gr alphabet "integration5.1 a^nb^nc^n"
                    in generateTestLabel tm testData word False) numbers'
                tests2 = map (\t -> let
                    word = "S(" ++ replicate t 'a' ++ replicate t 'b' ++ replicate t 'c' ++ ")"
                    testData = IN gr alphabet "integration5.2 a^nb^nc^n"
                    in generateTestLabel tm testData word True) numbers'
                allTests = checkGr : tests1 ++ tests2
                in TestLabel "Correctness of grammar parser and boolean2tm algorithm integration" $ TestList allTests

test6 :: Test
test6 = let
    input = "S; S A1 B1 S1; a b\nS-> A1 S1\nS1-> S B1\nS-> A1 B1\nB1-> b\nA1-> a"
    alphabet = inAlphabet testData8
    result =  parseMaybe GrammarReader.parser input
        in case result of
        Nothing -> TestLabel "Correct input format for parser" $ TestCase $ assertFailure "no parse"
        Just gr -> let
            tm = boolean2tm gr
            assertion = assertEqual "a^mb^nc^n" gr $ inGrammar testData8
            checkGr = TestLabel "Check grammar parsed correctly" $ TestCase assertion
            tests1 = map (\t -> let
                word = "S(" ++ replicate t 'a' ++ replicate t 'b' ++ ")"
                testData = IN gr alphabet "integration6.1 a^nb^n"
                in generateTestLabel tm testData word True) numbers'
            tests2 = map (\t -> let
                word = "S(a" ++ replicate t 'a' ++ replicate t 'b' ++ ")"
                testData = IN gr alphabet "integration5.2 a^nb^n"
                in generateTestLabel tm testData word False) numbers'
            allTests = checkGr : tests1 ++ tests2
            in TestLabel "Correctness of grammar parser and boolean2tm algorithm integration" $ TestList allTests    
