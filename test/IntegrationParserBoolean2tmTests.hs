{-# LANGUAGE OverloadedStrings #-}

module IntegrationParserBoolean2tmTests where

import Text.Megaparsec
import Test.HUnit

import qualified Data.Set as Set

import GrammarReader
import Boolean2TM
import Interpreter (states, currentState, startWithAlphabet')
import InterpreterInputData
import DebuggingTMTypes (convertToTuringMachine)
import TMTypes
import TestGenerationHelpers

tests :: Test
tests = TestList [testsAccepted]

testsAccepted :: Test
testsAccepted = let
    input = "S; S B A; a b\nS-> A B\nB-> B B\nB-> b\nA-> a"
    alphabet = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4",
                    "5", "6", "A", "B", "S", "a", "b"]
    result =  parseMaybe GrammarReader.parser input
    in case result of
        Nothing -> TestLabel "lala" $ TestCase $ assertFailure "no parse"
        Just gr -> let
            numbers = [1..10]
            allTests = map (\t -> let
                word = ["S","(","a"] ++ replicate t "b" ++ [")"]
                testData = IN gr alphabet "integration1"
                in generateTestLabel testData word True) numbers
            in TestLabel "Correctness of grammar parser and boolean2tm algorithm integration:" $ TestList allTests

