{-# LANGUAGE OverloadedStrings #-}

module IntegrationParserBoolean2tmTests where

import Text.Megaparsec
import Test.HUnit

import qualified Data.Set as Set

import GrammarReader
import Boolean2TM
import Interpreter
import DebuggingTMTypes (convertToTuringMachine)
import TMTypes

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
            word = ["S","(","a","b",")"]
            tm = convertToTuringMachine $ boolean2tm gr
            xs = states $ startWithAlphabet' tm word 0 alphabet
            testCase = TestCase (assertEqual (concat word) (Q 0) (currentState $ last xs))
                    --else TestCase (assertEqual (concat word) (Q (-1)) (currentState $ last xs))
            in TestLabel "lolo" testCase
        

