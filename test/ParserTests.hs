{-# LANGUAGE OverloadedStrings #-}

module ParserTests (tests) where

import Text.Megaparsec
import Test.HUnit

import qualified Data.Set as Set

import GrammarReader
import GrammarType

tests :: Test
tests = TestLabel "Parser tests" $ TestList [testCustomCFG1, testCustomCFG2, testCustomCFG3,
    testCustomBoolean1, testCustomBoolean2, testCustomBoolean3,
    testCustomConjunctive1, testCustomConjunctive2, testCustomConjunctive3]

testCustomCFG1 :: Test
testCustomCFG1 = do
    let input = "S; S; b\nS-> S S\nS-> b"
    let expectedGr = Grammar (
            Set.fromList [Nonterminal "S"], Set.fromList [Terminal "b"],
            Set.fromList [Relation (Nonterminal "S", [N $ Nonterminal "S", N $ Nonterminal "S"]),
                         Relation (Nonterminal "S", [T $ Terminal "b"])],
                         Nonterminal "S")
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> TestCase $ assertFailure  "Error occured during parsing custom CFG grammar1."
        Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

testCustomCFG2 :: Test
testCustomCFG2 = do
    let input = "S; S B; a b\nS-> a B\nB-> b"
    let expectedGr = expectedCFG
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> TestCase $ assertFailure "Error occured during parsing custom CFG grammar2."
        Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

testCustomCFG3 :: Test
testCustomCFG3 = do
    let input = "S; S A; a b\nS-> a A S\nS-> a\nA-> S b A\nA-> b a\nA-> S S"
    let expectedGr = Grammar (
            Set.fromList [Nonterminal "S", Nonterminal "A"], Set.fromList [Terminal "b", Terminal "a"],
            Set.fromList [Relation (Nonterminal "S", [T $ Terminal "a", N $ Nonterminal "A", N $ Nonterminal "S"]),
                         Relation (Nonterminal "S", [T $ Terminal "a"]),
                         Relation (Nonterminal "A", [N $ Nonterminal "S", T $ Terminal "b", N $ Nonterminal "A"]),
                         Relation (Nonterminal "A", [T $ Terminal "b", T $ Terminal "a"]),
                         Relation (Nonterminal "A", [N $ Nonterminal "S", N $ Nonterminal "S"])],
                         Nonterminal "S")
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> TestCase $ assertFailure "Error occured during parsing custom CFG grammar3."
        Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

testCustomConjunctive1 :: Test
testCustomConjunctive1 = do
    let input = "S; S F D; a o kdf dff\nS-> D& F kdf F\nS-> o D D\nD-> a\nF-> dff"
    let expectedGr = Grammar (
            Set.fromList [Nonterminal "S", Nonterminal "F", Nonterminal "D"],
            Set.fromList [Terminal "a", Terminal "o", Terminal "kdf", Terminal "dff"],
            Set.fromList [BooleanRelation (Nonterminal "S",
            [PosConj [N $ Nonterminal "D"],
            PosConj [N $ Nonterminal "F", T $ Terminal "kdf", N $ Nonterminal "F"]]),
            Relation (Nonterminal "S",
            [T $ Terminal "o", N $ Nonterminal "D", N $ Nonterminal "D"]),
            Relation (Nonterminal "D", [T $ Terminal "a"]),
            Relation (Nonterminal "F", [T $ Terminal "dff"])],
            Nonterminal "S")
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> TestCase $ assertFailure "Error occured during parsing custom conjunctive grammar1."
        Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

testCustomConjunctive2 :: Test
testCustomConjunctive2 = do
    let input = "S; S Abc D Cr; c b d e\nS-> D c& d Abc\nAbc-> b\nD-> Cr\nCr-> e"
    let expectedGr = expectedConjunctive
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> TestCase $ assertFailure "Error occured during parsing custom conjunctive grammar2."
        Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

testCustomConjunctive3 :: Test
testCustomConjunctive3 = do
    let input = "S; S D B; c b\nS-> D D& B B\nD-> c\nB-> b"
    let expectedGr = Grammar (
            Set.fromList [Nonterminal "S", Nonterminal "D", Nonterminal "B"],
            Set.fromList [Terminal "c", Terminal "b"],
            Set.fromList [BooleanRelation (Nonterminal "S",
                [PosConj [N $ Nonterminal "D", N $ Nonterminal "D"],
                PosConj [N $ Nonterminal "B", N $ Nonterminal "B"]]),
                Relation (Nonterminal "D", [T $ Terminal "c"]),
                Relation (Nonterminal "B", [T $ Terminal "b"])],
            Nonterminal "S")
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> TestCase $ assertFailure "Error occured during parsing custom conjunctive grammar3."
        Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

testCustomBoolean1 :: Test
testCustomBoolean1 = do
    let input = "S; S Sa; c v b\nS-> c&! v&! Sa\nSa->! b\nS-> a& b&! v&! Sa"
    let expectedGr = expectedBoolean 
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> TestCase $ assertFailure "Error occured during parsing custom boolean grammar1."
        Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

testCustomBoolean2 :: Test
testCustomBoolean2 = do
    let input = "St; St D E B; e b d\nSt-> D E& B St\nSt->! D D&! B B\nSt->! B D\nD-> E D\nE-> e\nB-> B D\nB-> b\nD-> d"
    let expectedGr = Grammar (
            Set.fromList [Nonterminal "St", Nonterminal "D", Nonterminal "E", Nonterminal "B"],
            Set.fromList [Terminal "e", Terminal "b", Terminal "d"],
            Set.fromList [BooleanRelation (Nonterminal "St",
                [PosConj [N $ Nonterminal "D", N $ Nonterminal "E"],
                PosConj [N $ Nonterminal "B", N $ Nonterminal "St"]]),
                BooleanRelation (Nonterminal "St",
                [NegConj [N $ Nonterminal "D", N $ Nonterminal "D"],
                NegConj [N $ Nonterminal "B", N $ Nonterminal "B"]]),
                BooleanRelation (Nonterminal "St",
                [NegConj [N $ Nonterminal "B", N $ Nonterminal "D"]]),
                Relation (Nonterminal "D",
                [N $ Nonterminal "E", N $ Nonterminal "D"]),
                Relation (Nonterminal "E", [T $ Terminal "e"]),
                Relation (Nonterminal "B", [T $ Terminal "b"]),
                Relation (Nonterminal "D", [T $ Terminal "d"]),
                Relation (Nonterminal "B", [N $ Nonterminal "B", N $ Nonterminal "D"])],
                Nonterminal "St")
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> TestCase $ assertFailure "Error occured during parsing custom boolean grammar2."
        Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

testCustomBoolean3 :: Test
testCustomBoolean3 = do
    let input = "B; S B; a b\nS-> B S&! B B\nB-> b\nS-> a"
    let expectedGr = Grammar (
            Set.fromList [Nonterminal "S", Nonterminal "B"],
            Set.fromList [Terminal "a", Terminal "b"],
            Set.fromList [BooleanRelation (Nonterminal "S",
            [PosConj [N $ Nonterminal "B", N $ Nonterminal "S"], NegConj [N $ Nonterminal "B", N $ Nonterminal "B"]]),
            Relation (Nonterminal "B", [T $ Terminal "b"]),
            Relation (Nonterminal "S", [T $ Terminal "a"])],
            Nonterminal "B")
    let result =  parseMaybe GrammarReader.parser input
    case result of
            Nothing -> TestCase $ assertFailure "Error occured during parsing custom boolean grammar3."
            Just grammar -> TestCase $ assertEqual "assert grammar representation" expectedGr grammar

expectedCFG :: Grammar
expectedCFG = Grammar (
        Set.fromList [Nonterminal "B", Nonterminal "S"], Set.fromList [Terminal "a", Terminal "b"],
        Set.fromList [
            Relation (Nonterminal "B",[T (Terminal "b")]),
            Relation (Nonterminal "S",[T (Terminal "a"),N (Nonterminal "B")])
            ],
        Nonterminal "S")
        
expectedConjunctive :: Grammar
expectedConjunctive = Grammar (
        Set.fromList [Nonterminal "Abc",Nonterminal "Cr",Nonterminal "D",Nonterminal "S"],
        Set.fromList [Terminal "b",Terminal "c",Terminal "d",Terminal "e"],
        Set.fromList [
            Relation (Nonterminal "Abc",[T (Terminal "b")]),
            Relation (Nonterminal "Cr",[T (Terminal "e")]),
            Relation (Nonterminal "D",[N (Nonterminal "Cr")]),
            BooleanRelation (Nonterminal "S",
            [PosConj [N (Nonterminal "D"),T (Terminal "c")],
            PosConj [T (Terminal "d"),N (Nonterminal "Abc")]])
            ],
        Nonterminal "S")             

expectedBoolean :: Grammar
expectedBoolean = Grammar (
        Set.fromList [Nonterminal "S",Nonterminal "Sa"],
        Set.fromList [Terminal "b",Terminal "c",Terminal "v"],
        Set.fromList [
            BooleanRelation (Nonterminal "S",
            [PosConj [T (Terminal "a")],
            PosConj [T (Terminal "b")],
            NegConj [T (Terminal "v")],
            NegConj [N (Nonterminal "Sa")]]),
            BooleanRelation (Nonterminal "S",
            [PosConj [T (Terminal "c")],
            NegConj [T (Terminal "v")],
            NegConj [N (Nonterminal "Sa")]]),
            BooleanRelation (Nonterminal "Sa",[NegConj [T (Terminal "b")]])
            ],
        Nonterminal "S")           