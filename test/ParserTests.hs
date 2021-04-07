{-# LANGUAGE OverloadedStrings #-}

module ParserTests where

import Text.Megaparsec
import Test.HUnit

import qualified Data.Set as Set

import GrammarReader
import GrammarType

testCustomCFG :: Assertion
testCustomCFG = do
    let input = "S; S B; a b\nS-> a B\nB-> b"
    let expectedGr = expectedCFG
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> assertFailure "Error occured during parsing custom grammar."
        Just grammar -> assertEqual "assert grammar representation" expectedGr grammar

testCustomConjunctive :: Assertion
testCustomConjunctive = do
    let input = "S; S Abc D Cr; c b d e\nS-> D c& d Abc\nAbc-> b\nD-> Cr\nCr-> e"
    let expectedGr = expectedConjunctive
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> assertFailure "Error occured during parsing custom grammar."
        Just grammar -> assertEqual "assert grammar representation" expectedGr grammar

testCustomBoolean :: Assertion
testCustomBoolean = do
    let input = "S; S Sa; c v b\nS-> c&! v&! Sa\nSa->! b\nS-> a& b&! v&! Sa"
    let expectedGr = expectedBoolean 
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> assertFailure "Error occured during parsing custom grammar."
        Just grammar -> assertEqual "assert grammar representation" expectedGr grammar
        
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