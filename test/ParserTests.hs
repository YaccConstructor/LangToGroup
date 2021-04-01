{-# LANGUAGE OverloadedStrings #-}

module ParserTests where

import Text.Megaparsec
import Test.HUnit

import qualified Data.Set as Set

import GrammarReader
import GrammarType

{--testCustomCFG :: Assertion
testCustomCFG = do
    let input = "S; S B; a b\nS-> a B\nB-> b"
    let expectedGr = Grammar (
                                Set.fromList [Nonterminal "B", Nonterminal "S"], Set.fromList [Terminal "a", Terminal "b"],
                                Set.fromList [
                                                Relation (Nonterminal "B",[T (Terminal "b")]),
                                                Relation (Nonterminal "S",[T (Terminal "a"),N (Nonterminal "B")])
                                             ],
                                Nonterminal "S")
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> assertFailure "Error occured during parsing custom grammar."
        Just grammar -> assertEqual "assert grammar representation" expectedGr grammar

testCustomConjunctive :: Assertion
testCustomConjunctive = do
    let input = "S; S Abc D Cr; c b d e\nS-> D c& d Abc\nAbc-> b\nD-> Cr\nCr-> e"
    let expectedGr = Grammar (
                                Set.fromList [Nonterminal "Abc",Nonterminal "Cr",Nonterminal "D",Nonterminal "S"],
                                Set.fromList [Terminal "b",Terminal "c",Terminal "d",Terminal "e"],
                                Set.fromList [
                                                Relation (Nonterminal "Abc",[T (Terminal "b")]),
                                                Relation (Nonterminal "Cr",[T (Terminal "e")]),
                                                Relation (Nonterminal "D",[N (Nonterminal "Cr")]),
                                                Relation (Nonterminal "S",[N (Nonterminal "D"),T (Terminal "c"),O Conjunction,T (Terminal "d"),N (Nonterminal "Abc")])
                                             ],
                                Nonterminal "S")
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> assertFailure "Error occured during parsing custom grammar."
        Just grammar -> assertEqual "assert grammar representation" expectedGr grammar

testCustomBoolean :: Assertion
testCustomBoolean = do
    let input = "S; S Sa; c v b\nS-> c&! v&! Sa&! Eps\nSa->! b\nS-> a& b&! v&! Sa&! Eps"
    let expectedGr = Grammar (
                                Set.fromList [Nonterminal "S",Nonterminal "Sa"],
                                Set.fromList [Terminal "b",Terminal "c",Terminal "v"],
                                Set.fromList [
                                                Relation (Nonterminal "S",[T (Terminal "a"),O Conjunction,T (Terminal "b"),O Conjunction,O Negation,T (Terminal "v"),O Conjunction,O Negation,N (Nonterminal "Sa"),O Conjunction,O Negation,N (Nonterminal "Eps")]),
                                                Relation (Nonterminal "S",[T (Terminal "c"),O Conjunction,O Negation,T (Terminal "v"),O Conjunction,O Negation,N (Nonterminal "Sa"),O Conjunction,O Negation,N (Nonterminal "Eps")]),
                                                Relation (Nonterminal "Sa",[O Negation,T (Terminal "b")])
                                              ],
                                Nonterminal "S")
    let result =  parseMaybe GrammarReader.parser input
    case result of
        Nothing -> assertFailure "Error occured during parsing custom grammar."
        Just grammar -> assertEqual "assert grammar representation" expectedGr grammar--}