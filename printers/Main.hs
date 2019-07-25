{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.LaTeX.Base.Render
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Inputenc
import qualified Data.Set as Set

import GrammarPrinter
import Tm1Printer
import Lib
import GrammarType
import CfgToTMMapper 
import TMType
import TMInterpreter
import ConfigPrinter
import TM2TM'
import TM2SM
import SMPrinter

preambula :: LaTeXM ()
preambula = 
    documentclass [] article
    <> usepackage [utf8] inputenc
    <> usepackage [] "unicode-math"
    <> usepackage [] "amsmath"
    <> usepackage [] "mathtools"
    <> usepackage ["left=1cm","right=3cm", "top=2cm", "bottom=2cm", "bindingoffset=0cm"] "geometry"
    <> title "Examples"

example :: LaTeX
example = execLaTeXM $ 
    do
        preambula 
        document $ do
            -- doLaTeX testGrammar
            -- doLaTeX $ mapCfgToTM testGrammar
            -- doLaTeX $ interpretTM ["a"] $ mapCfgToTM testGrammar
            -- newpage
            -- doLaTeX $ mapTM2TM' $ mapCfgToTM testGrammar
            -- newpage
            -- doLaTeX epsTestGrammar
            -- doLaTeX $ mapCfgToTM epsTestGrammar
            -- doLaTeX $ interpretTM ["a"] $ mapCfgToTM epsTestGrammar
            -- newpage
            -- doLaTeX epsTestLeftGrammar
            -- doLaTeX $ mapCfgToTM epsTestLeftGrammar
            -- doLaTeX $ interpretTM ["a"] $ mapCfgToTM epsTestLeftGrammar
            -- newpage
            -- doLaTeX abTestGrammar
            -- doLaTeX $ mapCfgToTM abTestGrammar
            -- doLaTeX $ interpretTM ["b", "b", "a", "a"] $ mapCfgToTM abTestGrammar
            -- newpage
            -- doLaTeX ab2TestGrammar
            -- doLaTeX $ mapCfgToTM ab2TestGrammar
            -- doLaTeX $ interpretTM ["b", "a", "b", "a"] $ mapCfgToTM ab2TestGrammar
            -- newpage
            -- doLaTeX ab3TestGrammar
            -- doLaTeX $ mapCfgToTM ab3TestGrammar
            -- doLaTeX $ interpretTM ["b", "a", "b", "a"] $ mapCfgToTM ab3TestGrammar
            doLaTeX $ smFinal tmForTestSm

main :: IO()
main = do
    renderFile "out.tex" example 

tmForTestSm = tm where
    s = Value "S"
    q0 = StateOmega $ State "q_0"
    q0' = StateOmega $ State "q_0'"
    q1 = StateOmega $ State "q_1"
    q1' = StateOmega $ State "q_1'"
    inp = InputAlphabet (Set.fromList [])
    tapes = [TapeAlphabet (Set.fromList [s]), TapeAlphabet (Set.fromList [])]
    states = MultiTapeStates []
    cmds = Commands $ Set.fromList [[PreSMCommand ((s, q0), (emptySymbol, q0')), PreSMCommand ((emptySymbol, q1), (emptySymbol, q1'))]]
    start = StartStates []
    access = AccessStates []
    tm = TM (inp, tapes, states, cmds, start, access)    

-- Example from test
testGrammar = grammar where
    terminal = Terminal "a"
    nonterminal = Nonterminal "S"
    eps = Epsilon "ε"
    grammar =
        Grammar(
            (Set.fromList [nonterminal]),
            (Set.fromList [terminal]),
            (Set.fromList [GrammarType.Relation (nonterminal, [GrammarType.T terminal])]),
            nonterminal,
            eps
        )

epsTestGrammar = grammar where
    terminal = Terminal "a"
    start = Nonterminal "S"
    nonterminal = Nonterminal "A"
    eps = Epsilon "ε"
    grammar =
        Grammar(
            (Set.fromList [nonterminal, start]),
            (Set.fromList [terminal]),
            (Set.fromList [
                GrammarType.Relation (start, [GrammarType.N nonterminal, GrammarType.N start]),
                GrammarType.Relation (start, [GrammarType.E eps]),
                GrammarType.Relation (nonterminal, [GrammarType.T terminal])
                ]),
            start,
            eps
        )

epsTestLeftGrammar = grammar where
    terminal = Terminal "a"
    start = Nonterminal "S"
    nonterminal = Nonterminal "A"
    eps = Epsilon "ε"
    grammar =
        Grammar(
            (Set.fromList [nonterminal, start]),
            (Set.fromList [terminal]),
            (Set.fromList [
                GrammarType.Relation (start, [GrammarType.N start, GrammarType.N nonterminal]),
                GrammarType.Relation (start, [GrammarType.E eps]),
                GrammarType.Relation (nonterminal, [GrammarType.T terminal])
                ]),
            start,
            eps
        )

abTestGrammar = grammar where
    a = Terminal "a"
    b = Terminal "b"
    s = Nonterminal "S"
    s1 = Nonterminal "C"
    aN = Nonterminal "A"
    bN = Nonterminal "B"
    eps = Epsilon "ε"
    grammar =
        Grammar(
            (Set.fromList [s, s1, aN, bN]),
            (Set.fromList [a, b]),
            (Set.fromList [
                GrammarType.Relation (s, [GrammarType.N aN, GrammarType.N s1]),
                GrammarType.Relation (s, [GrammarType.E eps]),
                GrammarType.Relation (s1, [GrammarType.N s, GrammarType.N bN]),
                GrammarType.Relation (aN, [GrammarType.T a]),
                GrammarType.Relation (bN, [GrammarType.T b])
                ]),
            s,
            eps
        )

ab2TestGrammar = grammar where
    a = Terminal "a"
    b = Terminal "b"
    s = Nonterminal "S"
    s1 = Nonterminal "C"
    aN = Nonterminal "A"
    bN = Nonterminal "B"
    b1 = Nonterminal "D"
    eps = Epsilon "ε"
    grammar =
        Grammar(
            (Set.fromList [s, s1, aN, bN]),
            (Set.fromList [a, b]),
            (Set.fromList [
                GrammarType.Relation (s, [GrammarType.N aN, GrammarType.N s1]),
                GrammarType.Relation (s, [GrammarType.E eps]),
                GrammarType.Relation (s1, [GrammarType.N s, GrammarType.N b1]),
                GrammarType.Relation (aN, [GrammarType.T a]),
                GrammarType.Relation (bN, [GrammarType.T b]),
                GrammarType.Relation (b1, [GrammarType.N bN, GrammarType.N s])
                ]),
            s,
            eps
        )

ab3TestGrammar = grammar where
    a = Terminal "a"
    b = Terminal "b"
    s0 = Nonterminal "S_0"
    s1 = Nonterminal "S_1"
    s = Nonterminal "S"
    c = Nonterminal "C"
    aN = Nonterminal "A"
    bN = Nonterminal "B"
    eps = Epsilon "ε"
    grammar =
        Grammar(
            (Set.fromList [s0, c, s, s1, aN, bN]),
            (Set.fromList [a, b]),
            (Set.fromList [
                GrammarType.Relation (s0, [GrammarType.N s, GrammarType.N s1]),
                GrammarType.Relation (s0, [GrammarType.N aN, GrammarType.N c]),
                GrammarType.Relation (s0, [GrammarType.E eps]),
                GrammarType.Relation (s1, [GrammarType.N aN, GrammarType.N c]),
                GrammarType.Relation (s1, [GrammarType.N s, GrammarType.N s1]),
                GrammarType.Relation (s, [GrammarType.N aN, GrammarType.N c]),
                GrammarType.Relation (s, [GrammarType.N s, GrammarType.N s1]),
                GrammarType.Relation (c, [GrammarType.N bN]),
                GrammarType.Relation (c, [GrammarType.N s, GrammarType.N bN]),
                GrammarType.Relation (aN, [GrammarType.T a]),
                GrammarType.Relation (bN, [GrammarType.T b])
                ]),
            s0,
            eps
        )