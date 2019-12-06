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
import SMachineToGroup
import qualified SMType
import GRType
import GapFuncWriter
import System.IO
import qualified Data.Map.Strict as Map

preambula :: LaTeXM ()
preambula = 
    documentclass [] article
    <> usepackage [Text.LaTeX.Packages.Inputenc.utf8] inputenc
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
            --doLaTeX testGrammar
            --doLaTeX $ mapCfgToTM testGrammar
            --doLaTeX $ interpretTM ["a"] $ mapCfgToTM testGrammar
            --newpage
            --doLaTeX $ mapTM2TM' $ mapCfgToTM testGrammar
            -- newpage
            --doLaTeX $ smFinal $ mapTM2TM' $ mapCfgToTM testGrammar
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
            --doLaTeX $ smFinal $ mapTM2TM' $ mapCfgToTM ab2TestGrammar
            -- newpage
            -- doLaTeX ab3TestGrammar
            -- doLaTeX $ mapCfgToTM ab3TestGrammar
            -- doLaTeX $ interpretTM ["b", "a", "b", "a"] $ mapCfgToTM ab3TestGrammar
            -- doLaTeX $ fst $ smFinal tmForTestSm
            doLaTeX $ mapTM2TM' $ fst $ oneruleTM

-- main :: IO()
-- main = do
--     renderFile "out.tex" example 

main :: IO()
main = do
    let (tm, w) = oneruleTM
    let tm'@(TM (inp, tapes, states, cmds, StartStates start, access)) = mapTM2TM' tm
    --let sw = hubRelation $ sigmaFunc start w 
    let gr@(GR (a, r)) = smToGR $ smFinal $ tm'
    putStrLn $ (show $ length a) ++ " " ++ (show $ length r)
    let genmap = Map.fromList $ zip a $ map ((++) "f." . show) [1..]
    do fhandle <- openFile "oneruleTM.txt" WriteMode
       writeGap gr fhandle genmap
       hFlush fhandle
       hClose fhandle
    -- do handle <- openFile "oneruleTMWord.txt" WriteMode
    --    writeWord sw handle genmap
    --    hFlush handle
    --    hClose handle

oneruleTM = (tm, w) where
    a = Value "a"
    q0 = State "q_0^1"
    q1 = State "q_1^1"
    inp = InputAlphabet (Set.fromList [a])
    tapes = [TapeAlphabet (Set.fromList [a])]
    states = MultiTapeStates [Set.fromList [q0, q1]]
    cmd = [SingleTapeCommand ((a,  q0, rightBoundingLetter), (emptySymbol, q1, rightBoundingLetter))]
    cmds = Commands $ Set.fromList [cmd]
    start = StartStates [q0]
    access = AccessStates [q1]
    tm = TM (inp, tapes, states, cmds, start, access)
    w = [[SMType.SmbY $ SMType.Y a], [], [SMType.SmbY $ SMType.Y $ BCommand cmd], []]

simpleTM = tm where
    a = Value "a"
    q0 = State "q_0^1"
    q1 = State "q_1^1"
    q2 = State "q_2^1"
    inp = InputAlphabet (Set.fromList [a])
    tapes = [TapeAlphabet (Set.fromList [a])]
    states = MultiTapeStates [Set.fromList [q0, q1, q2]]
    cmds = Commands 
        $ Set.fromList [[SingleTapeCommand ((a,  q0, rightBoundingLetter), (emptySymbol, q1, rightBoundingLetter))],
                        [SingleTapeCommand ((emptySymbol,  q1, rightBoundingLetter), (emptySymbol, q2, rightBoundingLetter))]]
    start = StartStates [q0]
    access = AccessStates [q2]
    tm = TM (inp, tapes, states, cmds, start, access)    

tmForTestSm = tm where
    s = Value "S"
    q0 = State "q_0"
    q0' = State "q_0'"
    q1 = State "q_1"
    q1' = State "q_1'"
    inp = InputAlphabet (Set.fromList [])
    tapes = [TapeAlphabet (Set.fromList [s]), TapeAlphabet (Set.fromList [])]
    states = MultiTapeStates [Set.fromList [q0, q0'], Set.fromList [q1, q1']]
    cmds = Commands $ Set.fromList [[PreSMCommand ((s, StateOmega q0), (emptySymbol, StateOmega q0')), 
                                    PreSMCommand ((emptySymbol, StateOmega q1), (emptySymbol, StateOmega q1'))]]
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