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

preambula :: LaTeXM ()
preambula = 
    documentclass [] article
    <> usepackage [utf8] inputenc
    <> usepackage [] "unicode-math"
    <> title "Examples"


example :: LaTeX
example = execLaTeXM $ 
    do
        preambula 
        document $ do
            doLaTeX testGrammar
            doLaTeX $ mapCfgToTM testGrammar
            newpage
            doLaTeX $ interpretTM ["a"] $ mapCfgToTM testGrammar
            newpage
            doLaTeX seq1Grammar
            doLaTeX $ mapCfgToTM seq1Grammar
            newpage
            doLaTeX rpsGrammar
            doLaTeX $ mapCfgToTM rpsGrammar


main :: IO()
main = do
    renderFile "out.tex" example 



t  c = Terminal c
nt c = Nonterminal c
rel l rs = Relation (l, rs)


-- RPS example 
-- O -> (
-- C -> )
-- W -> OC
-- W -> WW
-- W -> QC
-- Q -> OW
-- S -> WW
rpsGrammar =
    Grammar(
        (Set.fromList [nS, nO, nC, nQ, nW]), 
        (Set.fromList [open, close]), 
        (Set.fromList relations),
        nS
    ) 
    where
        open = t "("
        close = t ")"

        nW = nt "W"
        nO = nt "O"
        nC = nt "C"
        nQ = nt "Q"
        nS = nt "S"

        relations = [
            rel nO [T open],
            rel nC [T close],
            rel nW [N nO, N nC],
            rel nW [N nW, N nW],
            rel nW [N nQ, N nC],
            rel nQ [N nO, N nS],
            rel nS [N nW, N nW]]


-- 1's sequence example
-- S -> OS
-- O -> 1
seq1Grammar =
    Grammar(
        (Set.fromList [nS, nO]), 
        (Set.fromList [one]), 
        (Set.fromList relations),
        nS
    ) 
    where
        one = t "1"

        nO = nt "O"
        nS = nt "S"

        relations = [
            rel nS [N nO, N nS],
            rel nO [T one]]


-- Example from test
testGrammar = grammar where
    terminal = Terminal "a"
    nonterminal = Nonterminal "S"
    grammar =
        Grammar(
            (Set.fromList [nonterminal]),
            (Set.fromList [terminal]),
            (Set.fromList [GrammarType.Relation (nonterminal,
            [GrammarType.T terminal])]),
            nonterminal
        )

