{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.LaTeX.Base.Render
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Base.Math
import qualified Data.Set as Set

import GrammarPrinter
import Lib
import GrammarType


open = Terminal '('
close = Terminal ')'

rS = Nonterminal 'S'

relation1 = GrammarType.Relation (rS, [GrammarType.T open, GrammarType.T close])
relation2 = GrammarType.Relation (rS, [GrammarType.N rS, GrammarType.N rS])
relation3 = GrammarType.Relation (rS, [GrammarType.T open, GrammarType.N rS, GrammarType.T close])
grammar =
    Grammar(
        (Set.fromList [rS]), 
        (Set.fromList [open, close]), 
        (Set.fromList [relation1, relation2, relation3]),
        rS
    )
 

preambula :: LaTeXM ()
preambula = 
    documentclass [] article
    <> usepackage [utf8] inputenc
    <> title "Examples with trees"
    <> author "Daniel Díaz"


example :: LaTeX
example = execLaTeXM $ 
    do
        preambula 
        document $ doLaTeX grammar


main :: IO()
main = renderFile "out.tex" example 
