{-# LANGUAGE OverloadedStrings #-}

module GrammarPrinter where


import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Inputenc

import qualified Data.Set as Set
import GrammarType
import Lib

showNonterminals :: [Nonterminal] -> LaTeXM ()
showNonterminals = helper where
    helper [nonterminal]    = doLaTeX nonterminal
    helper (nonterminal:ss) = do { doLaTeX nonterminal; ","; showNonterminals ss }

showTerminals :: [Terminal] -> LaTeXM ()
showTerminals = helper where
    helper [terminal]    = doLaTeX terminal
    helper (terminal:ss) = do { doLaTeX terminal; ","; showTerminals ss }


instance ShowLaTeX Nonterminal where
    doLaTeX (Nonterminal symbol) = fromString symbol

instance ShowLaTeX Terminal where
    doLaTeX (Terminal symbol)    = fromString symbol

instance ShowLaTeX Epsilon where
    doLaTeX (Epsilon symbol)    = fromString symbol

instance ShowLaTeX Symbol where
    doLaTeX (T symbol) = doLaTeX symbol 
    doLaTeX (N symbol) = doLaTeX symbol 
    doLaTeX (E symbol) = doLaTeX symbol


instance ShowLaTeX Relation where
    doLaTeX (Relation (nonterminal, symbols)) = do 
        math $ do 
            doLaTeX (N nonterminal) ; rightarrow ; mapM_ doLaTeX symbols ; lnbk


instance ShowLaTeX Grammar where
    doLaTeX (Grammar (nonterminals, terminals, relations, start, eps)) = do
        subsection_ "Nonterminals"
       -- math $ mapM_ doLaTeX nonterminals ; lnbk
        math $ showNonterminals $ Set.toList nonterminals ; lnbk
        subsection_ "Terminals"
        math $ showTerminals $ Set.toList terminals ; lnbk
        subsection_ "Rules"
        mapM_ doLaTeX relations ; lnbk





