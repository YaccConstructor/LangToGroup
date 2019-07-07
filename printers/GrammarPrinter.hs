{-# LANGUAGE OverloadedStrings #-}

module GrammarPrinter where


import Text.LaTeX.Base
import Text.LaTeX.Base.Commands
import Text.LaTeX.Packages.AMSMath

import GrammarType
import Lib


instance ShowLaTeX Nonterminal where
    doLaTeX (Nonterminal symbol) = fromString symbol 


instance ShowLaTeX Terminal where
    doLaTeX (Terminal symbol)    = fromString symbol


instance ShowLaTeX Symbol where
    doLaTeX (T symbol) = doLaTeX symbol 
    doLaTeX (N symbol) = doLaTeX symbol 


instance ShowLaTeX Relation where
    doLaTeX (Relation (nonterminal, symbols)) = do 
        math $ do 
            doLaTeX (N nonterminal) ; rightarrow ; mapM_ doLaTeX symbols ; lnbk


instance ShowLaTeX Grammar where
    doLaTeX (Grammar (nonterminals, terminals, relations, start)) = do
        subsection_ "Nonterminals"
        math $ mapM_ doLaTeX nonterminals ; lnbk
        subsection_ "Terminals"
        math $ mapM_ doLaTeX terminals    ; lnbk
        subsection_ "Rules"
        mapM_ doLaTeX relations           ; lnbk





