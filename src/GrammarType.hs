 -- |This module represents types of formal grammar.
--
-- In the moment we use it to represent a context-free grammar, conjunctive grammar, boolean grammar.

{-# LANGUAGE DataKinds #-}
module GrammarType where

import Data.Set (Set)

-- |'Terminal' is a type that represents terminal in the formal grammar 'Grammar'.
newtype Terminal = Terminal {terminalValue :: String}
    deriving (Eq, Ord, Show)

-- |'Nonterminal' is a type that represents nonterminal in the formal grammar 'Grammar'.
newtype Nonterminal = Nonterminal {nonterminalValue :: String}
    deriving (Eq, Ord, Show)

-- |'Symbol' represents symbol that can be appear in right part of the 'Relation'.
--
-- 'T' is for 'Terminal'.
--
-- 'N' is for 'Nonterminal'.
--
-- And 'Eps' is for empty symbol, epsilon.
data Symbol = T Terminal | N Nonterminal | Eps
    deriving (Eq, Ord, Show)

newtype PConj = PConj [Symbol]
    deriving (Eq, Ord, Show)

newtype NConj = NConj [Symbol]
    deriving (Eq, Ord, Show)

data Conj = PosConj' PConj | NegConj' NConj
    deriving (Eq, Ord, Show)
    
-- |This type is synonym 'Nonterminal' and used in order to separate 'StartSymbol' from normal 'Nonterminal'.
type StartSymbol = Nonterminal

-- |'Relation' is a rule of 'Grammar'. 
--
-- Rule might be in context-free, conjunctive or boolean form.
data Relation = Relation (Nonterminal, PConj) | BooleanRelation (Nonterminal, [Conj])
    deriving (Eq, Ord, Show)

-- |This type we using to classify grammars.
data GrammarType = CFG | Conjunctive | Boolean
    deriving (Eq, Ord, Show)

-- |This type we using to represent a formal grammar.
newtype Grammar = Grammar (Set Nonterminal, Set Terminal, Set Relation, StartSymbol)
    deriving (Eq, Ord, Show)
