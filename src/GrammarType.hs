
-- |This module represents types of formal grammar. 
--
-- In the moment we use it to represent a context-free grammar, conjunctive grammar, boolean grammar.
module GrammarType where

import Data.Set (Set)

-- |'Operand' is a type that represents logical operations in 'Boolean' or 'Conjunctive' grammars.
data Operand = Conjunction | Negation
    deriving (Eq, Ord, Show)

-- |'Terminal' is a type that represents terminal in the formal grammar 'Grammar'.
newtype Terminal = Terminal String
    deriving (Eq, Ord, Show)

-- |'Nonterminal' is a type that represents nonterminal in the formal grammar 'Grammar'.
data Nonterminal = Nonterminal String
    deriving (Eq, Ord, Show)

-- |'Symbol' represents symbol that can be appear in right part of the 'Relation'.
--
-- 'T' is for 'Terminal'.
--
-- 'N' is for 'Nonterminal'.
--
-- 'O' is for 'Operand'.
--
-- And 'Eps' is for empty symbol, epsilon.
data Symbol = T Terminal | N Nonterminal | O Operand | Eps
    deriving (Eq, Ord, Show)

-- |This type is synonym 'Nonterminal' and used in order to separate 'StartSymbol' from normal 'Nonterminal'.
type StartSymbol = Nonterminal

-- |'Relation' is a rule of 'Grammar'. 
--
-- Notice that rule in context-free form. 
newtype Relation = Relation (Nonterminal, [Symbol])
    deriving (Eq, Ord, Show)

-- |This type we using to classify grammars.
data GrammarType = CFG | Conjunctive | Boolean
    deriving (Eq, Ord, Show)

-- |This type we using to represent a context-free formal grammar. 
newtype Grammar = Grammar (Set Nonterminal, Set Terminal, Set Relation, StartSymbol)
    deriving (Eq, Ord, Show)
