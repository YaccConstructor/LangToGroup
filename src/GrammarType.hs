module GrammarType where

import Data.Set (Set)
import qualified Data.Set as Set

newtype Terminal = Terminal Char
    deriving (Eq, Ord)
newtype Nonterminal = Nonterminal Char
    deriving (Eq, Ord)
-- union of terminal and nonterminal
data Symbol = T Terminal | N Nonterminal
    deriving (Eq, Ord)
-- start symbol must be a nonterminal synonym
type StartSymbol = Nonterminal
-- grammar relation
newtype Relation = Relation (Nonterminal, [Symbol])
    deriving (Eq, Ord)
newtype Relations = Relations (Set Relation)
    deriving (Eq, Ord)
newtype Nonterminals = Nonterminals (Set Nonterminal)
    deriving (Eq, Ord)
newtype Terminals = Terminals (Set Terminal)
    deriving (Eq, Ord)
-- grammar type
newtype Grammar = Grammar (Nonterminals, Terminals, Relations, StartSymbol)
    deriving (Eq, Ord)