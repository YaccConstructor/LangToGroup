module GrammarType where

import Data.Set (Set)
import qualified Data.Set as Set

newtype Terminal = Terminal String
    deriving (Eq, Ord)
newtype Nonterminal = Nonterminal String
    deriving (Eq, Ord)
-- union of terminal and nonterminal
data Symbol = T Terminal | N Nonterminal
    deriving (Eq, Ord)
-- start symbol must be a nonterminal synonym
type StartSymbol = Nonterminal
-- grammar relation
newtype Relation = Relation (Nonterminal, [Symbol])
    deriving (Eq, Ord)
-- grammar type
newtype Grammar = Grammar (Set Nonterminal, Set Terminal, Set Relation, StartSymbol)
    deriving (Eq, Ord)
