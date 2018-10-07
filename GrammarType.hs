module GrammarType where

import Data.Set (Set)
import qualified Data.Set as Set

newtype Terminal = Terminal Char
newtype Nonterminal = Nonterminal Char
-- union of terminal and nonterminal
data Symbol = T Terminal | N Nonterminal
-- start symbol must be a nonterminal synonym
type StartSymbol = Nonterminal
-- grammar relation
newtype Relation = Relation (Nonterminal, [Symbol])
newtype Relations = Relations (Set Relation)
newtype Nonterminals = Nonterminals (Set Nonterminal)
newtype Terminals = Terminals (Set Terminal)
-- grammar type
newtype Grammar = Grammar (Nonterminals, Terminals, Relations, StartSymbol)