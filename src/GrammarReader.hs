{-# LANGUAGE OverloadedStrings #-}
module GrammarReader (main) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set

import GrammarType

type Parser = Parsec Void Text

pNonterminal :: Parser Nonterminal
pNonterminal = Nonterminal <$> ((++) <$> ((++) <$> (some upperChar) <*> (many lowerChar)) <*> (many digitChar))

pTerminal :: Parser Terminal
pTerminal = Terminal <$> ((++) <$> (some lowerChar) <*> (many digitChar))

pWord :: Parser [Symbol]
pWord = (:) <$> (T <$> pTerminal) <*> (pure [])

pRelations :: Parser (Set Relation)
pRelations = do
	nonterminal <- pNonterminal
	void ("->")
	symbols <- pWord
	relations <- Set.insert <$> (Relation <$>  (pure (nonterminal, symbols))) <*> (pure Set.empty)
	return relations

pGrammar :: Parser Grammar
pGrammar = do
	startSymbol <- pNonterminal
	void (char ';')
	relations <- pRelations
	grammar <- (Grammar <$> pure (Set.empty, Set.empty, relations, startSymbol))
	return grammar

--temporary added deriving show to all types in GrammarType module
main = do parseTest (pGrammar <* eof) "Sba;S->a"