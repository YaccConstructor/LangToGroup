{-# LANGUAGE OverloadedStrings #-}
module GrammarReader (main) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void
import Data.Functor

import GrammarType

type Parser = Parsec Void Text


pStartSymbol :: Parser String
pStartSymbol = (++) <$> (some upperChar) <*> (many lowerChar)

pGrammar :: Parser StartSymbol
pGrammar = do
	startSymbol <- pStartSymbol
	void (char ':')
	return (Nonterminal startSymbol)

--temporary added deriving show in Nonterminal in GrammarType module
main = do parseTest (pGrammar <* eof) "Sba:"
--main = do runParser(pGrammar <* eof) "" "Sba:"