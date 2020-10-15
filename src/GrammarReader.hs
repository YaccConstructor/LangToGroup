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

pEpsilon :: Parser Symbol
pEpsilon = void ("Eps") >> pure Eps

pNonterminal :: Parser Nonterminal
pNonterminal = Nonterminal <$> ((++) <$> ((++) <$> ((:) <$> upperChar <*> (pure [])) <*> (many lowerChar)) <*> (many digitChar))

pTerminal :: Parser Terminal
pTerminal = Terminal <$> ((++) <$> (some lowerChar) <*> (many digitChar))

pword' = (many (void " " >> (T <$> pTerminal <|> N <$> pNonterminal))) <|> ((:) <$> pEpsilon <*> (pure []))

{---pword' = do
    word <- (++) <$> many (try ((T <$> pTerminal <|> (N <$> pNonterminal)) <* void " ")) <*> (pure [])
    word <- count' 0 1 pEpsilon
    return word --}
    --nonEmptyWord = (++) <$> many ((T <$> pTerminal <|> N <$> pNonterminal) <* void (" ")) <*> (count' 0 1 pEpsilon)
    --emptyWord = void " " >> (pure []) <$> pEpsilon

pWord :: Parser [Symbol]
pWord = (++) <$> pword' <*> (pure [])

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
main = do parseTest (pGrammar <* eof) "Sa;S-> Abc bbc"