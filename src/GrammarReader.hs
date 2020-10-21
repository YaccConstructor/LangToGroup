{-# LANGUAGE OverloadedStrings #-}
module GrammarReader (main) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Text (Text)
import Data.String
import Data.Void
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set

import GrammarType

type Parser = Parsec Void Text

pEpsilon :: Parser Symbol
pEpsilon = void ("Eps") >> pure Eps

pNonterminal :: Parser Nonterminal
pNonterminal = Nonterminal
    <$> (
        (++)
        <$> ((++) <$> ((:) <$> upperChar <*> (pure [])) <*> many lowerChar)
        <*> many digitChar
        )

pTerminal :: Parser Terminal
pTerminal = Terminal
    <$> (
        (++)
        <$> some lowerChar
        <*> many digitChar
        )

pword' = some (void " " *> (T <$> pTerminal <|> N <$> pNonterminal))
         <|>
         (:) <$> pEpsilon <*> (pure [])

pWord :: Parser [Symbol]
pWord = (++) <$> pword' <*> (pure [])

pConjunction :: Parser Symbol
pConjunction = void ("&") *> pure (O Conjunction)

pNegation :: Parser Symbol
pNegation = void ("!") *> pure (O Negation)

pVeryLongRule :: Parser [Symbol]
pVeryLongRule = do
    symbols <- pWord
    symbols <- (++) <$> pure symbols <*> concat <$> many pPositiveConjunction
    void ("_")
    symbols <- (++) <$> pure symbols <*> concat <$> many pNegativeConjunction
    return symbols

pNegativeConjunction :: Parser [Symbol]
pNegativeConjunction = do
    symbols1 <- pConjunction
    symbols2 <- pNegation
    symbols3 <- pWord
    symbols <- pure ((++) (symbols1 : symbols2 : []) symbols3)
    return symbols

pPositiveConjunction :: Parser [Symbol]
pPositiveConjunction = do
    symbols <- pConjunction
    symbols <- (++) <$> pure (symbols : []) <*> pWord
    return symbols

pPositiveFormula :: Parser [Symbol]
pPositiveFormula = do
    symbols <- pWord
    symbols <- ((++) <$> pure symbols <*> concat <$> many pPositiveConjunction)
    return symbols

pNegativeFormula :: Parser [Symbol]
pNegativeFormula = do
    symbols <- pNegation
    symbols <- (++) <$> pWord <*> pure (symbols : [])
    symbols <- ((++) <$> pure symbols <*> concat <$> many pNegativeConjunction)
    return symbols

pRelation :: Parser Relation
pRelation = do
     nonterminal <- pNonterminal
     void ("->")
     symbols <-  try (pVeryLongRule) <|> (pPositiveFormula) <|> (pNegativeFormula)
     relation <- Relation <$>  pure (nonterminal, symbols)
     return relation

pNonterminals :: Parser (Set Nonterminal)
pNonterminals = do
    nonterminals <- Set.fromList <$> ((++) <$> many (void " " *> pNonterminal) <*> (pure []))
    return nonterminals

pTerminals :: Parser (Set Terminal)
pTerminals = do
    terminals <- Set.fromList <$> ((++) <$> many (void " " *> pTerminal) <*> (pure []))
    return terminals

pRelations :: Parser (Set Relation)
pRelations = do
     relations <- Set.fromList <$> many (void (";") *> pRelation)
     return relations

pGrammar :: Parser Grammar
pGrammar = do
    startSymbol <- pNonterminal
    void (";")
    nonterminals <- pNonterminals
    void (";")
    terminals <- pTerminals
    relations <- pRelations
    grammar <- Grammar <$> pure (nonterminals, terminals, relations, startSymbol)
    return grammar

parseFromFile p file = runParser p file <$> readFile file

--temporary added deriving show to all types in GrammarType module
main = do
      content <- readFile "grammar1.txt"
      parseTest (pGrammar <* eof) (fromString content)
      --parseFromFile (pGrammar <* eof) "grammar1.txt"
      --runParser (pGrammar <* eof) "errors.txt" (fromString content)
    --parseTest (pGrammar <* eof) "Sa; S; c v b;S-> c_&! v&! b&! Eps"
    --parseTest (pGrammar <* eof) "Sa; S Abc D Cr; c b;S-> c b& d e;Abc-> b;D-> Cr"
    --parseTest (pGrammar <* eof) "Sa; S A D1 Cr; c2 b;S-> c2 b& d e_&! D1;A-> b;D1-> Cr"