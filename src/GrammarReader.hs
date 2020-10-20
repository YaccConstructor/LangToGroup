{-# LANGUAGE OverloadedStrings #-}
module GrammarReader (main) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
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
pNonterminal = Nonterminal
    <$> (
        (++)
        <$> ((++) <$> ((:) <$> upperChar <*> (pure [])) <*> (many lowerChar))
        <*> (many digitChar)
        )

pTerminal :: Parser Terminal
pTerminal = Terminal
    <$> (
        (++)
        <$> (some lowerChar)
        <*> (many digitChar)
        )

pword' = (some (void " " *> (T <$> pTerminal <|> N <$> pNonterminal)))
         <|>
         ((:) <$> pEpsilon <*> (pure []))

pWord :: Parser [Symbol]
pWord = (++) <$> pword' <*> (pure [])

pConjunction :: Parser Symbol
pConjunction = void ("&") *> (pure (O Conjunction))

pNegation :: Parser Symbol
pNegation = void ("!") *> (pure (O Negation))

pVeryLongRule :: Parser [Symbol]
pVeryLongRule = do
    symbols <- pWord
    symbols <- (++) <$> ((concat) <$> many pPositiveConjunction) <*> (pure symbols)
    void ("_")
    symbols <- (++) <$> ((concat) <$> many pNegativeConjunction) <*> (pure symbols)
    return symbols

pNegativeConjunction :: Parser [Symbol]
pNegativeConjunction = do
    symbols1 <- pConjunction
    symbols2 <- pNegation
    symbols3 <- pWord
    symbols <- pure ((++) symbols3 (symbols1 : symbols2 : []))
    return symbols

pPositiveConjunction :: Parser [Symbol]
pPositiveConjunction = do
    symbols <- pConjunction
    symbols <- (++) <$> pWord <*> (pure (symbols : []))
    return symbols

pPositiveFormula :: Parser [Symbol]
pPositiveFormula = do
    symbols <- pWord
    symbols <- ( (++) <$> ((concat) <$> many pPositiveConjunction) <*> (pure symbols))
    return symbols

pNegativeFormula :: Parser [Symbol]
pNegativeFormula = do
    symbols <- pNegation
    symbols <- (++) <$> pWord <*> (pure (symbols : []))
    symbols <- ( (++) <$> ((concat) <$> many pNegativeConjunction) <*> (pure symbols))
    return symbols

pRelation :: Parser Relation
pRelation = do
     nonterminal <- pNonterminal
     void ("->")
     symbols <- dbg "rule" ( try (pVeryLongRule) <|> (pPositiveFormula) <|> (pNegativeFormula))
     {---symbols <- pWord

     symbols <- ( (++) <$> ((concat) <$> many pPositiveFormula) <*> (pure symbols))
     symbols <- ((++) <$> count' 0 1 pConjunction <*> (pure symbols))
     symbols <- (++) <$> ((concat) <$> many pNegativeFormula) <*> (pure symbols) --}
     relation <- (Relation <$>  (pure (nonterminal, symbols)))
     return relation

pRelations :: Parser (Set Relation)
pRelations = do
     relations <- Set.fromList <$> many (void (";") *> pRelation)
     return relations

pGrammar :: Parser Grammar
pGrammar = do
    startSymbol <- pNonterminal
    relations <- pRelations
    grammar <- (Grammar <$> pure (Set.empty, Set.empty, relations, startSymbol))
    return grammar

--temporary added deriving show to all types in GrammarType module
main = do parseTest (pGrammar <* eof) "Sa;S-> c_&! v&! b"
--main = do parseTest (pGrammar <* eof) "Sa;S-> Abc bbc&! b1;S-> Eps"