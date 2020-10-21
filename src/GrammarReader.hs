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
import Data.List
import qualified Data.List as List

import System.IO

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

parseFromFile p errorFile grammarFile = runParser p errorFile <$> ((fromString) <$> readFile grammarFile)

checkGrammarType :: Grammar -> GrammarType
checkGrammarType (Grammar (_, _, setOfRelations, _)) =
    checkGrammarType' (concat (map (\ (Relation (_, symbols)) -> symbols) (Set.toList setOfRelations)))

checkGrammarType' :: [Symbol] -> GrammarType
checkGrammarType' symbols
    | (List.elem (O Conjunction) symbols) && (List.elem (O Negation) symbols) = Boolean
    | (List.elem (O Conjunction) symbols) && not (List.elem (O Negation) symbols) = Conjunctive
    | not (List.elem (O Conjunction) symbols) && not (List.elem (O Negation) symbols) = CFG


--temporary added deriving show to all types in GrammarType module
main = do
    putStrLn "Enter the name of file with grammar. Grammar file should be in working directory."
    grammarFile <- getLine
    putStrLn "Enter the name of file for saving errors during the parsing. File with errors will be created in working directory."
    errorFile <- getLine
    result <- parseFromFile (pGrammar <* eof) errorFile grammarFile
    case result of
      Left err -> hPutStrLn stderr $ "Error: " ++ show err
      Right cs -> case (checkGrammarType cs) of
          Boolean -> putStrLn ("boolean") -- here will be new algorithm
          Conjunctive -> putStrLn ("conjunctive")
          CFG -> putStrLn ("cfg")

