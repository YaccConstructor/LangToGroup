module Boolean2TM where

import GrammarType
import TMTypes

{--boolean2tm :: Grammar -> TuringMachine
boolean2tm
    (Grammar
        (setOfNonterminals,
        setOfTerminals,
        setOfRelations,
        Nonterminal startSymbol)) = do --}
--
newtype ConjunctionPair = ConjunctionPair (Integer, Nonterminal, Nonterminal)

-- naming for blocks of TM?
generateQFindNewSubstitution :: Grammar -> Quadruples

generateQRefineConjunctionDetails :: Grammar -> Quadruples

generateQFoldConjCallNextConjFromSameRule :: Grammar -> Quadruples

generateQFoldConjCallNextConjFromNextRule :: Grammar -> Quadruples

generateQFoldConjunctionPutResult :: Grammar -> Quadruples

generateQRetryConjunction :: Grammar -> Quadruples

generateQCreateNewSubstitution :: Grammar -> Quadruples

generateQFigureWordOrSymbol :: Grammar -> Quadruples

generateQGetResultForSymbol :: Grammar -> Quadruples

-- helper functions
calculateMaxNumberOfRulesForNonterminal :: Grammar -> Integer

calculateNextConjunctionInSameRule :: ConjunctionPair -> ConjunctionPair

calculateNextConjunctionInNextRule :: ConjunctionPair -> ConjunctionPair





