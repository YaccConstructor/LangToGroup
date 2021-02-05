module Boolean2TM where

import GrammarType
import TMTypes
import Data.Char (chr, ord)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Data.List.Split (splitOn)

newtype SymbolsPair = SymbolsPair (Nonterminal, Int, Bool, GrammarType.Symbol, GrammarType.Symbol)
    deriving (Eq, Show)

-- firstly generating descriptional states (name of states describes, which activities TM does in this state),
-- after convert it to state Q_index from TMTypes module via bijective transformation
-- c |-> chr $ c + (ord 'a' - 1)
-- (transformation is bijective, since will use only chars 'A'..'Z','a'..'z', '1'..'9', '-' with disjoint prefixes for debugging names)
newtype DebuggingState = DState String
        deriving (Eq, Ord, Show)

newtype DebuggingSymbol = DSymbol String
        deriving (Eq, Ord, Show)

newtype DebuggingQuadruples = DQuadruples (Map.Map (DebuggingState, DebuggingSymbol) (SymbolMove, DebuggingState))
        deriving (Eq, Ord, Show)

finalDState :: DebuggingState
finalDState = DState " "
{---boolean2tm :: Grammar -> TuringMachine
boolean2tm
    (Grammar
        (setOfNonterminals,
        setOfTerminals,
        setOfRelations,
        Nonterminal startSymbol)) = do --}

-- naming for blocks of TM?
generateBlockForFindingNewSubstitution :: Grammar -> DebuggingQuadruples
generateBlockForFindingNewSubstitution grammar@(Grammar (nonterminals, terminals, relations, startSymbol))= let
  qFindNewSubstitution = "qFindNewSubstitution"
  qCheckIfNotCompleted = "qCheckIfNotCompleted"
  qSubstituteOrFold = "qSubstituteOrFold"
  qSkipCompletedNonterminal = "qSkipCompletedNonterminal"
  qCountWordLength' = "qCountWordLength'"
  qCountWordLength = "qCountWordLength"
  qMoveToEndToScanResults = "qMoveToEndToScanResults"
  qFold' = "qFold\'"

  terminalsList = Set.toList terminals
  nonterminalsList = Set.toList nonterminals
  signsList = ["+", "-"]
  negation = ["!"]

  maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar

  -- BLOCK for qFindNewSubstitution
  list = concat [map show [1..maxNumberOfRules], map terminalValue terminalsList, signsList, negation]
  -- there is no necessity in insertWithKey, since TM is deterministic
  symbolsInQFindNewSubstitutionQdrs = map (\t -> ((DState qFindNewSubstitution, DSymbol t),(TMTypes.L, DState qFindNewSubstitution))) list
  -- in TMTypes EmptySymbol is 0
  symbolsFromQFindNewSubstitutionToAcceptQdrs = [((DState qFindNewSubstitution, DSymbol " "),(TMTypes.R, finalDState))]
  symbolsWhichChangeStateQdrs =
    map ((\t -> ((DState qFindNewSubstitution, DSymbol t), (TMTypes.R, DState qCheckIfNotCompleted))) . nonterminalValue) nonterminalsList

  -- BLOCK for qCheckIfNotCompleted
  symbolsToQSubstituteOrFoldQdrs = map 
    ((\t -> ((DState qCheckIfNotCompleted, DSymbol t),(TMTypes.R, DState qSubstituteOrFold))) . show) [1..maxNumberOfRules]
  symbolsToQSkipCompletedNonterminalQdrs = map 
    (\t -> ((DState qCheckIfNotCompleted, DSymbol t),(TMTypes.L, DState qSkipCompletedNonterminal))) signsList
  symbolsFromQCheckIfNotCompletedToAcceptQdrs = [((DState qCheckIfNotCompleted, DSymbol " "),(TMTypes.R, finalDState))]

  -- BLOCK for qSubstituteOrFold
  symbolsInQSubstituteOrFoldQdrs = [((DState qSubstituteOrFold, DSymbol "("),(TMTypes.R, finalDState))]
  symbolsToCountWordLength'Qdrs = map
    ((\t -> ((DState qSubstituteOrFold, DSymbol t),(TMTypes.L, DState qCountWordLength'))) . terminalValue) terminalsList
  symbolsToQFold'Qdrs = map 
    (\t -> ((DState qSubstituteOrFold, DSymbol t),(TMTypes.L, DState qCountWordLength'))) $ map nonterminalValue nonterminalsList ++ negation

  -- BLOCK for qCountWordLength'
  symbolsToCountWordLengthQdrs = [((DState qCountWordLength', DSymbol "("),(TMTypes.L, DState qCountWordLength))]

  -- BLOCK for qFold'
  symbolsToQMoveToEndToScanResults = [((DState qFold', DSymbol "("),(TMTypes.L, DState qMoveToEndToScanResults))]

  -- BLOCK for qSkipCompletedNonterminal
  symbolsToQFindNewSubstitutionQdrs = map 
    ((\t -> ((DState qSkipCompletedNonterminal, DSymbol t),(TMTypes.L, DState qFindNewSubstitution))) . nonterminalValue) nonterminalsList

  quadruples = symbolsInQFindNewSubstitutionQdrs ++ symbolsFromQFindNewSubstitutionToAcceptQdrs ++ symbolsWhichChangeStateQdrs 
    ++ symbolsToQSubstituteOrFoldQdrs ++ symbolsToQSkipCompletedNonterminalQdrs ++ symbolsFromQCheckIfNotCompletedToAcceptQdrs
    ++ symbolsInQSubstituteOrFoldQdrs ++ symbolsToCountWordLength'Qdrs ++ symbolsToQFold'Qdrs ++ symbolsToCountWordLengthQdrs 
    ++ symbolsToQMoveToEndToScanResults ++ symbolsToQFindNewSubstitutionQdrs
  in DQuadruples (addCollectionToMap quadruples Map.empty)


addCollectionToMap :: (Ord k) => [(k, a)] -> Map.Map k a -> Map.Map k a
addCollectionToMap ((a,b) : xs) myMap = addCollectionToMap xs $ Map.insert a b myMap
addCollectionToMap [] myMap = myMap
  
{--generateQRefineConjunctionDetails :: Grammar -> Quadruples

generateQFoldConjCallNextConjFromSameRule :: Grammar -> Quadruples

generateQFoldConjCallNextConjFromNextRule :: Grammar -> Quadruples

generateQFoldConjunctionPutResult :: Grammar -> Quadruples

generateQRetryConjunction :: Grammar -> Quadruples

generateQCreateNewSubstitution :: Grammar -> Quadruples

generateQFigureWordOrSymbol :: Grammar -> Quadruples

generateQGetResultForSymbol :: Grammar -> Quadruples --}

-- helper functions
convertDebuggingStateToState :: DebuggingState -> State
convertDebuggingStateToState (DState string) = Q number where
    number = read (concatMap (show . ord) string) :: Int

calculateMaxNumberOfRulesForNonterminal :: Grammar -> Int
calculateMaxNumberOfRulesForNonterminal (Grammar (_, _, relations, _)) = let 
    listRelations = Set.toList relations
    groupedRelations = calculateGroupRelationsByNonterminals listRelations
    in (snd $ maximumBy (comparing snd) (Map.toList $ Map.map length groupedRelations))


calculateGroupRelationsByNonterminals :: [Relation] -> Map.Map Nonterminal [[GrammarType.Symbol]]
calculateGroupRelationsByNonterminals relations = let
    mapWithReversedRelationsOrder = Map.fromListWith (++) [(nonterminal, [symbols]) | Relation (nonterminal, symbols) <- relations]
    in Map.map sort mapWithReversedRelationsOrder

calculateNextConjunctionInSameRule :: Grammar -> SymbolsPair -> Maybe SymbolsPair
calculateNextConjunctionInSameRule (Grammar (_, _, relations, _)) 
                                   (SymbolsPair (nonterminal, relationNumber, hasNeg, N conjNonterminal1, N conjNonterminal2)) = do
                                   let groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
                                   let relationsForNonterminal = groupedRelations Map.! nonterminal
                                   -- exception: index too large, it is incorrest situation - throwing exception
                                   let relation = relationsForNonterminal !! relationNumber
                                   let conjunctionPairs = splitOn [O Conjunction] relation
                                   let list = if hasNeg then [O Negation, N conjNonterminal1, N conjNonterminal2] else [N conjNonterminal1, N conjNonterminal2]
                                   let conjunctionPairIndex = elemIndex list conjunctionPairs
                                   case conjunctionPairIndex of
                                        Just index | index == (length conjunctionPairs - 1) -> Nothing
                                                   | otherwise -> Just $ convertListToConjunctionPair nonterminal relationNumber (conjunctionPairs !! (index + 1))
                                        Nothing -> error "No such conjunction in given rule. "
calculateNextConjunctionInSameRule _ _ = error "Conjunction must be pair of nonterminals. "

convertListToConjunctionPair :: Nonterminal -> Int -> [GrammarType.Symbol] -> SymbolsPair
convertListToConjunctionPair nonterminal relationNumber [_, N conjNonterminal1, N conjNonterminal2] =
    SymbolsPair (nonterminal, relationNumber, True, N conjNonterminal1 , N conjNonterminal2)
convertListToConjunctionPair nonterminal relationNumber [N conjNonterminal1, N conjNonterminal2] =
    SymbolsPair (nonterminal, relationNumber, False, N conjNonterminal1, N conjNonterminal2)
convertListToConjunctionPair _ _ _ = error "Conjunction must be pair of nonterminals. "

calculateFirstConjunctionInNextRule :: Grammar -> SymbolsPair -> Maybe SymbolsPair
calculateFirstConjunctionInNextRule (Grammar (_, _, relations, _))
                                   (SymbolsPair (nonterminal, relationNumber, _, _, _)) = do
                                   let groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
                                   let relationsForNonterminal = groupedRelations Map.! nonterminal
                                   let nextRelationNumber = relationNumber + 1
                                   if nextRelationNumber >= length relationsForNonterminal
                                        then Nothing
                                        else
                                          let
                                          relation = relationsForNonterminal !! nextRelationNumber
                                          conjunctionPairs = splitOn [O Conjunction] relation
                                          in Just $ convertListToConjunctionPair nonterminal nextRelationNumber $ head conjunctionPairs


