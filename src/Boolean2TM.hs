{-# LANGUAGE TupleSections #-}

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
generateBlockForFindingNewSubstitution grammar@(Grammar (nonterminals, terminals, relations, startSymbol)) = let
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
  indices = map show [1..maxNumberOfRules]

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

generateBlockForScanResults :: Grammar -> DebuggingQuadruples
generateBlockForScanResults grammar@(Grammar (nonterminals, terminals, relations, startSymbol)) = let
    qMoveToEndAndScanResult = "qMoveToEndAndScanResult"
    qStart' = "qStart'"
    qSecPos = "qSecPos"
    qSecNeg = "qSecNeg"
    qBothPos = "qBothPos"
    qSomeNeg = "qSomeNeg"
    qWordsChangingMoveToBringSymbol = "qWordsChangingMoveToBringSymbol"
    -- parts for set of states q1FindNegation, q2FindNegation...qkFindNegation
    q = "q"
    findNegation = "FindNegation"

    maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar

    -- BLOCK for qMoveToEndAndScanResults
    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    signs = ["+", "-"]
    negation = ["!"]
    leftBracket = ["("]
    rightBracket = [")"]
    brackets = leftBracket ++ rightBracket
    indices = map show [1..maxNumberOfRules]
    allSymbols = terminalsList ++ nonterminalsList ++ signs ++ indices ++ brackets ++ negation

    symbolsInQMoveToEndAndScanResultQdrs = map
        (\t -> ((DState qMoveToEndAndScanResult, DSymbol t),(TMTypes.R, DState qMoveToEndAndScanResult))) allSymbols
    symbolsToQStart'Qdrs = [((DState qMoveToEndAndScanResult, DSymbol " "),(TMTypes.L, DState qStart'))]

    -- BLOCK for qStart'
    symbolsInQStart'Qdrs = map
        (\t -> ((DState qStart', DSymbol t),(TMTypes.L, DState qStart'))) $ brackets ++ terminalsList
    symbolsToQSecPosQdrs = [((DState qStart', DSymbol "+"),(TMTypes.L, DState qSecPos))]
    symbolsToQSecNegQdrs = [((DState qStart', DSymbol "-"),(TMTypes.L, DState qSecNeg))]

    -- BLOCK for qSecPos
    symbolsInQSecPosQdrs = map
        (\t -> ((DState qSecPos, DSymbol t),(TMTypes.L, DState qSecPos))) $ brackets ++ terminalsList ++ nonterminalsList
    symbolsToQBothPosQdrs = [((DState qSecPos, DSymbol "+"),(TMTypes.L, DState qBothPos))]
    symbolsFromQSecPosToQSomeNegQdrs = [((DState qSecPos, DSymbol "-"),(TMTypes.L, DState qSomeNeg))]

    -- BLOCK for qSecNeg: there is possibility to remove this state and move to qSomeNeg state
    symbolsInQSecNegQdrs = map
        (\t -> ((DState qSecNeg, DSymbol t),(TMTypes.L, DState qSecNeg))) $ brackets ++ terminalsList ++ nonterminalsList
    symbolsFromQSecNegToQSomeNegQdrs =
      [((DState qSecNeg, DSymbol "+"),(TMTypes.L, DState qSomeNeg)),((DState qSecNeg, DSymbol "-"),(TMTypes.L, DState qSomeNeg))]

    -- BLOCK for qBothPos
    symbolsInQBothPosQdrs = map
        (\t -> ((DState qBothPos, DSymbol t),(TMTypes.L, DState qBothPos))) $ rightBracket ++ negation ++ nonterminalsList
    symbolsToQ_ithFindNegationQdrs = map
        (\t -> ((DState qBothPos, DSymbol t),(TMTypes.L, DState $ q ++ t ++ findNegation))) indices

    -- BLOCK for qSomeNeg
    symbolsInSomeNegQdrs = map
        (\t -> ((DState qSomeNeg, DSymbol t),(TMTypes.L, DState qSomeNeg))) $ rightBracket ++ negation ++ nonterminalsList
    symbolsToQWordsChangingMoveToBringSymbolQdrs = map
        (\t -> ((DState qSomeNeg, DSymbol t),(TMTypes.R, DState qWordsChangingMoveToBringSymbol))) indices

    quadruples = symbolsInQMoveToEndAndScanResultQdrs ++ symbolsToQStart'Qdrs ++ symbolsInQStart'Qdrs
        ++ symbolsToQSecPosQdrs ++ symbolsToQSecNegQdrs ++ symbolsInQSecPosQdrs ++ symbolsToQBothPosQdrs
        ++ symbolsFromQSecPosToQSomeNegQdrs ++ symbolsInQSecNegQdrs ++ symbolsFromQSecNegToQSomeNegQdrs
        ++ symbolsInQBothPosQdrs ++ symbolsToQ_ithFindNegationQdrs ++ symbolsInSomeNegQdrs
        ++ symbolsToQWordsChangingMoveToBringSymbolQdrs

    in DQuadruples (addCollectionToMap quadruples Map.empty)

{---generateBlockForRefiningConjunctionDetails :: Grammar -> Quadruples
generateBlockForRefiningConjunctionDetails grammar@(Grammar (nonterminals, terminals, relations, startSymbol)) = let
    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    signs = ["+", "-"]
    negation = ["!"]
    leftBracket = ["("]
    rightBracket = [")"]
    brackets = leftBracket ++ rightBracket
    indices = map show [1..maxNumberOfRules] --}

generateBlockForQKFindNegation :: Grammar -> String -> DebuggingQuadruples
generateBlockForQKFindNegation grammar@(Grammar (nonterminals, terminals, relations, _)) k = let
    -- parts for set of states q1FindNegation, q2FindNegation...qkFindNegation
    q = "q"
    qkFindNegation = q ++ k ++ "FindNegation"
    qkMoveToStart = q ++ k ++ "MoveToStart"
    qkMoveToStartNeg = q ++ k ++ "MoveToStartNegation"
    qRuleKFindNonterminal = q ++ "Rule" ++ k ++ "KFindNonterminal"

    maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar

    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    signs = ["+", "-"]
    negation = ["!"]
    leftBracket = ["("]
    rightBracket = [")"]
    brackets = leftBracket ++ rightBracket
    indices = map show [1..maxNumberOfRules]

    -- BLOCK for qkFindNegation
    symbolsInQkFindNegation = [((DState qkFindNegation, DSymbol "("),(TMTypes.R, DState qkFindNegation))]
    symbolsToQkMoveToStart = map
        (\t -> ((DState qkFindNegation, DSymbol t),(TMTypes.L, DState qkMoveToStart))) nonterminalsList
    symbolsToQkMoveToStartNegation = [((DState qkFindNegation, DSymbol "!"),(TMTypes.L, DState qkMoveToStartNeg))]

    -- BLOCK for qkMoveToStart/qkMoveToStartNeg
    symbolsInQkMoveToStart = [((DState qkMoveToStart, DSymbol "("),(TMTypes.L, DState qkMoveToStart))]
    symbolsToQRulekFindNonterminal = [((DState qkMoveToStart, DSymbol k),(TMTypes.L, DState qRuleKFindNonterminal))]

    -- BLOCK for qRulekFindNonterminal
    nonterminalsWithKRels = map nonterminalValue (getNonterminalsWithKRelsAnyLong (Set.toList relations) (read k :: Int))
    symbolsToQRulekNonterminalFindFst = map (\t ->
            ((DState qRuleKFindNonterminal, DSymbol t),(TMTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ "findFst"))
            ) nonterminalsWithKRels

    -- BLOCK for qRulektFindFst
    symbolsInQRulektFindFst = map (\t ->
            ((DState $ q ++ "Rule" ++ k ++ t ++ "findFst", DSymbol t),(TMTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ "findFst"))
            ) $ [k] ++ rightBracket ++ negation

    triplets = concatMap (\t -> let firstNonterminals = getFirstNonterminalsInConjunctionsOfGivenRelation grammar t k
                       in map (k, t,) firstNonterminals) nonterminalsWithKRels

    symbolsToQRulektjFindSnd = map
        (\(k, t, j) -> ((DState $ q ++ "Rule" ++ k ++ t ++ "findFst", DSymbol j),
            (TMTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd"))) triplets

    -- BLOCK for qRulektjFindSnd
    quads = map (\(k, t, j) -> (k, t, j, getSecondNonterminalsInConjunctionsOfGivenRelation grammar k t j)) triplets
    symbolsInRulektjFindSnd = concatMap (\s ->
        map (\(k, t, j) ->
            ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol s),
             (TMTypes.L, DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd"))
            ) triplets) $ terminalsList ++ signs ++ brackets
    symbolsToQRulektjs = concatMap (\(k,t,j,s) -> map (\f -> ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol f),
        (TMTypes.L, DState $ q ++ "Rule" ++ k ++ t ++ j ++ f))) s) quads

    quadruples = symbolsToQRulekFindNonterminal ++ symbolsToQRulekNonterminalFindFst
            ++ symbolsInQRulektFindFst ++ symbolsToQRulektjFindSnd ++ symbolsInRulektjFindSnd ++ symbolsToQRulektjs
    commonQuadruples = symbolsInQkFindNegation ++ symbolsToQkMoveToStart
            ++ symbolsToQkMoveToStartNegation ++ symbolsInQkMoveToStart

    quadruplesMap = addCollectionToMap quadruples Map.empty
    quadruplesNegMap = generateCaseForNegConj quadruplesMap
    commonQuadruplesMap = addCollectionToMap commonQuadruples Map.empty

    in DQuadruples (Map.union commonQuadruplesMap $ Map.union quadruplesMap quadruplesNegMap)

generateCaseForNegConj :: Map.Map (DebuggingState, DebuggingSymbol) (SymbolMove, DebuggingState)
                       -> Map.Map (DebuggingState, DebuggingSymbol) (SymbolMove, DebuggingState)
generateCaseForNegConj quadruples = let
    quadruples' = Map.mapKeys (\(DState state, DSymbol symbol) -> (DState $ state ++ "Negation", DSymbol symbol)) quadruples
    in Map.map (\(move, DState state) -> (move, DState $ state ++ "Negation")) quadruples'

-- grammar -> string (nonterminal in left part) -> string (number of relation)
--  -> string (first nonterminal in conjunction) -> list of first nonterminals
getSecondNonterminalsInConjunctionsOfGivenRelation :: Grammar -> String -> String -> String -> [String]
getSecondNonterminalsInConjunctionsOfGivenRelation (Grammar (_, _, relations, _)) leftNonterminal number fstNontermInConj = let
    groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    givenRelation = (groupedRelations Map.! Nonterminal leftNonterminal) !! (read number :: Int)
    conjunctions = splitOn [O Conjunction] givenRelation
    symbol = N (Nonterminal fstNontermInConj)
    possibleConjunctions = filter (\t -> length t == 3 && t !! 1 == symbol || length t == 2 && head t == symbol) conjunctions
    possibleSndNonterminals = map (\t -> if length t == 3 then t !! 2 else t !! 1) possibleConjunctions
    in map refineSymbolInConjunctionToNonterminal possibleSndNonterminals

getNonterminalsWithKRelsAnyLong :: [Relation] -> Int -> [Nonterminal]
getNonterminalsWithKRelsAnyLong relations k = let
    groupedRelations = calculateGroupRelationsByNonterminals $ getLongRels relations
    in (Map.keys $ Map.filter (\t -> length t >= k) groupedRelations)

getLongRels :: [Relation] -> [Relation]
getLongRels = filter (not . relationHasOneTerminalInRightPart)

-- grammar -> string (nonterminal in left part) -> string (number of relation) -> list of first nonterminals
getFirstNonterminalsInConjunctionsOfGivenRelation :: Grammar -> String -> String -> [String]
getFirstNonterminalsInConjunctionsOfGivenRelation (Grammar (_, _, relations, _)) nonterminalValue number = do
    let groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    let givenRelation = (groupedRelations Map.! Nonterminal nonterminalValue) !! (read number :: Int)
    if relationHasOneTerminalInRightPart $ Relation (Nonterminal nonterminalValue, givenRelation)
        then []
        else let
        conjunctions = splitOn [O Conjunction] givenRelation
        firstSymbolsInConjunctions = map (\t -> if length t == 3 then t !! 1 else head t) conjunctions
        in map refineSymbolInConjunctionToNonterminal firstSymbolsInConjunctions

relationHasOneTerminalInRightPart :: Relation -> Bool
relationHasOneTerminalInRightPart (Relation (_, [T (Terminal _)])) = True
relationHasOneTerminalInRightPart _ = False

refineSymbolInConjunctionToNonterminal :: GrammarType.Symbol -> String
refineSymbolInConjunctionToNonterminal (N nonterminal) = nonterminalValue nonterminal
refineSymbolInConjunctionToNonterminal otherwise = error "Not a nonterminal, conjunction pair must be a pair of nonterminals"

addCollectionToMap :: (Ord k) => [(k, a)] -> Map.Map k a -> Map.Map k a
addCollectionToMap ((a,b) : xs) myMap = addCollectionToMap xs $ Map.insert a b myMap
addCollectionToMap [] myMap = myMap


{--generateQFoldConjCallNextConjFromSameRule :: Grammar -> Quadruples

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


