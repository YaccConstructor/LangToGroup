{-# LANGUAGE TupleSections #-}

module Boolean2TM where

import GrammarType
import TMTypes
import DebuggingTypes
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Data.List.Split (splitOn)
import Data.Maybe

newtype SymbolsPair = SymbolsPair (Nonterminal, Int, Bool, GrammarType.Symbol, GrammarType.Symbol)
    deriving (Eq, Show)

{---boolean2tm :: Grammar -> TuringMachine
boolean2tm
    (Grammar
        (setOfNonterminals,
        setOfTerminals,
        setOfRelations,
        Nonterminal startSymbol)) = do --}

-- naming for blocks of TM?
generateBlockForFindingNewSubstitution :: Grammar -> DebuggingQuadruples
generateBlockForFindingNewSubstitution grammar@(Grammar (nonterminals, terminals, _, _)) = let
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
  indices= map show [1..maxNumberOfRules]

  maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar

  -- BLOCK for qFindNewSubstitution
  list = concat [map show indices, map terminalValue terminalsList, signsList, negation]
  -- there is no necessity in insertWithKey, since TM is deterministic
  symbolsInQFindNewSubstitutionQdrs = map (\t -> ((DState qFindNewSubstitution, DSymbol t),(DebuggingTypes.L, DState qFindNewSubstitution))) list
  -- in TMTypes EmptySymbol is 0
  symbolsFromQFindNewSubstitutionToAcceptQdrs = [((DState qFindNewSubstitution, DSymbol " "),(DebuggingTypes.R, finalDState))]
  symbolsWhichChangeStateQdrs =
    map ((\t -> ((DState qFindNewSubstitution, DSymbol t), (DebuggingTypes.R, DState qCheckIfNotCompleted))) . nonterminalValue) nonterminalsList

  -- BLOCK for qCheckIfNotCompleted
  symbolsToQSubstituteOrFoldQdrs = map 
    ((\t -> ((DState qCheckIfNotCompleted, DSymbol t),(DebuggingTypes.R, DState qSubstituteOrFold))) . show) indices
  symbolsToQSkipCompletedNonterminalQdrs = map 
    (\t -> ((DState qCheckIfNotCompleted, DSymbol t),(DebuggingTypes.L, DState qSkipCompletedNonterminal))) signsList
  symbolsFromQCheckIfNotCompletedToAcceptQdrs = [((DState qCheckIfNotCompleted, DSymbol " "),(DebuggingTypes.R, finalDState))]

  -- BLOCK for qSubstituteOrFold
  symbolsInQSubstituteOrFoldQdrs = [((DState qSubstituteOrFold, DSymbol "("),(DebuggingTypes.R, finalDState))]
  symbolsToCountWordLength'Qdrs = map
    ((\t -> ((DState qSubstituteOrFold, DSymbol t),(DebuggingTypes.L, DState qCountWordLength'))) . terminalValue) terminalsList
  symbolsToQFold'Qdrs = map 
    (\t -> ((DState qSubstituteOrFold, DSymbol t),(DebuggingTypes.L, DState qCountWordLength'))) $ map nonterminalValue nonterminalsList ++ negation

  -- BLOCK for qCountWordLength'
  symbolsToCountWordLengthQdrs = [((DState qCountWordLength', DSymbol "("),(DebuggingTypes.L, DState qCountWordLength))]

  -- BLOCK for qFold'
  symbolsToQMoveToEndToScanResults = [((DState qFold', DSymbol "("),(DebuggingTypes.L, DState qMoveToEndToScanResults))]

  -- BLOCK for qSkipCompletedNonterminal
  symbolsToQFindNewSubstitutionQdrs = map 
    ((\t -> ((DState qSkipCompletedNonterminal, DSymbol t),(DebuggingTypes.L, DState qFindNewSubstitution))) . nonterminalValue) nonterminalsList

  quadruples = symbolsInQFindNewSubstitutionQdrs ++ symbolsFromQFindNewSubstitutionToAcceptQdrs ++ symbolsWhichChangeStateQdrs 
    ++ symbolsToQSubstituteOrFoldQdrs ++ symbolsToQSkipCompletedNonterminalQdrs ++ symbolsFromQCheckIfNotCompletedToAcceptQdrs
    ++ symbolsInQSubstituteOrFoldQdrs ++ symbolsToCountWordLength'Qdrs ++ symbolsToQFold'Qdrs ++ symbolsToCountWordLengthQdrs 
    ++ symbolsToQMoveToEndToScanResults ++ symbolsToQFindNewSubstitutionQdrs
  in DQuadruples (addCollectionToMap quadruples Map.empty)

generateBlockForScanResults :: Grammar -> DebuggingQuadruples
generateBlockForScanResults grammar@(Grammar (nonterminals, terminals, _, _)) = let
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
        (\t -> ((DState qMoveToEndAndScanResult, DSymbol t),(DebuggingTypes.R, DState qMoveToEndAndScanResult))) allSymbols
    symbolsToQStart'Qdrs = [((DState qMoveToEndAndScanResult, DSymbol " "),(DebuggingTypes.L, DState qStart'))]

    -- BLOCK for qStart'
    symbolsInQStart'Qdrs = map
        (\t -> ((DState qStart', DSymbol t),(DebuggingTypes.L, DState qStart'))) $ brackets ++ terminalsList
    symbolsToQSecPosQdrs = [((DState qStart', DSymbol "+"),(DebuggingTypes.L, DState qSecPos))]
    symbolsToQSecNegQdrs = [((DState qStart', DSymbol "-"),(DebuggingTypes.L, DState qSecNeg))]

    -- BLOCK for qSecPos
    symbolsInQSecPosQdrs = map
        (\t -> ((DState qSecPos, DSymbol t),(DebuggingTypes.L, DState qSecPos))) $ brackets ++ terminalsList ++ nonterminalsList
    symbolsToQBothPosQdrs = [((DState qSecPos, DSymbol "+"),(DebuggingTypes.L, DState qBothPos))]
    symbolsFromQSecPosToQSomeNegQdrs = [((DState qSecPos, DSymbol "-"),(DebuggingTypes.L, DState qSomeNeg))]

    -- BLOCK for qSecNeg: there is possibility to remove this state and move to qSomeNeg state
    symbolsInQSecNegQdrs = map
        (\t -> ((DState qSecNeg, DSymbol t),(DebuggingTypes.L, DState qSecNeg))) $ brackets ++ terminalsList ++ nonterminalsList
    symbolsFromQSecNegToQSomeNegQdrs =
      [((DState qSecNeg, DSymbol "+"),(DebuggingTypes.L, DState qSomeNeg)),((DState qSecNeg, DSymbol "-"),(DebuggingTypes.L, DState qSomeNeg))]

    -- BLOCK for qBothPos
    symbolsInQBothPosQdrs = map
        (\t -> ((DState qBothPos, DSymbol t),(DebuggingTypes.L, DState qBothPos))) $ rightBracket ++ negation ++ nonterminalsList
    symbolsToQ_ithFindNegationQdrs = map
        (\t -> ((DState qBothPos, DSymbol t),(DebuggingTypes.L, DState $ q ++ t ++ findNegation))) indices

    -- BLOCK for qSomeNeg
    symbolsInSomeNegQdrs = map
        (\t -> ((DState qSomeNeg, DSymbol t),(DebuggingTypes.L, DState qSomeNeg))) $ rightBracket ++ negation ++ nonterminalsList
    symbolsToQWordsChangingMoveToBringSymbolQdrs = map
        (\t -> ((DState qSomeNeg, DSymbol t),(DebuggingTypes.R, DState qWordsChangingMoveToBringSymbol))) indices

    quadruples = symbolsInQMoveToEndAndScanResultQdrs ++ symbolsToQStart'Qdrs ++ symbolsInQStart'Qdrs
        ++ symbolsToQSecPosQdrs ++ symbolsToQSecNegQdrs ++ symbolsInQSecPosQdrs ++ symbolsToQBothPosQdrs
        ++ symbolsFromQSecPosToQSomeNegQdrs ++ symbolsInQSecNegQdrs ++ symbolsFromQSecNegToQSomeNegQdrs
        ++ symbolsInQBothPosQdrs ++ symbolsToQ_ithFindNegationQdrs ++ symbolsInSomeNegQdrs
        ++ symbolsToQWordsChangingMoveToBringSymbolQdrs

    in DQuadruples (addCollectionToMap quadruples Map.empty)

generateBlockForRefiningConjunctionDetails :: Grammar -> DebuggingQuadruples
generateBlockForRefiningConjunctionDetails grammar = let
    maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [1..maxNumberOfRules]
    quadruplesList = map (generateBlockForQKFindNegation grammar) indices
    in unionQuadruples quadruplesList

unionQuadruples :: [DebuggingQuadruples] -> DebuggingQuadruples
unionQuadruples [quadruple] = quadruple
unionQuadruples quadruples = DQuadruples (Map.unions $ map (\(DQuadruples x) -> x) quadruples)

generateBlockForQKFindNegation :: Grammar -> String -> DebuggingQuadruples
generateBlockForQKFindNegation grammar@(Grammar (nonterminals, terminals, relations, _)) k = let
    -- parts for set of states q1FindNegation, q2FindNegation...qkFindNegation
    q = "q"
    qkFindNegation = q ++ k ++ "FindNegation"
    qkMoveToStart = q ++ k ++ "MoveToStart"
    qkMoveToStartNeg = q ++ k ++ "MoveToStartNegation"
    qRuleKFindNonterminal = q ++ "Rule" ++ k ++ "KFindNonterminal"

    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    signs = ["+", "-"]
    negation = ["!"]
    leftBracket = ["("]
    rightBracket = [")"]
    brackets = leftBracket ++ rightBracket

    -- BLOCK for qkFindNegation
    symbolsInQkFindNegation = [((DState qkFindNegation, DSymbol "("),(DebuggingTypes.R, DState qkFindNegation))]
    symbolsToQkMoveToStart = map
        (\t -> ((DState qkFindNegation, DSymbol t),(DebuggingTypes.L, DState qkMoveToStart))) nonterminalsList
    symbolsToQkMoveToStartNegation = [((DState qkFindNegation, DSymbol "!"),(DebuggingTypes.L, DState qkMoveToStartNeg))]

    -- BLOCK for qkMoveToStart/qkMoveToStartNeg
    symbolsInQkMoveToStart = [((DState qkMoveToStart, DSymbol "("),(DebuggingTypes.L, DState qkMoveToStart))]
    symbolsToQRulekFindNonterminal = [((DState qkMoveToStart, DSymbol k),(DebuggingTypes.L, DState qRuleKFindNonterminal))]

    -- BLOCK for qRulekFindNonterminal
    nonterminalsWithKRels = map nonterminalValue (getNonterminalsWithKRelsAnyLong (Set.toList relations) (read k :: Int))
    symbolsToQRulekNonterminalFindFst = map (\t ->
            ((DState qRuleKFindNonterminal, DSymbol t),(DebuggingTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ "findFst"))
            ) nonterminalsWithKRels

    -- BLOCK for qRulektFindFst
    symbolsInQRulektFindFst = map (\t ->
            ((DState $ q ++ "Rule" ++ k ++ t ++ "findFst", DSymbol t),(DebuggingTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ "findFst"))
            ) $ [k] ++ rightBracket ++ negation

    triplets = calculateTriplets grammar k nonterminalsWithKRels

    symbolsToQRulektjFindSnd = map
        (\(k, t, j) -> ((DState $ q ++ "Rule" ++ k ++ t ++ "findFst", DSymbol j),
            (DebuggingTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd"))) triplets

    -- BLOCK for qRulektjFindSnd
    quads = calculateQuads $ calculateQuads' grammar triplets
    symbolsInRulektjFindSnd = concatMap (\s ->
        map (\(k, t, j) ->
            ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol s),
             (DebuggingTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd"))
            ) triplets) $ terminalsList ++ signs ++ brackets

    {--symbolsToQRulektjs = concatMap (\(k,t,j,s) -> map (\f -> ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol f),
        (DebuggingTypes.L, DState $ q ++ "Rule" ++ k ++ t ++ j ++ f))) s) quads --}
    symbolsToQRulektjs = map (\(k,t,j,f) -> ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol f),
            (DebuggingTypes.L, DState $ q ++ "Rule" ++ k ++ t ++ j ++ f))) quads

    quadruples = symbolsToQRulekFindNonterminal ++ symbolsToQRulekNonterminalFindFst
            ++ symbolsInQRulektFindFst ++ symbolsToQRulektjFindSnd ++ symbolsInRulektjFindSnd ++ symbolsToQRulektjs
    commonQuadruples = symbolsInQkFindNegation ++ symbolsToQkMoveToStart
            ++ symbolsToQkMoveToStartNegation ++ symbolsInQkMoveToStart

    quadruplesMap = addCollectionToMap quadruples Map.empty
    quadruplesNegMap = generateCaseForNegConj quadruplesMap
    commonQuadruplesMap = addCollectionToMap commonQuadruples Map.empty

    in DQuadruples (Map.union commonQuadruplesMap $ Map.union quadruplesMap quadruplesNegMap)

generateCaseForNegConj :: Map.Map (DebuggingState, DebuggingSymbol) (DebuggingMove, DebuggingState)
                       -> Map.Map (DebuggingState, DebuggingSymbol) (DebuggingMove, DebuggingState)
generateCaseForNegConj quadruples = let
    quadruples' = Map.mapKeys (\(DState state, DSymbol symbol) -> (DState $ state ++ "Negation", DSymbol symbol)) quadruples
    in Map.map (\(move, DState state) -> (move, DState $ state ++ "Negation")) quadruples'

calculateTriplets :: Foldable t => Grammar -> String -> t String -> [(String, String, String)]
calculateTriplets grammar number
  = concatMap
      (\ t
         -> let
              firstNonterminals
                = getFirstNonterminalsInConjunctionsOfGivenRelation
                    grammar t number
            in map (number, t,) firstNonterminals)

calculateQuads' :: Grammar -> [([Char], [Char], [Char])] -> [([Char], [Char], [Char], [String])]
calculateQuads' grammar = map (\ (k, t, j) -> (k, t, j, getSecondNonterminalsInConjunctionsOfGivenRelation grammar k t j))

calculateQuads :: Foldable t => t (a, b, c, [d]) -> [(a, b, c, d)]
calculateQuads = concatMap (\ (k, t, j, s) -> map (k, t, j,) s)
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
getFirstNonterminalsInConjunctionsOfGivenRelation (Grammar (_, _, relations, _)) nonterminal number = do
    let groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    let givenRelation = (groupedRelations Map.! Nonterminal nonterminal) !! (read number :: Int)
    if relationHasOneTerminalInRightPart $ Relation (Nonterminal nonterminal, givenRelation)
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
refineSymbolInConjunctionToNonterminal _ = error "Not a nonterminal, conjunction pair must be a pair of nonterminals"

addCollectionToMap :: (Ord k) => [(k, a)] -> Map.Map k a -> Map.Map k a
addCollectionToMap ((a,b) : xs) myMap = addCollectionToMap xs $ Map.insert a b myMap
addCollectionToMap [] myMap = myMap

-- helper functions
convertDebuggingStateToState :: DebuggingState -> State
convertDebuggingStateToState (DState string) = Q number where
    number = read (concatMap (show . ord) string) :: Int


--BLOCK for moving from conjunction results to next blocks
generateTransitionFromConjunctionResult :: Grammar -> DebuggingQuadruples
generateTransitionFromConjunctionResult grammar@(Grammar (_, terminals, relations, _)) = let
    q = "q"
    rewrite = "Rewrite"
    transition = "Transition"
    skipParentNonterminal = "skipParentNonterminal"
    rule = "Rule"
    terminalsList = map terminalValue $ Set.toList terminals
    plus = "+"
    minus = "-"
    signs = [plus, minus]
    negation = ["!"]
    leftBracket = ["("]
    rightBracket = [")"]
    brackets = leftBracket ++ rightBracket

    maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar
    nonterminalsWithKRules = getNonterminalsWithKRelsAnyLong (Set.toList relations) maxNumberOfRules
    nonterminalsWithKRulesList = map show nonterminalsWithKRules
    triplets = calculateTriplets grammar (show maxNumberOfRules) nonterminalsWithKRulesList
    quads = calculateQuads $ calculateQuads' grammar triplets

    -- BLOCK FOR qRulektjs
    quadruplesInRulektjs' = concatMap (\s ->
      map (\(k,t,j,f) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ f, DSymbol s),
        (DebuggingTypes.L, DState $ q ++ rule ++ k ++ t ++ j ++ f))) quads) $ brackets ++ signs ++ terminalsList ++ negation

    quadruplesInRulektjs'' = map (\(k,t,j,s) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol t),
                                                        (DebuggingTypes.L, DState $ q ++ rule ++ k ++ t ++ j ++ s))) quads

    quadruplesInRulektjs = quadruplesInRulektjs' ++ quadruplesInRulektjs''

    -- BLOCK, which generates transitions to next subprograms - move to next rule, folding conj with sign, changing words

    -- fold and put +
    lastConjsWithoutNeg = filter (\t -> let
        hasNeg = checkIfConjHasNeg grammar t
        pair = constructSymbolsPairByQuad t hasNeg
        in isNothing (calculateNextConjunctionInSameRule grammar pair) && not hasNeg) quads
    quadruplesToRewritePlus = map (\(k,t,j,s) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol k),
                    (DebuggingTypes.L, DState $ q ++ rewrite ++ plus))) lastConjsWithoutNeg

    -- move to next conjunction
    midConjsWihoutNeg = filter (\t -> let
        hasNeg = checkIfConjHasNeg grammar t
        pair = constructSymbolsPairByQuad t hasNeg
        in isJust (calculateNextConjunctionInSameRule grammar pair) && not hasNeg) quads
    quadruplesToNextConj = map (\(k,t,j,s) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol k),
                    (DebuggingTypes.R, DState $ q ++ rule ++ k ++ t ++ j ++ s ++ skipParentNonterminal))) midConjsWihoutNeg

    -- move to next relation
    conjsInMidRuleWithNeg = filter (\t -> let
        hasNeg = checkIfConjHasNeg grammar t
        pair = constructSymbolsPairByQuad t hasNeg
        in isJust (calculateFirstConjunctionInNextRule grammar pair) && hasNeg) quads
    quadruplesToNextRel = concatMap (\(k,t,j,s) -> let
        inc = DSymbol (show $ (read k :: Int) + 1)
        qSkipParentNonterminalTransition = q ++ skipParentNonterminal ++ transition
        in [((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol k), (D inc, DState qSkipParentNonterminalTransition)),
        ((DState qSkipParentNonterminalTransition, inc),(DebuggingTypes.R, DState $ q ++ rewrite ++ plus))])
        conjsInMidRuleWithNeg

    -- fold and put -
    conjsInLastRuleWithNeg = filter (\t -> let
        hasNeg = checkIfConjHasNeg grammar t
        pair = constructSymbolsPairByQuad t hasNeg
        in isNothing (calculateFirstConjunctionInNextRule grammar pair) && hasNeg) quads
    quadruplesToRewriteMinus = map (\(k,t,j,s) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol k),
        (DebuggingTypes.L, DState $ q ++ rewrite ++ minus))) conjsInLastRuleWithNeg

    quadruples = quadruplesInRulektjs ++ quadruplesToRewritePlus ++ quadruplesToRewriteMinus
        ++ quadruplesToNextConj ++ quadruplesToNextRel 

    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

-- Block for figuring if conjunction has one of nonterminals with "-" sign and
-- it is possible to split a word into two in a different way for the same conjunction
generateBlockCheckIfWordsSplitCanBeChanged :: Grammar -> DebuggingQuadruples
generateBlockCheckIfWordsSplitCanBeChanged grammar@(Grammar (nonterminals, terminals, _, _)) = let
    qWordsChanging = "qWordsChanging"
    moveToBringSymbol = qWordsChanging ++ "MoveToBringSymbol"
    metFstNonterminal = qWordsChanging ++ "MetFstNonterminal"
    metSndNonterminal = qWordsChanging ++ "MetSndNonterminal"
    checkIfSndIsWord = qWordsChanging ++ "CheckIfSndIsWord"
    sndIsWord = qWordsChanging ++ "SndIsWord"
    returnToParentNonterminal = qWordsChanging ++ "ReturnToParentNonterminal"
    bringSymbol = qWordsChanging ++ "BringSymbol"
    failedPrefix = "Failed"
    negationWord = "Negation"
    failedCheckNegation = qWordsChanging ++ failedPrefix ++ "Check" ++ negationWord
    failedNegation = qWordsChanging ++ failedPrefix ++ negationWord
    failed = qWordsChanging ++ failedPrefix
    moveToStart = "MoveToStart"

    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    plus = "+"
    minus = "-"
    signs = [plus, minus]
    negation = ["!"]
    leftBracket = ["("]
    rightBracket = [")"]
    brackets = leftBracket ++ rightBracket
    maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [1..maxNumberOfRules]

    --BLOCK for qWordsChangingMoveToBringSymbol
    symbolsInMoveToBringSymbol = map
        (\t -> ((DState moveToBringSymbol, DSymbol t), (DebuggingTypes.R, DState moveToBringSymbol))) $ brackets ++ negation
    symbolsToMetFstNonterminalQdrs = map
        (\t -> ((DState moveToBringSymbol, DSymbol t), (DebuggingTypes.R, DState metFstNonterminal))) nonterminalsList

    --BLOCK for qWordsChangingMetFstNonterminal
    symbolsInMetFstNonterminal = map
        (\t -> ((DState metFstNonterminal, DSymbol t), (DebuggingTypes.R, DState metFstNonterminal)))
        $ brackets ++ signs ++ terminalsList
    symbolsToMetSndNonterminal = map
        (\t -> ((DState metFstNonterminal, DSymbol t), (DebuggingTypes.R, DState metSndNonterminal))) nonterminalsList

    --BLOCK for qWordsChangingMetSndNonterminal
    symbolsInMetSndNonterminal = map
        (\t -> ((DState metSndNonterminal, DSymbol t), (DebuggingTypes.R, DState metSndNonterminal))) signs
    symbolsToCheckIfSndIsWord = map
        (\t -> ((DState metSndNonterminal, DSymbol t), (DebuggingTypes.R, DState checkIfSndIsWord))) leftBracket

    --BLOCK for qWordsChangingCheckIfSndIsWord
    symbolsToSndIsWord = map
        (\t -> ((DState checkIfSndIsWord, DSymbol t), (DebuggingTypes.R, DState sndIsWord))) terminalsList

    --BLOCK for qWordsChangingSndIsWord
    symbolsToReturnToParentNonterminal = map
        (\t -> ((DState sndIsWord, DSymbol t), (DebuggingTypes.L, DState returnToParentNonterminal))) rightBracket
    symbolsToBringSymbol = map
        (\t -> ((DState sndIsWord, DSymbol t), (DebuggingTypes.L, DState bringSymbol))) terminalsList

    --BLOCK for qWordsChangingReturnToParentNonterminal
    notCounters = nonterminalsList ++ terminalsList ++ brackets ++ signs ++ negation
    symbolsInReturnToParentNonterminal = map
        (\t -> ((DState returnToParentNonterminal, DSymbol t),
        (DebuggingTypes.L, DState returnToParentNonterminal))) notCounters
    symbolsToWordsChangingFailed = map
        (\counter ->  ((DState returnToParentNonterminal, DSymbol counter),
        (DebuggingTypes.R, DState $ failedCheckNegation ++ counter))) indices

    --BLOCK for qWordsChangingFailedCheckNegationK
    symbolsInWordsChangingFailed = map
        (\counter -> ((DState $ failedCheckNegation ++ counter, DSymbol "("),
        (DebuggingTypes.R, DState $ failedCheckNegation ++ counter))) indices
    symbolsToWordsChangingFailedNegationK = map
        (\counter -> ((DState $ failedCheckNegation ++ counter, DSymbol "!"),
        (DebuggingTypes.L, DState $ failedNegation ++ counter))) indices
    symbolsToWordsChangingFailedK = concatMap
        (\counter -> map (\nonterm -> ((DState $ failedCheckNegation ++ counter, DSymbol nonterm),
        (DebuggingTypes.L, DState $ failed ++ counter))) nonterminalsList) indices

    --BLOCK for qWordsChangingFailedK
    symbolsToQkMoveToStart = map
        (\counter -> ((DState $ failed ++ counter, DSymbol "("),
        (DebuggingTypes.L, DState $ "q" ++ counter ++ moveToStart))) indices

    --BLOCK for qWordsChangingFailedNegationK
    symbolsToQkMoveToStartNegation = map
        (\counter -> ((DState $ failedNegation ++ counter, DSymbol "("),
        (DebuggingTypes.L, DState $ "q" ++ counter ++ moveToStart ++ negationWord))) indices

    quadruples = symbolsInMoveToBringSymbol ++ symbolsToMetFstNonterminalQdrs ++ symbolsInMetFstNonterminal
        ++ symbolsToMetSndNonterminal ++ symbolsInMetSndNonterminal ++ symbolsToCheckIfSndIsWord ++ symbolsToSndIsWord
        ++ symbolsToReturnToParentNonterminal ++ symbolsToBringSymbol ++ symbolsInReturnToParentNonterminal
        ++ symbolsToWordsChangingFailed ++ symbolsInWordsChangingFailed ++ symbolsToWordsChangingFailedNegationK
        ++ symbolsToWordsChangingFailedK ++ symbolsToQkMoveToStart ++ symbolsToQkMoveToStartNegation

    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

generateBlockForChangingWord :: Grammar -> DebuggingQuadruples
generateBlockForChangingWord (Grammar (nonterminals, terminals, _, _)) = let
    qWordsChanging = "qWordsChanging"
    bringSymbol = qWordsChanging ++ "BringSymbol"
    broughtSymbol = qWordsChanging ++ "BroughtSymbol"
    write = qWordsChanging ++ "Write"
    transition = "transition"
    writeBroughtSymbolt = qWordsChanging ++ "WriteBroughtSymbol"
    createCounterForFstNonterminal = qWordsChanging ++ "CreateCounterForFstNonterminal"
    moveToEnd = qWordsChanging ++ "moveToEnd"
    qFindNewSubstitution = "qFindNewSubstitution"
    fstCounter = "1"

    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    plus = "+"
    minus = "-"
    signs = [plus, minus]
    leftBracket = "("
    rightBracket = ")"
    brackets = [leftBracket, rightBracket]
    --BLOCK for qWordsChangingBringSymbol
    symbolsToBroughtSymbolt = concatMap (\t -> [
        ((DState bringSymbol, DSymbol t),(D $ DSymbol " ", DState $ bringSymbol ++ t ++ transition)),
        ((DState $ bringSymbol ++ t ++ transition, DSymbol t),(DebuggingTypes.L, DState $ broughtSymbol ++ t))
        ]) terminalsList
    --BLOCK for qWordsChangingBringSymbolt (list of states generated for each terminal t)
    symbolsInBroughtSymbolt = map (\t ->
        ((DState $ broughtSymbol ++ t, DSymbol " "),(DebuggingTypes.L, DState $ broughtSymbol ++ t))) terminalsList
    remembered = signs ++ nonterminalsList ++ brackets
    pairs = concatMap (\t -> map (t,) remembered) terminalsList
    symbolsToWritetk = concatMap (\(t, k) ->
        [((DState $ broughtSymbol ++ t, DSymbol k),(D $ DSymbol " ", DState $ write ++ t ++ k ++ transition)),
        ((DState $ write ++ t ++ k ++ transition, DSymbol " "),(DebuggingTypes.R, DState $ write ++ t ++ k))]) pairs
    --BLOCK for qWordsChangingWritetk
    remembered' = signs ++ nonterminalsList ++ [leftBracket]
    pairs' = concatMap (\t -> map (t,) remembered') terminalsList
    symbolsToBroughtSymbolt' = concatMap (\(t, k) ->
        [((DState $ write ++ t ++ k, DSymbol " "),(D $ DSymbol k, DState $ broughtSymbol ++ t ++ k ++ transition)),
        ((DState $ broughtSymbol ++ t ++ k ++ transition, DSymbol k),(DebuggingTypes.L, DState $ broughtSymbol ++ t))
        ]) pairs'
    -- special case for )
    symbolsToWriteBroughtSymbolt = concatMap (\t -> let
        oldState = write ++ t ++ rightBracket
        oldStateTransition = oldState ++ transition
        in
        [((DState oldState, DSymbol " "),(D $ DSymbol rightBracket, DState oldStateTransition)),
        ((DState oldStateTransition, DSymbol rightBracket),(DebuggingTypes.R, DState writeBroughtSymbolt))]) terminalsList

    --BLOCK for qWordsChangingWriteBroughtSymbolt
    symbolsToCreateCounterForFstNonterminal = concatMap (\t ->
        let transitionState = writeBroughtSymbolt ++ transition in
        [((DState writeBroughtSymbolt, DSymbol " "),(D $ DSymbol t, DState transitionState)),
        ((DState transitionState, DSymbol t),(DebuggingTypes.L, DState createCounterForFstNonterminal))]) terminalsList

    --BLOCK for qWordsChangingCreateCounterForFstNonterminal
    symbolsInCreateCounterForFstNonterminal = map (\t ->
        ((DState createCounterForFstNonterminal, DSymbol t),(DebuggingTypes.L, DState createCounterForFstNonterminal)))
        $ leftBracket : terminalsList
    symbolsToMoveToEnd = concatMap (\t -> let
        transitionState = createCounterForFstNonterminal ++ transition in
        [((DState createCounterForFstNonterminal, DSymbol t),(D $ DSymbol fstCounter, DState transitionState)),
        ((DState transitionState, DSymbol fstCounter),(DebuggingTypes.R, DState moveToEnd))
        ]) signs

    --BLOCK for qWordsChangingMoveToEnd
    symbols = fstCounter  : brackets ++ terminalsList ++ nonterminalsList 
    symbolsInMoveToEnd = map (\t -> ((DState moveToEnd, DSymbol t),(DebuggingTypes.R, DState moveToEnd))) symbols
    symbolsToQFindNewSubstitution = map (\t -> 
        ((DState moveToEnd, DSymbol t),(DebuggingTypes.L, DState qFindNewSubstitution))) $ " " : signs 

    quadruples = symbolsToBroughtSymbolt ++ symbolsInBroughtSymbolt ++ symbolsToWritetk
        ++ symbolsToBroughtSymbolt' ++ symbolsToWriteBroughtSymbolt ++ symbolsToCreateCounterForFstNonterminal
        ++ symbolsInCreateCounterForFstNonterminal ++ symbolsToMoveToEnd ++ symbolsInMoveToEnd
        ++ symbolsToQFindNewSubstitution

    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

generateBlockForFolding :: Grammar -> DebuggingQuadruples
generateBlockForFolding grammar@(Grammar (nonterminals, terminals, _,_)) = let
    qRewrite = "qRewrite"
    withMinus = "withMinus"
    qFindNewSubstitution = "qFindNewSubstitution"
    transition = "transition"
    qSkipParentNonterminal = "qSkipParentNonterminal"
    qRemoveSymbols = "qRemoveSymbols"
    qRemoveBracketsAroundFstWord = "qRemoveBracketsAroundFstWord"
    qRemoveBracketsAroundSndWord = "qRemoveBracketsAroundSndWord"
    qMoveToStart = "qMoveToStart"
    qFold = "qFold"
    qCheckLast = "qCheckLastRightBracket"
    lookForNewPlace = "LookForNewPlace"
    qFoldRightBracketLast' = "qFoldRightBracketLast'"
    qFoldRightBracketLast = "qFoldRightBracketLast"
    qFoldRightBracket = "qFoldRightBracket"
    qFoldRightBracket' = "qFoldRightBracket'"
    write = "write"
    minus = "-"
    plus = "+"
    signs = [minus, plus]
    negation = "!"
    leftBracket = "("
    rightBracket = ")"
    brackets = [leftBracket, rightBracket]
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals

  --BLOCK for qRewriteNWithWord (N - nonterminal)
  -- case, when current rule has one terminal in right part: applying this rule to word with more
  -- than 1 symbol is impossible
    nonterminalsWithIndices = Map.toList $ getNumbersOfShortRelations grammar
    symbolsToQFindNewSubstitution = concatMap (\(Nonterminal nonterm, indices) -> concatMap (\i -> let
        state = qRewrite ++ nonterm ++ withMinus
        stateTransition = state ++ transition
        in
        [((DState state, DSymbol i),(D $ DSymbol minus, DState stateTransition)),
         ((DState stateTransition, DSymbol i),(DebuggingTypes.L, DState qFindNewSubstitution))]) indices) nonterminalsWithIndices

    --BLOCK for qRewriteMinus or qRewritePlus
    pairs = concatMap (\sign -> map (sign,) nonterminalsList) signs
    symbolsToQRewriteNSign = map (\(sign, nonterm) -> let
        newState =  qRewrite ++ nonterm ++ sign in
        ((DState $ qRewrite ++ sign, DSymbol nonterm),(DebuggingTypes.R, DState newState))) pairs

    --BLOCK for qRewriteNSign
    maxNumber = calculateMaxNumberOfRulesForNonterminal grammar
    possibleIndices = map show [1..maxNumber]
    symbolsToQSkipParentNonterminal = concatMap (\(sign, nonterm) -> let
        state = qRewrite ++ nonterm ++ sign
        stateTransition = state ++ transition in
        concatMap (\index ->
            [((DState state, DSymbol index),(D $ DSymbol sign, DState stateTransition)),
             ((DState stateTransition, DSymbol index),(DebuggingTypes.R, DState qSkipParentNonterminal))]) possibleIndices) pairs

    --BLOCK for qSkipParentNonterminal
    symbolsToQRemoveSymbols = [((DState qSkipParentNonterminal, DSymbol leftBracket),(DebuggingTypes.R, DState qRemoveSymbols))]

    --BLOCK for qRemoveSymbols
    symbolsInQRemoveSymbols = concatMap (\t -> let
        stateTransition = qRemoveSymbols ++ transition in
        [((DState qRemoveSymbols, DSymbol t),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTypes.R, DState qRemoveSymbols))]) $ nonterminalsList ++ [negation]
    symbolsToQRemoveBracketAroundFstWord = concatMap (\sign -> let
        -- FIXME: add uniform way for naming transition : oldStateName ++ transition ++ newStateName
        stateTransition = qRemoveSymbols ++ transition ++ qRemoveBracketsAroundFstWord in
        [((DState qRemoveSymbols, DSymbol sign),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTypes.R, DState qRemoveBracketsAroundFstWord))]) signs

    --BLOCK for qRemoveBracketsAroundFstWord
    symbolsInQRemoveBracketsAroundFstWord' = let
        stateTransition = qRemoveBracketsAroundFstWord ++ transition ++ qRemoveBracketsAroundFstWord
        in [((DState qRemoveBracketsAroundFstWord, DSymbol leftBracket),(D $ DSymbol " ", DState stateTransition)),
            ((DState stateTransition, DSymbol " " ),(DebuggingTypes.R, DState qRemoveBracketsAroundFstWord))]
    symbolsInQRemoveBracketsAroundFstWord'' = map (\t ->
            ((DState qRemoveBracketsAroundFstWord, DSymbol t),(DebuggingTypes.R, DState qRemoveBracketsAroundFstWord))) terminalsList
    symbolsInQRemoveBracketsAroundFstWord = symbolsInQRemoveBracketsAroundFstWord' ++ symbolsInQRemoveBracketsAroundFstWord''
    symbolsToQRemoveBracketsAroundSndWord = let
        stateTransition = qRemoveBracketsAroundFstWord ++ transition ++ qRemoveBracketsAroundSndWord
        in [((DState qRemoveBracketsAroundFstWord, DSymbol rightBracket),(D $ DSymbol " ", DState stateTransition)),
            ((DState stateTransition, DSymbol " " ),(DebuggingTypes.R, DState qRemoveBracketsAroundSndWord))]

    --BLOCK for qRemoveBracketsAroundSndWord
    symbolsInQRemoveBracketsAroundSndWord' = concatMap (\t -> let
        stateTransition = qRemoveBracketsAroundSndWord ++ transition ++ qRemoveBracketsAroundSndWord
        in [((DState qRemoveBracketsAroundSndWord, DSymbol t),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTypes.R, DState qRemoveBracketsAroundSndWord))])
        $ terminalsList ++ signs ++ [leftBracket]
    symbolsInQRemoveBracketsAroundSndWord'' = map (\t ->
        ((DState qRemoveBracketsAroundSndWord, DSymbol t),(DebuggingTypes.R, DState qRemoveBracketsAroundSndWord))) terminalsList
    symbolsInQRemoveBracketsAroundSndWord = symbolsInQRemoveBracketsAroundSndWord' ++ symbolsInQRemoveBracketsAroundSndWord''
    symbolsToQMoveToStart = let
        stateTransition = qRemoveBracketsAroundSndWord ++ transition ++ qMoveToStart
        in [((DState qRemoveBracketsAroundSndWord, DSymbol rightBracket),(D $ DSymbol " ", DState stateTransition)),
            ((DState stateTransition, DSymbol " "),(DebuggingTypes.L, DState qMoveToStart))]

    --BLOCK for qMoveToStart
    symbolsInQMoveToStart = map (\t -> ((DState qMoveToStart, DSymbol t),(DebuggingTypes.L, DState qMoveToStart)))
        $ terminalsList ++ [" "]
    symbolsToQFold = [((DState qMoveToStart, DSymbol leftBracket),(DebuggingTypes.R, DState qFold))]

    --BLOCK for qFold
    symbolsInQFold = [((DState qFold, DSymbol " "),(DebuggingTypes.R, DState qFold))]
    symbolsToQFoldSLookForNewPlace = concatMap (\t -> let
        newState = qFold ++ t ++ lookForNewPlace
        stateTransition = qFold ++ transition ++ newState
        in
        [((DState qFold, DSymbol t),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTypes.L, DState newState))])
        $ terminalsList ++ nonterminalsList ++ brackets ++ signs

    --BLOCK for qFoldSLookForNewPlace - it common for all symbols except ")"
    symbols = terminalsList ++ nonterminalsList ++ [leftBracket] ++ signs
    symbolsInQFoldSLookForNewPlace = map (\t -> let
        state = qFold ++ t ++ lookForNewPlace in
        ((DState state, DSymbol " "),(DebuggingTypes.L, DState state))) symbols

    -- FIXME think about creating one function with parameter possibleSymbols
    --BLOCK for qFoldNonterminalLookForNewPlace
    symbolsToQFoldNonterminalWrite = map (\t -> let
        oldState = qFold ++ t ++ lookForNewPlace
        newState = qFold ++ t ++ write in
        ((DState oldState, DSymbol rightBracket),(DebuggingTypes.R, DState newState))) nonterminalsList

    --BLOCK for qFoldTerminalLookForNewPlace
    symbolsToQFoldTerminalWrite = concatMap (\t -> let
        oldState = qFold ++ t ++ lookForNewPlace
        newState = qFold ++ t ++ write
        possibleSymbols = terminalsList ++ [leftBracket] in
        map (\s -> ((DState oldState, DSymbol s),(DebuggingTypes.R, DState newState))) possibleSymbols) terminalsList

    --BLOCK for qFoldSignLookForNewPlace
    symbolsToQFoldSignWrite = concatMap (\t -> let
        oldState = qFold ++ t ++ lookForNewPlace
        newState = qFold ++ t ++ write
        possibleSymbols = nonterminalsList in
        map (\s -> ((DState oldState, DSymbol s),(DebuggingTypes.R, DState newState))) possibleSymbols) signs

    --BLOCK for qFold(LookForNewPlace
    symbolsToQFoldLeftBracketWrite = concatMap (\t -> let
        oldState = qFold ++ t ++ lookForNewPlace
        newState = qFold ++ t ++ write
        possibleSymbols = signs in
        map (\s -> ((DState oldState, DSymbol s),(DebuggingTypes.R, DState newState))) possibleSymbols) [leftBracket]


    --BLOCK for qFold)LookForNewPlace
    qFoldRightBracketLookForNewPlace = qFold ++ rightBracket ++ lookForNewPlace
    symbolsInQFoldRightBracketLookForNewPlace =
        [((DState qFoldRightBracketLookForNewPlace, DSymbol " "),(DebuggingTypes.R, DState qFoldRightBracketLookForNewPlace))]
    symbolsToQCheckLastRightBracket = let
        stateTransition = qFoldRightBracketLookForNewPlace ++ transition ++ qCheckLast
        in
        [((DState qFoldRightBracketLookForNewPlace, DSymbol rightBracket),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTypes.R, DState qCheckLast))]

    --BLOCK for qCheckLast)
    symbolsToQFoldRightBracketLast' = [((DState qCheckLast, DSymbol " "),(DebuggingTypes.L, DState qFoldRightBracketLast'))]
    symbolsToQFoldRightBracket' = map (\t ->
        ((DState qCheckLast, DSymbol t),(DebuggingTypes.L, DState qFoldRightBracket'))) $ nonterminalsList ++ [rightBracket]

    --BLOCK for case, when bracket is not last
    symbolsInQFoldRightBracket' = [((DState qFoldRightBracket', DSymbol " "),(DebuggingTypes.L, DState qFoldRightBracket'))]
    symbolsToQFoldRightBracket = map (\t ->
        ((DState qFoldRightBracket', DSymbol t),(DebuggingTypes.R, DState qFoldRightBracket))) $ terminalsList ++ [rightBracket]
    symbolsFromRightBracketToQFold = let
        stateTransition = qFoldRightBracket' ++ transition ++ qFold in
        [((DState qFoldRightBracket', DSymbol " "),(D $ DSymbol rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol rightBracket),(DebuggingTypes.R, DState qFold))]

    --BLOCK for case, when bracket is last
    symbolsInQFoldRightBracketLast' =
      [((DState qFoldRightBracketLast', DSymbol " "),(DebuggingTypes.L, DState qFoldRightBracketLast'))]
    symbolsToQFoldRightBracketLast = map (\t ->
        ((DState qFoldRightBracketLast', DSymbol t),(DebuggingTypes.R, DState qFoldRightBracketLast)))
        $ terminalsList ++ [rightBracket]
    symbolsFromRightBracketLastToQFold = let
            stateTransition = qFoldRightBracketLast ++ transition ++ qFindNewSubstitution in
            [((DState qFoldRightBracketLast, DSymbol " "),(D $ DSymbol rightBracket, DState stateTransition)),
            ((DState stateTransition, DSymbol rightBracket),(DebuggingTypes.R, DState qFindNewSubstitution))]

    quadruples = symbolsToQFindNewSubstitution ++ symbolsToQRewriteNSign ++ symbolsToQSkipParentNonterminal
        ++ symbolsInQRemoveSymbols ++ symbolsToQRemoveSymbols ++ symbolsToQRemoveBracketAroundFstWord
        ++ symbolsInQRemoveBracketsAroundFstWord ++ symbolsToQRemoveBracketsAroundSndWord ++ symbolsInQRemoveBracketsAroundSndWord
        ++ symbolsToQMoveToStart ++ symbolsInQMoveToStart ++ symbolsToQFold ++ symbolsInQFold ++ symbolsToQFoldSLookForNewPlace
        ++ symbolsInQFoldSLookForNewPlace ++ symbolsToQFoldNonterminalWrite ++ symbolsToQFoldTerminalWrite
        ++ symbolsToQFoldSignWrite ++ symbolsToQFoldLeftBracketWrite ++ symbolsInQFoldRightBracketLookForNewPlace
        ++ symbolsToQCheckLastRightBracket ++ symbolsToQFoldRightBracketLast' ++ symbolsToQFoldRightBracket'
        ++ symbolsInQFoldRightBracket' ++ symbolsToQFoldRightBracket ++ symbolsInQFoldRightBracketLast'
        ++ symbolsToQFoldRightBracketLast ++ symbolsFromRightBracketToQFold ++ symbolsFromRightBracketLastToQFold
    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

-- short relation is relation with one terminal in right part
getNumbersOfShortRelations :: Grammar -> Map.Map Nonterminal [String]
getNumbersOfShortRelations (Grammar (_, _, relations, _)) =
    Map.mapWithKey f $ calculateGroupRelationsByNonterminals $ Set.toList relations

f :: Nonterminal -> [[GrammarType.Symbol]] -> [String]
f nonterminal rightParts = let
    shortOrNotRels = map (\t -> relationHasOneTerminalInRightPart (Relation (nonterminal, t))) rightParts
    indices' = map (\t -> if t then elemIndex t shortOrNotRels else Nothing) shortOrNotRels
    indices = map (\t -> show t) $ catMaybes indices'
    in indices

generateBlockForCountWordLength :: Grammar -> DebuggingQuadruples
generateBlockForCountWordLength grammar@(Grammar (nonterminals, terminals, relations, _)) = let
    qWriteStartCounter = "qWriteStartCounter"
    qCountWordLength = "qCountWordLength"
    qCheckSymbol = "qCheckSymbol"
    qSymbol = "qSymbol"
    qWord = "qWord"
    qStart ="qStart"
    qChooseRelation = "qChooseRelation"
    transition = "transition"
    one = "1"
    leftBracket = "("
    rightBracket = ")"
    maxNumber = calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [1..maxNumber]
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals

    symbolsToQWriteStartCounterK = concatMap (\t -> let
        stateTransition = qWriteStartCounter ++ transition ++ qWriteStartCounter ++ t in
        [((DState qWriteStartCounter, DSymbol t),(D $ DSymbol one, DState stateTransition)),
        ((DState stateTransition, DSymbol one),(DebuggingTypes.L, DState $ qWriteStartCounter ++ t))
        ]) nonterminalsList

    --BLOCK for qWriteStartCounterK
    symbolsToQCountWordLength = concatMap (\t -> let
        stateTransition = qWriteStartCounter ++ transition ++ qCountWordLength in
        [((DState $ qWriteStartCounter ++ t, DSymbol " "),(D $ DSymbol t, DState stateTransition)),
        ((DState stateTransition, DSymbol t),(DebuggingTypes.R, DState qCountWordLength))
        ]) nonterminalsList

    --BLOCK for qCountWordLength
    symbolsInQCountWordLength = map (\t ->
        ((DState qCountWordLength, DSymbol t),(DebuggingTypes.R, DState qCountWordLength)))
        $ indices ++ nonterminalsList ++ [leftBracket]
    symbolsToQCheckSymbol = map (\t ->
        ((DState qCountWordLength, DSymbol t),(DebuggingTypes.R, DState qCheckSymbol))) terminalsList

    --BLOCK for qCheckSymbol
    symbolsToQSymbol = map (\t ->
        ((DState qCheckSymbol, DSymbol t),(DebuggingTypes.L, DState qSymbol))) [rightBracket]
    symbolsToQWord = map (\t ->
            ((DState qCheckSymbol, DSymbol t),(DebuggingTypes.L, DState qWord))) terminalsList

    --BLOCK for qSymbol
    symbolsInQSymbol = map (\t ->
        ((DState qSymbol, DSymbol t),(DebuggingTypes.L, DState qSymbol))) $ terminalsList ++ [leftBracket]
    symbolsToQStart = map (\t ->
        ((DState qSymbol, DSymbol t),(DebuggingTypes.L, DState qStart))) indices
    
    --BLOCK for qStart
    -- necessary to implement boolean function for figuring if current symbol is accepted by current nonterminal
    -- (if there is relation N -> t for N - nonterminal, t - nonterminal)
       
    --BLOCK for qWord
    symbolsInQWord = map (\t ->
        ((DState qWord, DSymbol t),(DebuggingTypes.L, DState qWord))) $ terminalsList ++ [leftBracket]
    symbolsToQChooseRelationI = map (\t ->
        ((DState qWord, DSymbol t),(DebuggingTypes.L, DState $ qChooseRelation ++ t))) indices
    
    --BLOCK for qChooseRelationI
    pairs = map (\k -> 
        (k, (Map.keys $ Map.filter (\t -> length t >= (read k :: Int)) $ calculateGroupRelationsByNonterminals $ Set.toList relations))) indices
    
        --in (Map.keys $ Map.filter (\t -> length t >= k) groupedRelations)
    {---symbolsToQChooseRelationI = map (\t ->
        ((DState qWord, DSymbol t),(DebuggingTypes.L, DState $ qChooseRelation ++ t))) indices--}
            

    quadruples = symbolsToQWriteStartCounterK ++ symbolsToQCountWordLength ++ symbolsInQCountWordLength
        ++ symbolsToQCheckSymbol ++ symbolsToQSymbol ++ symbolsToQWord ++ symbolsInQSymbol
        ++ symbolsToQStart ++ symbolsInQWord ++ symbolsToQChooseRelationI
    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

constructSymbolsPairByQuad :: (String, String, String, String) -> Bool -> SymbolsPair
constructSymbolsPairByQuad (number, leftN, fstN, sndN) hasNeg =
    SymbolsPair (Nonterminal leftN, read number :: Int, hasNeg, N $ Nonterminal fstN, N $ Nonterminal sndN)

checkIfConjHasNeg :: Grammar -> (String, String, String, String) -> Bool
checkIfConjHasNeg (Grammar(_, _, relations, _)) (number, leftN, fstN, sndN) = do
    let listRelations = Set.toList relations
    let groupedRelations = calculateGroupRelationsByNonterminals listRelations
    let relationsForNonterminal = groupedRelations Map.! Nonterminal leftN
    let relation = relationsForNonterminal !! (read number :: Int)
    let conjunctionsPair = splitOn [O Conjunction] relation
    let index = elemIndex [O Negation, N (Nonterminal fstN), N (Nonterminal sndN)] conjunctionsPair
    case index of
        Just _ -> True
        Nothing -> False

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
        else let
            relation = relationsForNonterminal !! nextRelationNumber
            conjunctionPairs = splitOn [O Conjunction] relation
        in Just $ convertListToConjunctionPair nonterminal nextRelationNumber $ head conjunctionPairs


