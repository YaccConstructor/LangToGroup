{-# LANGUAGE TupleSections #-}

module Boolean2TM where

import GrammarType
import TMTypes
import DebuggingTMTypes
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Data.List.Split (splitOn)
import Data.Maybe

newtype SymbolsPair = SymbolsPair (Nonterminal, Int, Bool, GrammarType.Symbol, GrammarType.Symbol)
    deriving (Eq, Show)

boolean2tm :: Grammar -> DebuggingTuringMachine
boolean2tm grammar = let
        quadruples1 = generateBlockForFolding grammar
        quadruples2 = generateBlockForMovingToNextConjunction grammar
        quadruples3 = generateBlockForSubstitution grammar
        quadruples4 = generateBlockForCountWordLength grammar
        quadruples5 = generateBlockForWritingSigns grammar
        quadruples7 = generateBlockForScanResults grammar
        quadruples8 = generateBlockForPreparingForSubstitution grammar
        quadruples9 = generateBlockForFolding grammar
        quadruples10 = generateBlockForFindingNewSubstitution grammar
        quadruples11 = generateBlockForRefiningConjunctionDetails grammar in
        (DTM $ unionQuadruples [quadruples1, quadruples2,
        quadruples3,quadruples4,quadruples5, quadruples7,quadruples8,
        quadruples9,quadruples10,quadruples11])

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
  qFold' = "qFold'"

  terminalsList = Set.toList terminals
  nonterminalsList = Set.toList nonterminals
  signsList = ["+", "-"]
  negation = ["!"]
  indices= map show [0..maxNumberOfRules - 1]

  maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar

  -- BLOCK for qFindNewSubstitution
  list = concat [map show indices, map terminalValue terminalsList, signsList, negation]
  -- there is no necessity in insertWithKey, since TM is deterministic
  symbolsInQFindNewSubstitutionQdrs = map (\t -> ((DState qFindNewSubstitution, DSymbol t),(DebuggingTMTypes.L, DState qFindNewSubstitution))) list
  -- in TMTypes EmptySymbol is 0
  symbolsFromQFindNewSubstitutionToAcceptQdrs = [((DState qFindNewSubstitution, DSymbol " "),(DebuggingTMTypes.R, finalDState))]
  symbolsWhichChangeStateQdrs =
    map ((\t -> ((DState qFindNewSubstitution, DSymbol t), (DebuggingTMTypes.R, DState qCheckIfNotCompleted))) . nonterminalValue) nonterminalsList

  -- BLOCK for qCheckIfNotCompleted
  symbolsToQSubstituteOrFoldQdrs = map 
    ((\t -> ((DState qCheckIfNotCompleted, DSymbol t),(DebuggingTMTypes.R, DState qSubstituteOrFold))) . show) indices
  symbolsToQSkipCompletedNonterminalQdrs = map 
    (\t -> ((DState qCheckIfNotCompleted, DSymbol t),(DebuggingTMTypes.L, DState qSkipCompletedNonterminal))) signsList
  symbolsFromQCheckIfNotCompletedToAcceptQdrs = [((DState qCheckIfNotCompleted, DSymbol " "),(DebuggingTMTypes.R, finalDState))]

  -- BLOCK for qSubstituteOrFold
  symbolsInQSubstituteOrFoldQdrs = [((DState qSubstituteOrFold, DSymbol "("),(DebuggingTMTypes.R, finalDState))]
  symbolsToCountWordLength'Qdrs = map
    ((\t -> ((DState qSubstituteOrFold, DSymbol t),(DebuggingTMTypes.L, DState qCountWordLength'))) . terminalValue) terminalsList
  symbolsToQFold'Qdrs = map 
    (\t -> ((DState qSubstituteOrFold, DSymbol t),(DebuggingTMTypes.L, DState qCountWordLength'))) $ map nonterminalValue nonterminalsList ++ negation

  -- BLOCK for qCountWordLength'
  symbolsToCountWordLengthQdrs = [((DState qCountWordLength', DSymbol "("),(DebuggingTMTypes.L, DState qCountWordLength))]

  -- BLOCK for qFold'
  symbolsToQMoveToEndToScanResults = [((DState qFold', DSymbol "("),(DebuggingTMTypes.L, DState qMoveToEndToScanResults))]

  -- BLOCK for qSkipCompletedNonterminal
  symbolsToQFindNewSubstitutionQdrs = map 
    ((\t -> ((DState qSkipCompletedNonterminal, DSymbol t),(DebuggingTMTypes.L, DState qFindNewSubstitution))) . nonterminalValue) nonterminalsList

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
    indices = map show [0..maxNumberOfRules - 1]
    allSymbols = terminalsList ++ nonterminalsList ++ signs ++ indices ++ brackets ++ negation

    symbolsInQMoveToEndAndScanResultQdrs = map
        (\t -> ((DState qMoveToEndAndScanResult, DSymbol t),(DebuggingTMTypes.R, DState qMoveToEndAndScanResult))) allSymbols
    symbolsToQStart'Qdrs = [((DState qMoveToEndAndScanResult, DSymbol " "),(DebuggingTMTypes.L, DState qStart'))]

    -- BLOCK for qStart'
    symbolsInQStart'Qdrs = map
        (\t -> ((DState qStart', DSymbol t),(DebuggingTMTypes.L, DState qStart'))) $ brackets ++ terminalsList
    symbolsToQSecPosQdrs = [((DState qStart', DSymbol "+"),(DebuggingTMTypes.L, DState qSecPos))]
    symbolsToQSecNegQdrs = [((DState qStart', DSymbol "-"),(DebuggingTMTypes.L, DState qSecNeg))]

    -- BLOCK for qSecPos
    symbolsInQSecPosQdrs = map
        (\t -> ((DState qSecPos, DSymbol t),(DebuggingTMTypes.L, DState qSecPos))) $ brackets ++ terminalsList ++ nonterminalsList
    symbolsToQBothPosQdrs = [((DState qSecPos, DSymbol "+"),(DebuggingTMTypes.L, DState qBothPos))]
    symbolsFromQSecPosToQSomeNegQdrs = [((DState qSecPos, DSymbol "-"),(DebuggingTMTypes.L, DState qSomeNeg))]

    -- BLOCK for qSecNeg: there is possibility to remove this state and move to qSomeNeg state
    symbolsInQSecNegQdrs = map
        (\t -> ((DState qSecNeg, DSymbol t),(DebuggingTMTypes.L, DState qSecNeg))) $ brackets ++ terminalsList ++ nonterminalsList
    symbolsFromQSecNegToQSomeNegQdrs =
      [((DState qSecNeg, DSymbol "+"),(DebuggingTMTypes.L, DState qSomeNeg)),((DState qSecNeg, DSymbol "-"),(DebuggingTMTypes.L, DState qSomeNeg))]

    -- BLOCK for qBothPos
    symbolsInQBothPosQdrs = map
        (\t -> ((DState qBothPos, DSymbol t),(DebuggingTMTypes.L, DState qBothPos))) $ rightBracket ++ negation ++ nonterminalsList
    symbolsToQ_ithFindNegationQdrs = map
        (\t -> ((DState qBothPos, DSymbol t),(DebuggingTMTypes.L, DState $ q ++ t ++ findNegation))) indices

    -- BLOCK for qSomeNeg
    symbolsInSomeNegQdrs = map
        (\t -> ((DState qSomeNeg, DSymbol t),(DebuggingTMTypes.L, DState qSomeNeg))) $ rightBracket ++ negation ++ nonterminalsList
    symbolsToQWordsChangingMoveToBringSymbolQdrs = map
        (\t -> ((DState qSomeNeg, DSymbol t),(DebuggingTMTypes.R, DState qWordsChangingMoveToBringSymbol))) indices

    quadruples = symbolsInQMoveToEndAndScanResultQdrs ++ symbolsToQStart'Qdrs ++ symbolsInQStart'Qdrs
        ++ symbolsToQSecPosQdrs ++ symbolsToQSecNegQdrs ++ symbolsInQSecPosQdrs ++ symbolsToQBothPosQdrs
        ++ symbolsFromQSecPosToQSomeNegQdrs ++ symbolsInQSecNegQdrs ++ symbolsFromQSecNegToQSomeNegQdrs
        ++ symbolsInQBothPosQdrs ++ symbolsToQ_ithFindNegationQdrs ++ symbolsInSomeNegQdrs
        ++ symbolsToQWordsChangingMoveToBringSymbolQdrs

    in DQuadruples (addCollectionToMap quadruples Map.empty)

generateBlockForRefiningConjunctionDetails :: Grammar -> DebuggingQuadruples
generateBlockForRefiningConjunctionDetails grammar = let
    maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [0..maxNumberOfRules - 1]
    quadruplesList = map (generateBlockForQKFindNegation grammar) indices
    in unionQuadruples quadruplesList

unionQuadruples :: [DebuggingQuadruples] -> DebuggingQuadruples
unionQuadruples [quadruple] = quadruple
unionQuadruples quadruples = DQuadruples (Map.unions $ map (\(DQuadruples x) -> x) quadruples)

generateBlockForQKFindNegation :: Grammar -> String -> DebuggingQuadruples
generateBlockForQKFindNegation grammar@(Grammar (nonterminals, terminals, relations, _)) k' = let
    -- parts for set of states q1FindNegation, q2FindNegation...qkFindNegation
    q = "q"
    qkFindNegation = q ++ k' ++ "FindNegation"
    qkMoveToStart = q ++ k' ++ "MoveToStart"
    qkMoveToStartNeg = q ++ k' ++ "MoveToStartNegation"
    qRuleKFindNonterminal = q ++ "Rule" ++ k' ++ "KFindNonterminal"

    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    relsList = Set.toList relations
    signs = ["+", "-"]
    negation = ["!"]
    leftBracket = ["("]
    rightBracket = [")"]
    brackets = leftBracket ++ rightBracket

    -- BLOCK for qkFindNegation
    symbolsInQkFindNegation = [((DState qkFindNegation, DSymbol "("),(DebuggingTMTypes.R, DState qkFindNegation))]
    symbolsToQkMoveToStart = map
        (\t -> ((DState qkFindNegation, DSymbol t),(DebuggingTMTypes.L, DState qkMoveToStart))) nonterminalsList
    symbolsToQkMoveToStartNegation = [((DState qkFindNegation, DSymbol "!"),(DebuggingTMTypes.L, DState qkMoveToStartNeg))]

    -- BLOCK for qkMoveToStart/qkMoveToStartNeg
    symbolsInQkMoveToStart = [((DState qkMoveToStart, DSymbol "("),(DebuggingTMTypes.L, DState qkMoveToStart))]
    symbolsToQRulekFindNonterminal = [((DState qkMoveToStart, DSymbol k'),(DebuggingTMTypes.L, DState qRuleKFindNonterminal))]

    -- BLOCK for qRulekFindNonterminal

    --nonterminalsWithKRels = map nonterminalValue (getNonterminalsWithKRelsAnyLong (Set.toList relations) (read k :: Int))
    nonterminalsWithKthRel = filter (\t -> kthRelForNonterminalLong relsList t k') nonterminalsList
    symbolsToQRulekNonterminalFindFst = map (\t ->
            ((DState qRuleKFindNonterminal, DSymbol t),(DebuggingTMTypes.R, DState $ q ++ "Rule" ++ k' ++ t ++ "findFst"))
            ) nonterminalsWithKthRel

    -- BLOCK for qRulektFindFst
    symbolsInQRulektFindFst = map (\t ->
            ((DState $ q ++ "Rule" ++ k' ++ t ++ "findFst", DSymbol t),
            (DebuggingTMTypes.R, DState $ q ++ "Rule" ++ k' ++ t ++ "findFst"))
            ) $ [k'] ++ rightBracket ++ negation

    triplets = calculateTriplets grammar k' nonterminalsWithKthRel

    symbolsToQRulektjFindSnd = map
        (\(k, t, j) -> ((DState $ q ++ "Rule" ++ k ++ t ++ "findFst", DSymbol j),
            (DebuggingTMTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd"))) triplets

    -- BLOCK for qRulektjFindSnd
    quads = calculateQuads grammar k' nonterminalsWithKthRel
    symbolsInRulektjFindSnd = concatMap (\s ->
        map (\(k, t, j) ->
            ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol s),
             (DebuggingTMTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd"))
            ) triplets) $ terminalsList ++ signs ++ brackets

    {--symbolsToQRulektjs = concatMap (\(k,t,j,s) -> map (\f -> ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol f),
        (DebuggingTMTypes.L, DState $ q ++ "Rule" ++ k ++ t ++ j ++ f))) s) quads --}
    symbolsToQRulektjs = map (\(k,t,j,f) -> ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol f),
            (DebuggingTMTypes.L, DState $ q ++ "Rule" ++ k ++ t ++ j ++ f))) quads

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

calculateQuads' :: Grammar -> [([Char], [Char], [Char])] -> [(String, String, String, [String])]
calculateQuads' grammar = map (\ (k, t, j) -> (k, t, j, getSecondNonterminalsInConjunctionsOfGivenRelation grammar k t j))

calculateQuads :: Grammar -> String -> [String] -> [(String, String, String, String)]
calculateQuads grammar k' nonterminalsWithKthRel = let
    triplets = calculateTriplets grammar k' nonterminalsWithKthRel
    quads' = calculateQuads' grammar triplets
    in concatMap (\ (k, t, j, s) -> map (k, t, j,) s) quads'

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
generateTransitionFromConjunctionResult grammar@(Grammar (nonterminals, terminals, relations, _)) = let
    q = "q"
    rewrite = "Rewrite"
    transition = "Transition"
    skipParentNonterminal = "SkipParentNonterminal"
    rule = "Rule"
    terminalsList = map terminalValue $ Set.toList terminals
    plus = "+"
    minus = "-"
    signs = [plus, minus]
    negation = ["!"]
    leftBracket = ["("]
    rightBracket = [")"]
    brackets = leftBracket ++ rightBracket
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    relsList = Set.toList relations

    maxNumberOfRules = calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [0..maxNumberOfRules - 1]

    indicesWithNonterms = map (\i ->
            (i, filter (\t -> kthRelForNonterminalLong relsList t i) nonterminalsList)) indices
    quads = concatMap (uncurry $ calculateQuads grammar) indicesWithNonterms

    -- BLOCK FOR qRulektjs
    quadruplesInRulektjs' = concatMap (\s ->
      map (\(k,t,j,f) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ f, DSymbol s),
        (DebuggingTMTypes.L, DState $ q ++ rule ++ k ++ t ++ j ++ f))) quads) $ brackets ++ signs ++ terminalsList ++ negation

    quadruplesInRulektjs'' = map (\(k,t,j,s) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol t),
                                                        (DebuggingTMTypes.L, DState $ q ++ rule ++ k ++ t ++ j ++ s))) quads

    quadruplesInRulektjs = quadruplesInRulektjs' ++ quadruplesInRulektjs''

    -- BLOCK, which generates transitions to next subprograms - move to next rule, folding conj with sign, changing words

    -- fold and put +
    lastConjsWithoutNeg = filter (\t -> let
        hasNeg = checkIfConjHasNeg grammar t
        pair = constructSymbolsPairByQuad t hasNeg
        in isNothing (calculateNextConjunctionInSameRule grammar pair) && not hasNeg) quads
    quadruplesToRewritePlus = map (\(k,t,j,s) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol k),
                    (DebuggingTMTypes.L, DState $ q ++ rewrite ++ plus))) lastConjsWithoutNeg

    -- move to next conjunction
    midConjsWihoutNeg = filter (\t -> let
        hasNeg = checkIfConjHasNeg grammar t
        pair = constructSymbolsPairByQuad t hasNeg
        in isJust (calculateNextConjunctionInSameRule grammar pair) && not hasNeg) quads
    quadruplesToNextConj = map (\(k,t,j,s) -> let
        oldState = q ++ rule ++ k ++ t ++ j ++ s
        newState = q ++ rule ++ k ++ t ++ j ++ s ++ skipParentNonterminal in
        ((DState oldState, DSymbol k),
        (DebuggingTMTypes.R, DState newState))) midConjsWihoutNeg

    -- move to next relation
    conjsInMidRuleWithNeg = filter (\t -> let
        hasNeg = checkIfConjHasNeg grammar t
        pair = constructSymbolsPairByQuad t hasNeg
        in isJust (calculateFirstConjunctionInNextRule grammar pair) && hasNeg) quads
    quadruplesToNextRel = concatMap (\(k,t,j,s) -> let
        inc = DSymbol (show $ (read k :: Int) + 1)
        oldState = q ++ rule ++ k ++ t ++ j ++ s
        newState = q ++ rewrite ++ plus
        stateTransition = oldState ++ transition ++ newState
        in [((DState oldState, DSymbol k), (D inc, DState stateTransition)),
        ((DState stateTransition, inc),(DebuggingTMTypes.R, DState newState))])
        conjsInMidRuleWithNeg

    -- fold and put -
    conjsInLastRuleWithNeg = filter (\t -> let
        hasNeg = checkIfConjHasNeg grammar t
        pair = constructSymbolsPairByQuad t hasNeg
        in isNothing (calculateFirstConjunctionInNextRule grammar pair) && hasNeg) quads
    quadruplesToRewriteMinus = map (\(k,t,j,s) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol k),
        (DebuggingTMTypes.L, DState $ q ++ rewrite ++ minus))) conjsInLastRuleWithNeg

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
    indices = map show [0..maxNumberOfRules - 1]

    --BLOCK for qWordsChangingMoveToBringSymbol
    symbolsInMoveToBringSymbol = map
        (\t -> ((DState moveToBringSymbol, DSymbol t), (DebuggingTMTypes.R, DState moveToBringSymbol))) $ brackets ++ negation
    symbolsToMetFstNonterminalQdrs = map
        (\t -> ((DState moveToBringSymbol, DSymbol t), (DebuggingTMTypes.R, DState metFstNonterminal))) nonterminalsList

    --BLOCK for qWordsChangingMetFstNonterminal
    symbolsInMetFstNonterminal = map
        (\t -> ((DState metFstNonterminal, DSymbol t), (DebuggingTMTypes.R, DState metFstNonterminal)))
        $ brackets ++ signs ++ terminalsList
    symbolsToMetSndNonterminal = map
        (\t -> ((DState metFstNonterminal, DSymbol t), (DebuggingTMTypes.R, DState metSndNonterminal))) nonterminalsList

    --BLOCK for qWordsChangingMetSndNonterminal
    symbolsInMetSndNonterminal = map
        (\t -> ((DState metSndNonterminal, DSymbol t), (DebuggingTMTypes.R, DState metSndNonterminal))) signs
    symbolsToCheckIfSndIsWord = map
        (\t -> ((DState metSndNonterminal, DSymbol t), (DebuggingTMTypes.R, DState checkIfSndIsWord))) leftBracket

    --BLOCK for qWordsChangingCheckIfSndIsWord
    symbolsToSndIsWord = map
        (\t -> ((DState checkIfSndIsWord, DSymbol t), (DebuggingTMTypes.R, DState sndIsWord))) terminalsList

    --BLOCK for qWordsChangingSndIsWord
    symbolsToReturnToParentNonterminal = map
        (\t -> ((DState sndIsWord, DSymbol t), (DebuggingTMTypes.L, DState returnToParentNonterminal))) rightBracket
    symbolsToBringSymbol = map
        (\t -> ((DState sndIsWord, DSymbol t), (DebuggingTMTypes.L, DState bringSymbol))) terminalsList

    --BLOCK for qWordsChangingReturnToParentNonterminal
    notCounters = nonterminalsList ++ terminalsList ++ brackets ++ signs ++ negation
    symbolsInReturnToParentNonterminal = map
        (\t -> ((DState returnToParentNonterminal, DSymbol t),
        (DebuggingTMTypes.L, DState returnToParentNonterminal))) notCounters
    symbolsToWordsChangingFailed = map
        (\counter ->  ((DState returnToParentNonterminal, DSymbol counter),
        (DebuggingTMTypes.R, DState $ failedCheckNegation ++ counter))) indices

    --BLOCK for qWordsChangingFailedCheckNegationK
    symbolsInWordsChangingFailed = map
        (\counter -> ((DState $ failedCheckNegation ++ counter, DSymbol "("),
        (DebuggingTMTypes.R, DState $ failedCheckNegation ++ counter))) indices
    symbolsToWordsChangingFailedNegationK = map
        (\counter -> ((DState $ failedCheckNegation ++ counter, DSymbol "!"),
        (DebuggingTMTypes.L, DState $ failedNegation ++ counter))) indices
    symbolsToWordsChangingFailedK = concatMap
        (\counter -> map (\nonterm -> ((DState $ failedCheckNegation ++ counter, DSymbol nonterm),
        (DebuggingTMTypes.L, DState $ failed ++ counter))) nonterminalsList) indices

    --BLOCK for qWordsChangingFailedK
    symbolsToQkMoveToStart = map
        (\counter -> ((DState $ failed ++ counter, DSymbol "("),
        (DebuggingTMTypes.L, DState $ "q" ++ counter ++ moveToStart))) indices

    --BLOCK for qWordsChangingFailedNegationK
    symbolsToQkMoveToStartNegation = map
        (\counter -> ((DState $ failedNegation ++ counter, DSymbol "("),
        (DebuggingTMTypes.L, DState $ "q" ++ counter ++ moveToStart ++ negationWord))) indices

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
    symbolsToBroughtSymbolt = concatMap (\t -> let
        oldState = bringSymbol
        newState = broughtSymbol ++ t
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol t),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol t),(DebuggingTMTypes.L, DState newState))
        ]) terminalsList
    --BLOCK for qWordsChangingBringSymbolt (list of states generated for each terminal t)
    symbolsInBroughtSymbolt = map (\t ->
        ((DState $ broughtSymbol ++ t, DSymbol " "),(DebuggingTMTypes.L, DState $ broughtSymbol ++ t))) terminalsList
    remembered = signs ++ nonterminalsList ++ brackets
    pairs = concatMap (\t -> map (t,) remembered) terminalsList
    symbolsToWritetk = concatMap (\(t, k) -> let
        oldState = broughtSymbol ++ t
        newState = write ++ t ++ k
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol k),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTMTypes.R, DState newState))]) pairs
    --BLOCK for qWordsChangingWritetk
    remembered' = signs ++ nonterminalsList ++ [leftBracket]
    pairs' = concatMap (\t -> map (t,) remembered') terminalsList
    symbolsToBroughtSymbolt' = concatMap (\(t, k) -> let
        oldState = write ++ t ++ k
        newState = broughtSymbol ++ t
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol " "),(D $ DSymbol k, DState stateTransition)),
        ((DState stateTransition, DSymbol k),(DebuggingTMTypes.L, DState newState))
        ]) pairs'
    -- special case for )
    symbolsToWriteBroughtSymbolt = concatMap (\t -> let
        oldState = write ++ t ++ rightBracket
        newState = writeBroughtSymbolt
        stateTransition = oldState ++ transition ++ newState
        in
        [((DState oldState, DSymbol " "),(D $ DSymbol rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol rightBracket),(DebuggingTMTypes.R, DState newState))]) terminalsList

    --BLOCK for qWordsChangingWriteBroughtSymbolt
    symbolsToCreateCounterForFstNonterminal = concatMap (\t -> let
        oldState = writeBroughtSymbolt
        newState = createCounterForFstNonterminal
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol " "),(D $ DSymbol t, DState stateTransition)),
        ((DState stateTransition, DSymbol t),(DebuggingTMTypes.L, DState newState))]) terminalsList

    --BLOCK for qWordsChangingCreateCounterForFstNonterminal
    symbolsInCreateCounterForFstNonterminal = map (\t ->
        ((DState createCounterForFstNonterminal, DSymbol t),
        (DebuggingTMTypes.L, DState createCounterForFstNonterminal)))
        $ leftBracket : terminalsList
    symbolsToMoveToEnd = concatMap (\t -> let
        oldState = createCounterForFstNonterminal
        newState = moveToEnd
        stateTransition = oldState ++ transition  ++ newState in
        [((DState oldState, DSymbol t),(D $ DSymbol fstCounter, DState stateTransition)),
        ((DState stateTransition, DSymbol fstCounter),(DebuggingTMTypes.R, DState newState))
        ]) signs

    --BLOCK for qWordsChangingMoveToEnd
    symbols = fstCounter  : brackets ++ terminalsList ++ nonterminalsList 
    symbolsInMoveToEnd = map (\t -> ((DState moveToEnd, DSymbol t),(DebuggingTMTypes.R, DState moveToEnd))) symbols
    symbolsToQFindNewSubstitution = map (\t -> 
        ((DState moveToEnd, DSymbol t),(DebuggingTMTypes.L, DState qFindNewSubstitution))) $ " " : signs 

    quadruples = symbolsToBroughtSymbolt ++ symbolsInBroughtSymbolt ++ symbolsToWritetk
        ++ symbolsToBroughtSymbolt' ++ symbolsToWriteBroughtSymbolt ++ symbolsToCreateCounterForFstNonterminal
        ++ symbolsInCreateCounterForFstNonterminal ++ symbolsToMoveToEnd ++ symbolsInMoveToEnd
        ++ symbolsToQFindNewSubstitution

    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

generateBlockForWritingSigns :: Grammar -> DebuggingQuadruples
generateBlockForWritingSigns grammar@(Grammar (nonterminals, _, _,_)) = let
    qRewrite = "qRewrite"
    withMinus = "withMinus"
    qFindNewSubstitution = "qFindNewSubstitution"
    transition = "transition"
    qSkipParentNonterminal = "qSkipParentNonterminal"
    minus = "-"
    plus = "+"
    signs = [minus, plus]
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals

  --BLOCK for qRewriteNWithWord (N - nonterminal)
  -- case, when current rule has one terminal in right part: applying this rule to word with more
  -- than 1 symbol is impossible
    nonterminalsWithIndices = Map.toList $ getNumbersOfShortRelations grammar
    symbolsToQFindNewSubstitution = concatMap (\(Nonterminal nonterm, indices) -> concatMap (\i -> let
        oldState = qRewrite ++ nonterm ++ withMinus
        newState = qFindNewSubstitution
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol i),(D $ DSymbol minus, DState stateTransition)),
         ((DState stateTransition, DSymbol i),(DebuggingTMTypes.L, DState newState))])
         indices) nonterminalsWithIndices

    --BLOCK for qRewriteMinus or qRewritePlus
    pairs = concatMap (\sign -> map (sign,) nonterminalsList) signs
    symbolsToQRewriteNSign = map (\(sign, nonterm) -> let
        newState =  qRewrite ++ nonterm ++ sign in
        ((DState $ qRewrite ++ sign, DSymbol nonterm),(DebuggingTMTypes.R, DState newState))) pairs

    --BLOCK for qRewriteNSign
    maxNumber = calculateMaxNumberOfRulesForNonterminal grammar
    possibleIndices = map show [1..maxNumber]
    symbolsToQSkipParentNonterminal = concatMap (\(sign, nonterm) -> let
        oldState = qRewrite ++ nonterm ++ sign
        newState = qSkipParentNonterminal
        stateTransition = oldState ++ transition ++ newState in
        concatMap (\index ->
            [((DState oldState, DSymbol index),(D $ DSymbol sign, DState stateTransition)),
             ((DState stateTransition, DSymbol index),(DebuggingTMTypes.R, DState newState))])
             possibleIndices) pairs

    quadruples = symbolsToQFindNewSubstitution ++ symbolsToQRewriteNSign ++ symbolsToQSkipParentNonterminal
    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

generateBlockForFolding :: Grammar -> DebuggingQuadruples
generateBlockForFolding (Grammar (nonterminals, terminals, _, _)) = let
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
    qFindNewSubstitution = "qFindNewSubstitution"
    
    minus = "-"
    plus = "+"
    signs = [minus, plus]
    negation = "!"
    leftBracket = "("
    rightBracket = ")"
    brackets = [leftBracket, rightBracket]
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals

    --BLOCK for qSkipParentNonterminal
    symbolsToQRemoveSymbols = [((DState qSkipParentNonterminal, DSymbol leftBracket),(DebuggingTMTypes.R, DState qRemoveSymbols))]

    --BLOCK for qRemoveSymbols
    symbolsInQRemoveSymbols = concatMap (\t -> let
        stateTransition = qRemoveSymbols ++ transition in
        [((DState qRemoveSymbols, DSymbol t),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTMTypes.R, DState qRemoveSymbols))]) $ nonterminalsList ++ [negation]
    symbolsToQRemoveBracketAroundFstWord = concatMap (\sign -> let
        stateTransition = qRemoveSymbols ++ transition ++ qRemoveBracketsAroundFstWord in
        [((DState qRemoveSymbols, DSymbol sign),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTMTypes.R, DState qRemoveBracketsAroundFstWord))]) signs

    --BLOCK for qRemoveBracketsAroundFstWord
    symbolsInQRemoveBracketsAroundFstWord' = let
        state = qRemoveBracketsAroundFstWord
        stateTransition = state ++ transition ++ state
        in [((DState state, DSymbol leftBracket),(D $ DSymbol " ", DState stateTransition)),
            ((DState stateTransition, DSymbol " " ),(DebuggingTMTypes.R, DState state))]
    symbolsInQRemoveBracketsAroundFstWord'' = map (\t -> let
            state = qRemoveBracketsAroundFstWord in
            ((DState state, DSymbol t),(DebuggingTMTypes.R, DState state))) terminalsList
    symbolsInQRemoveBracketsAroundFstWord =
      symbolsInQRemoveBracketsAroundFstWord' ++ symbolsInQRemoveBracketsAroundFstWord''
    symbolsToQRemoveBracketsAroundSndWord = let
        oldState = qRemoveBracketsAroundFstWord
        newState = qRemoveBracketsAroundSndWord
        stateTransition = oldState ++ transition ++ newState
        in [((DState oldState, DSymbol rightBracket),(D $ DSymbol " ", DState stateTransition)),
            ((DState stateTransition, DSymbol " " ),(DebuggingTMTypes.R, DState newState))]

    --BLOCK for qRemoveBracketsAroundSndWord
    symbolsInQRemoveBracketsAroundSndWord' = concatMap (\t -> let
        state = qRemoveBracketsAroundSndWord
        stateTransition = state ++ transition ++ state
        in [((DState state, DSymbol t),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTMTypes.R, DState state))])
        $ terminalsList ++ signs ++ [leftBracket]
    symbolsInQRemoveBracketsAroundSndWord'' = map (\t -> let
        state = qRemoveBracketsAroundSndWord in
        ((DState state, DSymbol t),(DebuggingTMTypes.R, DState state))) terminalsList
    symbolsInQRemoveBracketsAroundSndWord =
      symbolsInQRemoveBracketsAroundSndWord' ++ symbolsInQRemoveBracketsAroundSndWord''
    symbolsToQMoveToStart = let
        oldState = qRemoveBracketsAroundSndWord
        newState = qMoveToStart
        stateTransition = oldState ++ transition ++ newState
        in [((DState oldState, DSymbol rightBracket),(D $ DSymbol " ", DState stateTransition)),
            ((DState stateTransition, DSymbol " "),(DebuggingTMTypes.L, DState newState))]

    --BLOCK for qMoveToStart
    symbolsInQMoveToStart = map (\t -> ((DState qMoveToStart, DSymbol t),
        (DebuggingTMTypes.L, DState qMoveToStart))) $ terminalsList ++ [" "]
    symbolsToQFold = [((DState qMoveToStart, DSymbol leftBracket),(DebuggingTMTypes.R, DState qFold))]

    --BLOCK for qFold
    symbolsInQFold = [((DState qFold, DSymbol " "),(DebuggingTMTypes.R, DState qFold))]
    symbolsToQFoldSLookForNewPlace = concatMap (\t -> let
        oldState = qFold
        newState = qFold ++ t ++ lookForNewPlace
        stateTransition = oldState ++ transition ++ newState
        in
        [((DState oldState, DSymbol t),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTMTypes.L, DState newState))])
        $ terminalsList ++ nonterminalsList ++ brackets ++ signs

    --BLOCK for qFoldSLookForNewPlace - it common for all symbols except ")"
    symbols = terminalsList ++ nonterminalsList ++ [leftBracket] ++ signs
    symbolsInQFoldSLookForNewPlace = map (\t -> let
        state = qFold ++ t ++ lookForNewPlace in
        ((DState state, DSymbol " "),(DebuggingTMTypes.L, DState state))) symbols

    --BLOCK for qFoldNonterminalLookForNewPlace
    symbolsToQFoldNonterminalWrite = generateToQFoldSymbolWrite' [rightBracket] nonterminalsList
    --BLOCK for qFoldTerminalLookForNewPlace
    symbolsToQFoldTerminalWrite = generateToQFoldSymbolWrite' (terminalsList ++ [leftBracket]) terminalsList

    --BLOCK for qFoldSignLookForNewPlace
    symbolsToQFoldSignWrite = generateToQFoldSymbolWrite' nonterminalsList signs

    --BLOCK for qFold(LookForNewPlace
    symbolsToQFoldLeftBracketWrite = generateToQFoldSymbolWrite' signs [leftBracket]

    --BLOCK for qFold)LookForNewPlace
    qFoldRightBracketLookForNewPlace = qFold ++ rightBracket ++ lookForNewPlace
    symbolsInQFoldRightBracketLookForNewPlace =
        [((DState qFoldRightBracketLookForNewPlace, DSymbol " "),
            (DebuggingTMTypes.R, DState qFoldRightBracketLookForNewPlace))]
    symbolsToQCheckLastRightBracket = let
        oldState = qFoldRightBracketLookForNewPlace
        newState = qCheckLast
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol rightBracket),(D $ DSymbol " ", DState stateTransition)),
        ((DState stateTransition, DSymbol " "),(DebuggingTMTypes.R, DState newState))]

    --BLOCK for qCheckLast)
    symbolsToQFoldRightBracketLast' = [((DState qCheckLast, DSymbol " "),
        (DebuggingTMTypes.L, DState qFoldRightBracketLast'))]
    symbolsToQFoldRightBracket' = map (\t ->
        ((DState qCheckLast, DSymbol t),(DebuggingTMTypes.L, DState qFoldRightBracket'))
        ) $ nonterminalsList ++ [rightBracket]

    --BLOCK for case, when bracket is not last
    symbolsInQFoldRightBracket' = [((DState qFoldRightBracket', DSymbol " "),
        (DebuggingTMTypes.L, DState qFoldRightBracket'))]
    symbolsToQFoldRightBracket = map (\t ->
        ((DState qFoldRightBracket', DSymbol t),(DebuggingTMTypes.R, DState qFoldRightBracket))
        ) $ terminalsList ++ [rightBracket]
    symbolsFromRightBracketToQFold = let
        oldState = qFoldRightBracket'
        newState = qFold
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol " "),(D $ DSymbol rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol rightBracket),(DebuggingTMTypes.R, DState newState))]

    --BLOCK for case, when bracket is last
    symbolsInQFoldRightBracketLast' =
      [((DState qFoldRightBracketLast', DSymbol " "),(DebuggingTMTypes.L, DState qFoldRightBracketLast'))]
    symbolsToQFoldRightBracketLast = map (\t ->
        ((DState qFoldRightBracketLast', DSymbol t),(DebuggingTMTypes.R, DState qFoldRightBracketLast)))
        $ terminalsList ++ [rightBracket]
    symbolsFromRightBracketLastToQFindNewSubstitution = let
        oldState = qFoldRightBracketLast
        newState = qFindNewSubstitution
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol " "),(D $ DSymbol rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol rightBracket),(DebuggingTMTypes.R, DState newState))]

    quadruples = symbolsInQRemoveSymbols ++ symbolsToQRemoveSymbols ++ symbolsToQRemoveBracketAroundFstWord
        ++ symbolsInQRemoveBracketsAroundFstWord ++ symbolsToQRemoveBracketsAroundSndWord
        ++ symbolsInQRemoveBracketsAroundSndWord
        ++ symbolsToQMoveToStart ++ symbolsInQMoveToStart ++ symbolsToQFold ++ symbolsInQFold
        ++ symbolsToQFoldSLookForNewPlace
        ++ symbolsInQFoldSLookForNewPlace ++ symbolsToQFoldNonterminalWrite ++ symbolsToQFoldTerminalWrite
        ++ symbolsToQFoldSignWrite ++ symbolsToQFoldLeftBracketWrite ++ symbolsInQFoldRightBracketLookForNewPlace
        ++ symbolsToQCheckLastRightBracket ++ symbolsToQFoldRightBracketLast' ++ symbolsToQFoldRightBracket'
        ++ symbolsInQFoldRightBracket' ++ symbolsToQFoldRightBracket ++ symbolsInQFoldRightBracketLast'
        ++ symbolsToQFoldRightBracketLast ++ symbolsFromRightBracketToQFold
        ++ symbolsFromRightBracketLastToQFindNewSubstitution
    in (DQuadruples $ addCollectionToMap quadruples Map.empty)


generateToQFoldSymbolWrite' :: [String] -> [String]
    -> [((DebuggingState, DebuggingSymbol), (DebuggingMove, DebuggingState))]
generateToQFoldSymbolWrite' inputSymbols stateSymbols = let
    qFold = "qFold"
    lookForNewPlace = "LookForNewPlace"
    write = "write"
    symbolsToQFoldLeftBracketWrite = concatMap (\t -> let
        oldState = qFold ++ t ++ lookForNewPlace
        newState = qFold ++ t ++ write in
        map (\s -> ((DState oldState, DSymbol s),(DebuggingTMTypes.R, DState newState)))
            inputSymbols) stateSymbols
    in symbolsToQFoldLeftBracketWrite

-- short relation is relation with one terminal in right part
getNumbersOfShortRelations :: Grammar -> Map.Map Nonterminal [String]
getNumbersOfShortRelations (Grammar (_, _, relations, _)) =
    Map.mapWithKey getShortRightParts $ calculateGroupRelationsByNonterminals $ Set.toList relations

getShortRightParts :: Nonterminal -> [[GrammarType.Symbol]] -> [String]
getShortRightParts nonterminal rightParts = let
    shortOrNotRels = map (\t -> relationHasOneTerminalInRightPart (Relation (nonterminal, t))) rightParts
    indices' = map (\t -> if t then elemIndex t shortOrNotRels else Nothing) shortOrNotRels
    indices = map show $ catMaybes indices'
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
    qRememberStart = "qRememberStart"
    qRewriteWithWord = "qRewriteWithWord"
    qFindNewSubstitution = "qFindNewSubstitution"
    q = "q"
    transition = "transition"
    one = "1"
    leftBracket = "("
    rightBracket = ")"
    minus = "-"
    plus = "+"
    signs = [plus, minus]
    maxNumber = calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [0..maxNumber - 1]
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals
    relationsList = Set.toList relations

    symbolsToQWriteStartCounterK = concatMap (\t -> let
        oldState = qWriteStartCounter
        newState = qWriteStartCounter ++ t
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol t),(D $ DSymbol one, DState stateTransition)),
        ((DState stateTransition, DSymbol one),(DebuggingTMTypes.L, DState newState))
        ]) nonterminalsList

    --BLOCK for qWriteStartCounterK
    symbolsToQCountWordLength = concatMap (\t -> let
        oldState = qWriteStartCounter ++ t
        newState = qCountWordLength
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol " "),(D $ DSymbol t, DState stateTransition)),
        ((DState stateTransition, DSymbol t),(DebuggingTMTypes.R, DState newState))
        ]) nonterminalsList

    --BLOCK for qCountWordLength
    symbolsInQCountWordLength = map (\t ->
        ((DState qCountWordLength, DSymbol t),(DebuggingTMTypes.R, DState qCountWordLength)))
        $ indices ++ nonterminalsList ++ [leftBracket]
    symbolsToQCheckSymbol = map (\t ->
        ((DState qCountWordLength, DSymbol t),(DebuggingTMTypes.R, DState qCheckSymbol))) terminalsList

    --BLOCK for qCheckSymbol
    symbolsToQSymbol = map (\t ->
        ((DState qCheckSymbol, DSymbol t),(DebuggingTMTypes.L, DState qSymbol))) [rightBracket]
    symbolsToQWord = map (\t ->
            ((DState qCheckSymbol, DSymbol t),(DebuggingTMTypes.L, DState qWord))) terminalsList

    --BLOCK for qSymbol
    symbolsInQSymbol = map (\t ->
        ((DState qSymbol, DSymbol t),(DebuggingTMTypes.L, DState qSymbol))) $ terminalsList ++ [leftBracket]
    symbolsToQStart = map (\t ->
        ((DState qSymbol, DSymbol t),(DebuggingTMTypes.L, DState qStart))) indices
    
    --BLOCK for qStart
    symbolsToQNonterminal = map (\t ->
        ((DState qStart, DSymbol t),(DebuggingTMTypes.L, DState $ q ++ t))) nonterminalsList

    --BLOCK for qNonterminal
    symbolsInQNonterminal = concatMap (\t -> let
        symbols = indices ++ [leftBracket]
        in map (\s ->
            ((DState $ q ++ t, DSymbol s),(DebuggingTMTypes.R, DState $ q ++ t)))
            symbols) nonterminalsList
    symbolsToQNonterminalSign = concatMap (\t -> map (\s ->
        if symbolAcceptedByNonterminal grammar t s
            then ((DState $ q ++ t, DSymbol s),(DebuggingTMTypes.L, DState $ q ++ t ++ plus))
            else ((DState $ q ++ t, DSymbol s),(DebuggingTMTypes.L, DState $ q ++ t ++ minus))
        ) terminalsList) nonterminalsList

    --BLOCK for qNonterminal
    -- if there is no terminals, which is accepted by given nonterminal, there will be unreachable
    -- state qNonterminal+
    symbolsInQNonterminalSign = concatMap (\t -> map (\sign ->
        ((DState $ q ++ t ++ sign, DSymbol leftBracket),(DebuggingTMTypes.L, DState $ q ++ t ++ sign)))
        signs) nonterminalsList
    symbolsToQFindNewSubstitution = concatMap (\t -> concatMap (\sign -> let
        oldState = q ++ t ++ sign
        newState = qFindNewSubstitution
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol t),(D $ DSymbol sign, DState stateTransition)),
        ((DState stateTransition, DSymbol sign),(DebuggingTMTypes.L, DState newState))]
        ) signs) indices

    --BLOCK for qWord
    symbolsInQWord = map (\t ->
        ((DState qWord, DSymbol t),(DebuggingTMTypes.L, DState qWord))) $ terminalsList ++ [leftBracket]
    symbolsToQChooseRelationI = map (\t ->
        ((DState qWord, DSymbol t),(DebuggingTMTypes.L, DState $ qChooseRelation ++ t))) indices

    --BLOCK for qChooseRelationI
    symbolsToQRemember = concatMap (\k -> let
        nonterminals' = filter (\n -> kthRelForNonterminalLong relationsList n k) nonterminalsList
        oldState = qChooseRelation ++ k
        in map (\t -> let
            (SymbolsPair (_, number, _, nonterm1, nonterm2)) = getFstConjInKthRel grammar t k
            nonterm1Val = refineSymbolInConjunctionToNonterminal nonterm1
            nonterm2Val = refineSymbolInConjunctionToNonterminal nonterm2
            newState = qRememberStart ++ show number ++ t ++ nonterm1Val ++ nonterm2Val
            in
            ((DState oldState, DSymbol t),(DebuggingTMTypes.R, DState newState)))
            nonterminals') indices

    symbolsToQRewriteWithWordNonterminal = concatMap (\k -> let
        nonterminals' = filter (\n -> kthRelForNonterminalLong relationsList n k) nonterminalsList
        rest = nonterminalsList \\ nonterminals'
        oldState = qChooseRelation ++ k
        in map (\t ->
            ((DState oldState, DSymbol t),(DebuggingTMTypes.R, DState $ qRewriteWithWord ++ t)))
            rest) indices


    quadruples = symbolsToQWriteStartCounterK ++ symbolsToQCountWordLength ++ symbolsInQCountWordLength
        ++ symbolsToQCheckSymbol ++ symbolsToQSymbol ++ symbolsToQWord ++ symbolsInQSymbol
        ++ symbolsToQStart ++ symbolsInQWord ++ symbolsToQChooseRelationI ++ symbolsToQRemember
        ++ symbolsToQRewriteWithWordNonterminal ++ symbolsToQNonterminal ++ symbolsInQNonterminal
        ++ symbolsToQNonterminalSign ++ symbolsInQNonterminalSign ++ symbolsToQFindNewSubstitution
    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

kthRelForNonterminalLong :: [Relation] -> String -> String -> Bool
kthRelForNonterminalLong relations nontermVal k = do
  let k' = read k :: Int
  let groupedRelations = calculateGroupRelationsByNonterminals relations
  let nonterminal = Nonterminal nontermVal
  let relationsForNonterm = groupedRelations Map.! nonterminal
  length relationsForNonterm >= k' &&
    not (relationHasOneTerminalInRightPart $ Relation (nonterminal, relationsForNonterm !! k'))

getFstConjInKthRel :: Grammar -> String -> String -> SymbolsPair
getFstConjInKthRel (Grammar (_, _, relations, _)) nontermVal number = let
    number' = read number :: Int
    nonterminal = Nonterminal nontermVal
    groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    relationsForNonterminal = groupedRelations Map.! nonterminal
    relation = relationsForNonterminal !! number'
    conjunctionPairs = splitOn [O Conjunction] relation
    in convertListToConjunctionPair nonterminal number' $ head conjunctionPairs


symbolAcceptedByNonterminal :: Grammar -> String -> String -> Bool
symbolAcceptedByNonterminal (Grammar (_, _, relations, _)) nontermValue symbol = let
    groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    nonterminal = Nonterminal nontermValue
    nontermRels = groupedRelations Map.! nonterminal
    terminalsInRightPart' = map (\symbols ->
        if relationHasOneTerminalInRightPart (Relation (nonterminal, symbols))
            then Just $ head symbols
            else Nothing) nontermRels
    terminalsInRightPart = catMaybes terminalsInRightPart'
    terminalsValues = map refineSymbolToTerminalValue terminalsInRightPart
    in elem symbol terminalsValues

refineSymbolToTerminalValue :: GrammarType.Symbol -> String
refineSymbolToTerminalValue (T t) = terminalValue t
refineSymbolToTerminalValue _ = error "Given symbol is not terminal"

generateBlockForPreparingForSubstitution :: Grammar -> DebuggingQuadruples
generateBlockForPreparingForSubstitution grammar@(Grammar (nonterminals, terminals, rels, _)) = let
    qRememberStart = "qRememberStart"
    qRemember = "qRemember"
    markEnd = "MarkEnd"
    qShiftWord = "qShiftWord"
    qWriteSymbol = "qWriteSymbol"
    qWriteRemembered = "qWriteRemembered"
    qWriteCounter = "qWriteCounter"
    qMoveToEnd = "qMoveToEnd"
    qUnmarkEnd = "qUnmarkEnd"
    qWritingRelation = "qWritingRelation"

    transition = "transition"
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals
    maxNumberOfRels = calculateMaxNumberOfRulesForNonterminal grammar
    relsList = Set.toList rels
    indices = map show [0..maxNumberOfRels - 1]
    space = " "
    leftBracket = "("
    rightBracket = ")"
    star = "*"
    plus = "+"
    minus = "-"
    hash = "#"
    signs = [plus, minus]
    brackets = [leftBracket, rightBracket]

    --helper calculations
    indicesWithNonterms = map (\i ->
        (i, filter (\t -> kthRelForNonterminalLong relsList t i) nonterminalsList)) indices
    -- quads have same form, as other quads in this module, only difference is
    -- they are quads for first conjunctions in kth relations
    quads = concatMap (\(i, nonterms) -> map (\nonterm -> let
        (SymbolsPair (_, number, _, nonterm1, nonterm2)) = getFstConjInKthRel grammar nonterm i
        nonterm1Val = refineSymbolInConjunctionToNonterminal nonterm1
        nonterm2Val = refineSymbolInConjunctionToNonterminal nonterm2
        in (show number, nonterm, nonterm1Val, nonterm2Val)
        ) nonterms) indicesWithNonterms
    --BLOCK for qRememberStartCounterNonterm1Nonterm2Nonterm3
    rememberedSymbols = terminalsList ++ [rightBracket]
    quintets = concatMap (\(k, t, j, f) -> map (k, t, j, f,) rememberedSymbols) quads

    symbolsInQRememberStartKTJF = concatMap (\(k, t, j, f) -> map (\symbol ->
        ((DState $ qRememberStart  ++ k ++ t ++ j ++ f, DSymbol symbol),
        (DebuggingTMTypes.R, DState $ qRememberStart  ++ k ++ t ++ j ++ f))) $ indices ++ [leftBracket])
        quads
    symbolsToQRememberSymbolMarkEndKTJF = concatMap (\(k, t, j, f, symbol) -> let
        oldState = qRememberStart  ++ k ++ t ++ j ++ f
        newState = qRemember ++ symbol ++ markEnd ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState
        in
        [((DState oldState, DSymbol symbol),(D $ DSymbol star, DState stateTransition)),
        ((DState stateTransition, DSymbol star),(DebuggingTMTypes.R, DState newState))]) quintets

    --BLOCK for qRememberSymbolMarkEndKTJF
    symbolsInQRememberSymbolMarkEndKTJF = concatMap (\(k, t, j, f, s) -> map (\symbol -> let
        state = qRemember ++ s ++ markEnd ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol symbol), (DebuggingTMTypes.R, DState state)))
         $ nonterminalsList ++ signs ++ brackets ++ terminalsList) quintets
    symbolsToQShiftWordSymbolKTJF = concatMap (\(k, t, j, f, s) -> let
        oldState = qRemember ++ s ++ markEnd ++ k ++ t ++ j ++ f
        newState = qShiftWord ++ s ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol hash, DState stateTransition)),
        ((DState stateTransition, DSymbol hash), (DebuggingTMTypes.R, DState newState))]) quintets

    --BLOCK for qShiftWordSymbolKTJF
    symbolsInQShiftWordSymbolKTJF = concatMap (\(k, t, j, f, s) -> let
        state = qShiftWord ++ s ++ k ++ t ++ j ++ f in
        [((DState state, DSymbol space), (DebuggingTMTypes.L, DState state))]) quintets

    symbolsWithSimilarStatesForTerms = nonterminalsList ++ signs ++
        terminalsList ++ brackets ++ [hash]

    tuple6 = concatMap (\(k, t, j, f, s) -> map (k, t, j, f, s,)
        symbolsWithSimilarStatesForTerms) quintets
    symbolsToQWriteSymbolKTJFsymbol = concatMap (\(k, t, j, f, s, s') -> let
        oldState = qShiftWord ++ s ++ k ++ t ++ j ++ f
        newState = qWriteSymbol ++ s ++ k ++ t ++ j ++ f ++ s'
        stateTransition = oldState ++ transition ++ newState
        in [((DState oldState, DSymbol s'), (D $ DSymbol space, DState stateTransition)),
            ((DState stateTransition, DSymbol space), (DebuggingTMTypes.R, DState newState))]
        ) tuple6

    -- case for star for terminals and bracket
    symbolsToQWriteSymbolKTJFStar = map (\(k, t, j, f, s) -> let
        oldState = qShiftWord ++ s ++ k ++ t ++ j ++ f
        newState = qWriteRemembered ++ s ++ k ++ t ++ j ++ f in
        ((DState oldState, DSymbol star), (DebuggingTMTypes.R, DState newState))) quintets

    --FIXME add docs to size of shift dependence
    shift = 6
    shifts = map show [1..shift]
    shiftNeg = 7
    shiftsNeg = map show [1..shiftNeg]
    -- case for shifts for terminals
    terminalQuintets = concatMap (\(k, t, j, f) ->
        map (k, t, j, f,) terminalsList) quads
    symbolsToQWriteSymbolKTJFshift = concatMap (\(k, t, j, f, s) ->
        if checkIfConjHasNeg grammar (k, t, j, f) then
            map (\i -> let
            oldState = qShiftWord ++ s ++ k ++ t ++ j ++ f
            newState = qWriteCounter ++ s ++ k ++ t ++ j ++ f in
            ((DState oldState, DSymbol i), (DebuggingTMTypes.R, DState newState))) shiftsNeg
        else
            map (\i -> let
            oldState = qShiftWord ++ s ++ k ++ t ++ j ++ f
            newState = qWriteCounter ++ s ++ k ++ t ++ j ++ f in
            ((DState oldState, DSymbol i), (DebuggingTMTypes.R, DState newState))) shifts
            ) terminalQuintets

    --BLOCK for qWriteSymbolKTJFs'
    symbolsFromQWriteSymbolKTJFs'ToQShiftWordSymbolKTJF = concatMap (\(k, t, j, f, s, s') -> let
        oldState = qWriteSymbol ++ s ++ k ++ t ++ j ++ f ++ s'
        newState = qShiftWord ++ s ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol s', DState stateTransition)),
        ((DState stateTransition, DSymbol s'), (DebuggingTMTypes.L, DState newState))]) tuple6

    --BLOCK for qWriteRememberedsKTJF
    shiftsPairs = getShiftsDecrements shift star
    shiftsPairsNeg = getShiftsDecrements shiftNeg star
    symbolsToQWriteCountersKTJFterminals = concatMap (\(k, t, j, f, s) -> let
        oldState = qWriteRemembered ++ s ++ k ++ t ++ j ++ f
        newState = qWriteCounter ++ s ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState
        in
        [((DState oldState, DSymbol space), (D $ DSymbol s, DState stateTransition)),
        ((DState stateTransition, DSymbol s), (DebuggingTMTypes.L, DState newState))]) terminalQuintets
    -- BLOCK for qWriteCounterSymbolKTJF (terminals)
    symbolsInQWriteCountersKTJF = map (\(k, t, j, f, s) -> let
        oldState = qWriteCounter ++ s ++ k ++ t ++ j ++ f in
        ((DState oldState, DSymbol space), (DebuggingTMTypes.L, DState oldState))) terminalQuintets

    -- pair ("1", "*") = pair of end of shifting, there is special transition, this pair called endShift
    -- rest of pairs called midPairs
    endShift = head $ filter (\t -> snd t == star) shiftsPairs
    midShifts = shiftsPairs \\ [endShift]
    endShiftNeg = head $ filter (\t -> snd t == star) shiftsPairsNeg
    midShiftsNeg = shiftsPairsNeg \\ [endShiftNeg]

    symbolsToQUnmarkEndKTJF = concatMap (\(k, t, j, f, s) ->
        if checkIfConjHasNeg grammar (k, t, j, f) then let
            oldState =  qWriteCounter ++ s ++ k ++ t ++ j ++ f
            newState = qUnmarkEnd ++ k ++ t ++ j ++ f
            stateTransition = oldState ++ transition ++ newState
            oldSymbol = fst endShiftNeg
            newSymbol = snd endShiftNeg in
            [((DState oldState, DSymbol oldSymbol), (D $ DSymbol newSymbol, DState stateTransition)),
            ((DState stateTransition, DSymbol newSymbol), (DebuggingTMTypes.R, DState newState))]
        else let
            oldState =  qWriteCounter ++ s ++ k ++ t ++ j ++ f
            newState = qUnmarkEnd ++ k ++ t ++ j ++ f
            stateTransition = oldState ++ transition ++ newState
            oldSymbol = fst endShift
            newSymbol = snd endShift in
            [((DState oldState, DSymbol oldSymbol), (D $ DSymbol newSymbol, DState stateTransition)),
            ((DState stateTransition, DSymbol newSymbol), (DebuggingTMTypes.R, DState newState))]
        ) terminalQuintets
    symbolsToQMoveToEndsKTJF = concatMap (\(k, t, j, f, s) ->
        if checkIfConjHasNeg grammar (k, t, j, f) then
            concatMap (\ (i, iDecr) -> let
            oldState =  qWriteCounter ++ s ++ k ++ t ++ j ++ f
            newState = qMoveToEnd ++ s ++ k ++ t ++ j ++ f
            stateTransition = oldState ++ transition ++ newState in
            [((DState oldState, DSymbol i), (D $ DSymbol iDecr, DState stateTransition)),
            ((DState stateTransition, DSymbol iDecr), (DebuggingTMTypes.L, DState newState))]) midShiftsNeg
         else
            concatMap (\ (i, iDecr) -> let
            oldState =  qWriteCounter ++ s ++ k ++ t ++ j ++ f
            newState = qMoveToEnd ++ s ++ k ++ t ++ j ++ f
            stateTransition = oldState ++ transition ++ newState in
            [((DState oldState, DSymbol i), (D $ DSymbol iDecr, DState stateTransition)),
            ((DState stateTransition, DSymbol iDecr), (DebuggingTMTypes.L, DState newState))]) midShifts
        ) terminalQuintets

    --BLOCK for qMoveToEndsKTJF
    symbolsInQMoveToEndKTJF = concatMap (\(k, t, j, f, s) -> map (\symbol -> let
        state = qMoveToEnd ++ s ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol symbol), (DebuggingTMTypes.R, DState state)))
        $ nonterminalsList ++ signs ++ brackets ++ terminalsList ++ [space]) terminalQuintets
    symbolsFromQMoveToEndToQShiftWordSymbolKTJF = map (\(k, t, j, f, s) -> let
        oldState = qMoveToEnd ++ s ++ k ++ t ++ j ++ f
        newState = qShiftWord ++ s ++ k ++ t ++ j ++ f in
        ((DState oldState, DSymbol hash), (DebuggingTMTypes.R, DState newState))) terminalQuintets

    -- BLOCK for star (bracket - special case)
    symbolsInQWriteRememberedBracket = concatMap (\(k, t, j, f) -> let
        oldState = qWriteRemembered ++ rightBracket ++ k ++ t ++ j ++ f
        newState = oldState
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol rightBracket), (DebuggingTMTypes.L, DState newState))]) quads
    symbolsToQUnmarkEndKTJFBracket = concatMap (\(k, t, j, f) -> let
        oldState = qWriteRemembered ++ rightBracket ++ k ++ t ++ j ++ f
        newState = qUnmarkEnd ++ k ++ t ++ j ++ f ++ rightBracket
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol star), (D $ DSymbol rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol rightBracket), (DebuggingTMTypes.R, DState newState))]
        ) quads

    symbolsInQUnmarkEndKTJF = concatMap (\(k, t, j, f) -> map (\s -> let
        state = qUnmarkEnd ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol s), (DebuggingTMTypes.R, DState state)))
        $ nonterminalsList ++ signs ++ brackets ++ terminalsList) quads
    symbolsToQWritingRelationKTJF = concatMap (\(k, t, j, f) -> let
            oldState = qUnmarkEnd ++ k ++ t ++ j ++ f
            newState = qWritingRelation ++ k ++ t ++ j ++ f
            stateTransition = oldState ++ transition ++ newState in
            [((DState oldState, DSymbol hash), (D $ DSymbol space, DState stateTransition)),
            ((DState stateTransition, DSymbol space), (DebuggingTMTypes.R, DState newState))]) quads

    symbolsInQUnmarkEndKTJFBracket = concatMap (\(k, t, j, f) -> map (\s -> let
        state = qUnmarkEnd ++ k ++ t ++ j ++ f ++rightBracket in
        ((DState state, DSymbol s), (DebuggingTMTypes.R, DState state)))
        $ nonterminalsList ++ signs ++ brackets ++ terminalsList) quads
    qFindNewSubstitution = "qFindNewSubstitution"
    symbolsToQFindNewSubstitution = concatMap (\(k, t, j, f) -> let
        oldState = qUnmarkEnd ++ k ++ t ++ j ++ f ++rightBracket
        newState = qFindNewSubstitution
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol hash), (D $ DSymbol space, DState stateTransition)),
        ((DState stateTransition, DSymbol space), (DebuggingTMTypes.R, DState newState))]) quads

    quadruples = symbolsInQRememberStartKTJF ++ symbolsToQRememberSymbolMarkEndKTJF
        ++ symbolsInQRememberSymbolMarkEndKTJF ++ symbolsToQShiftWordSymbolKTJF
        ++ symbolsInQShiftWordSymbolKTJF ++ symbolsToQWriteSymbolKTJFsymbol
        ++ symbolsToQWriteSymbolKTJFStar ++ symbolsToQWriteSymbolKTJFshift
        ++ symbolsFromQWriteSymbolKTJFs'ToQShiftWordSymbolKTJF
        ++ symbolsInQWriteCountersKTJF ++ symbolsToQMoveToEndsKTJF
        ++ symbolsToQWriteCountersKTJFterminals  ++ symbolsInQWriteRememberedBracket
        ++ symbolsToQUnmarkEndKTJFBracket ++ symbolsInQUnmarkEndKTJF
        ++ symbolsToQWritingRelationKTJF ++ symbolsInQMoveToEndKTJF
        ++ symbolsFromQMoveToEndToQShiftWordSymbolKTJF ++ symbolsToQUnmarkEndKTJF
        ++ symbolsInQUnmarkEndKTJFBracket ++ symbolsToQFindNewSubstitution
    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

generateBlockForSubstitution :: Grammar -> DebuggingQuadruples
generateBlockForSubstitution grammar@(Grammar (nonterminals, terminals, relations, _) ) = let
    qWritingRelation = "qWritingRelation"
    qWrite = "qWrite"
    transition = "transition"
    qWriteCounter = "qWriteCounter"
    qLetterWriting = "qLetterWriting"
    preparation = "Preparation"
    letter = "Letter"
    shiftWord = "ShiftWord"
    qRememberStart = "qRememberStart"

    space = " "
    leftBracket = ")"
    rightBracket = "("
    star = "*"
    exclamation = "!"
    comma = "'"
    brackets = [leftBracket, rightBracket]
    maxNumber = calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [0..maxNumber - 1]
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals
    relsList = Set.toList relations

    --helper calculations
    indicesWithNonterms = map (\i ->
        (i, filter (\t -> kthRelForNonterminalLong relsList t i) nonterminalsList)) indices
    -- quads have same form, as other quads in this module, only difference is
    -- they are quads for first conjunctions in kth relations
    quads = concatMap (\(i, nonterms) -> map (\nonterm -> let
        (SymbolsPair (_, number, _, nonterm1, nonterm2)) = getFstConjInKthRel grammar nonterm i
        nonterm1Val = refineSymbolInConjunctionToNonterminal nonterm1
        nonterm2Val = refineSymbolInConjunctionToNonterminal nonterm2
        in (show number, nonterm, nonterm1Val, nonterm2Val)
        ) nonterms) indicesWithNonterms

    quadsNeg = filter (checkIfConjHasNeg grammar) quads
    quadsPos = quads \\ quadsNeg

    --BLOCK for qWritingRelationKTJF
    symbols = terminalsList ++ nonterminalsList ++ brackets ++ [space]
    symbolsInQWritingRelationKTJF = concatMap (\(k, t, j, f) -> map (\symbol ->
        let state = qWritingRelation ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol symbol), (DebuggingTMTypes.L, DState state))) symbols) quads

    -- block for case when conj (k, t, j, f) has neg
    symbolsFromQWritingRelationToQWriteKTJFT = concatMap (\(k, t, j, f) -> let
        oldState = qWritingRelation ++ k ++ t ++ j ++ f
        newState = qWrite ++ k ++ t ++ j ++ f ++ j
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol star), (D $ DSymbol exclamation, DState stateTransition)),
        ((DState stateTransition, DSymbol exclamation), (DebuggingTMTypes.R, DState newState))]) quadsNeg
    symbolsFromQWriteKTJFJtoQWriteCounterKTJFJ = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ j
        newState = qWriteCounter ++ k ++ t ++ j ++ f ++ j
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol star), (D $ DSymbol exclamation, DState stateTransition)),
        ((DState stateTransition, DSymbol exclamation), (DebuggingTMTypes.R, DState newState))]) quadsNeg

    -- block for case when conj (k, t, j, f) does not have neg
    -- writing initial value to counter
    symbolsFromQWritingRelationToQWriteCounterKTJFT = concatMap (\(k, t, j, f) -> let
        oldState = qWritingRelation ++ k ++ t ++ j ++ f
        newState = qWriteCounter ++ k ++ t ++ j ++ f ++ j
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol star), (D $ DSymbol j, DState stateTransition)),
        ((DState stateTransition, DSymbol j), (DebuggingTMTypes.R, DState newState))]) quadsPos

    -- block for qWriteKTJFJCounter
    one = "1"
    symbolsToQWriteKTJFJleftBracket = concatMap (\(k, t, j, f) -> let
        oldState = qWriteCounter ++ k ++ t ++ j ++ f ++ j
        newState = qWrite ++ k ++ t ++ j ++ f ++ j ++ leftBracket
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol one, DState stateTransition)),
        ((DState stateTransition, DSymbol one), (DebuggingTMTypes.R, DState newState))]) quads

    -- block for qWriteKJFJSymbol
    symbolsToQWriteKTJFJSymbol = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ j ++ leftBracket
        newState = qWrite ++ k ++ t ++ j ++ f ++ j ++ letter
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol leftBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol leftBracket), (DebuggingTMTypes.R, DState newState))]) quads

     -- block for qWriteKTJFJSymbol
     -- creating empty space for first word letter, which will move here after
    symbolsToQWriteKTJFJRightBracket = map (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ j ++ letter
        newState = qWrite ++ k ++ t ++ j ++ f ++ j ++ rightBracket in
        ((DState oldState, DSymbol space), (DebuggingTMTypes.R, DState newState))) quads

    -- block for qWriteKTJFJRightBracket
    symbolsToQKTJFJletterWriting = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ j ++ rightBracket
        newState = qLetterWriting ++ k ++ t ++ j ++ f ++ j
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol rightBracket), (DebuggingTMTypes.R, DState newState))]) quads

    -- block for qLetterWritingKTJFJs'
    quintets = concatMap (\(k, t, j, f) -> map (k, t, j, f,) terminalsList) quads
    symbolsInQLetterWritingKTJFJ = map (\(k, t, j, f) -> let
        state = qLetterWriting ++ k ++ t ++ j ++ f ++ j in
        ((DState state, DSymbol space), (DebuggingTMTypes.R, DState state))) quads
    symbolsToQletterWritingKTJFJs' = concatMap (\(k, t, j, f, s) -> let
        oldState = qLetterWriting ++ k ++ t ++ j ++ f ++ j
        newState = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s ++ comma
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol s), (D $ DSymbol space, DState stateTransition)),
        ((DState stateTransition, DSymbol space), (DebuggingTMTypes.L, DState newState))]) quintets

    -- block for qLetterWritingKTJFJs'
    skipSymbols = space : [rightBracket]
    symbolsInQLetterWritingKTJFJs' = concatMap (\(k, t, j, f, s) -> map (\skipSymbol -> let
        state = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s ++ comma in
        ((DState state, DSymbol skipSymbol), (DebuggingTMTypes.L, DState state))) skipSymbols) quintets
    symbolsToQletterWritingKTJFJs = map (\(k, t, j, f, s) -> let
        oldState = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s ++ comma
        newState = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s in
        ((DState oldState, DSymbol leftBracket), (D $ DSymbol space, DState newState))) quintets

    -- block for qLetterWritingKTJF
    symbolsToQWriteKTJFF = concatMap (\(k, t, j, f, s) -> let
        oldState = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s
        newState = qWrite ++ k ++ t ++ j ++ f ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol s, DState stateTransition)),
        ((DState stateTransition, DSymbol s), (DebuggingTMTypes.R, DState newState))]) quintets

    -- block for qWriteKTJFF
    symbolsInQWriteKTJFF = map (\(k, t, j, f) -> let
        state = qWrite ++ k ++ t ++ j ++ f ++ f in
        ((DState state, DSymbol leftBracket), (DebuggingTMTypes.R, DState state))) quads
    symbolsToQWriteCounterKTJFF = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ f
        newState = qWriteCounter ++ k ++ t ++ j ++ f ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol f, DState stateTransition)),
        ((DState stateTransition, DSymbol f), (DebuggingTMTypes.R, DState newState))]) quads

    --block for qWriteCounterKTJFF
    symbolsToQWriteKTJFFleftBracket = concatMap (\(k, t, j, f) -> let
        oldState = qWriteCounter ++ k ++ t ++ j ++ f ++ f
        newState = qWrite ++ k ++ t ++ j ++ f ++ f ++ leftBracket
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol one, DState stateTransition)),
        ((DState stateTransition, DSymbol one), (DebuggingTMTypes.R, DState newState))]) quads

    --block for qWriteKTJFFleftBracket
    symbolsToQWriteKTJFFRightBracketPreparation = concatMap (\(k, t, j, f) -> let
        oldState = qWriteCounter ++ k ++ t ++ j ++ f ++ f
        newState = qWrite ++ k ++ t ++ j ++ f ++ f ++ rightBracket ++ preparation
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol space), (D $ DSymbol leftBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol leftBracket), (DebuggingTMTypes.R, DState newState))]) quads

    --block for qWriteKTJFFRightBracketPreparation
    symbolsInQWriteKTJFFRightBracketPreparation = map (\(k, t, j, f, s) -> let
        state = qWrite ++ k ++ t ++ j ++ f ++ f ++ rightBracket ++ preparation in
        ((DState state, DSymbol s), (DebuggingTMTypes.R, DState state))) quintets
    symbolsToQKTJFFRightBracketShiftWord = map (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ f ++ rightBracket ++ preparation
        newState = qWrite ++ k ++ t ++ j ++ f ++ f ++ rightBracket ++ shiftWord in
        ((DState oldState, DSymbol rightBracket), (DebuggingTMTypes.L, DState newState))) quads
    
    --block for qWriteKTJFFRightBracketShiftWord -- transition state for moving to qRememberStartKTJF block
    -- we've generated in previous block case for shifting ')' on one cell, now we use it to move all
    -- symbols, which are more right then letters of word, to put ')' after word
    symbolsToQKTJFShiftWord = map (\(k, t, j, f, s) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ f ++ rightBracket ++ shiftWord
        newState = qRememberStart ++ k ++ t ++ j ++ f in
        ((DState oldState, DSymbol s), (DebuggingTMTypes.R, DState newState))) quintets

    quadruples = symbolsInQWritingRelationKTJF ++ symbolsFromQWritingRelationToQWriteKTJFT
        ++ symbolsFromQWriteKTJFJtoQWriteCounterKTJFJ
        ++ symbolsFromQWritingRelationToQWriteCounterKTJFT
        ++ symbolsToQWriteKTJFJleftBracket ++ symbolsToQWriteKTJFJSymbol
        ++ symbolsToQWriteKTJFJRightBracket ++ symbolsToQKTJFJletterWriting
        ++ symbolsToQletterWritingKTJFJs' ++ symbolsInQLetterWritingKTJFJ
        ++ symbolsInQLetterWritingKTJFJs' ++ symbolsToQletterWritingKTJFJs
        ++ symbolsToQWriteKTJFF ++ symbolsInQWriteKTJFF ++ symbolsToQWriteCounterKTJFF
        ++ symbolsToQWriteKTJFFleftBracket ++ symbolsToQWriteKTJFFRightBracketPreparation
        ++ symbolsInQWriteKTJFFRightBracketPreparation ++ symbolsToQKTJFFRightBracketShiftWord
        ++ symbolsToQKTJFShiftWord
    in (DQuadruples $ addCollectionToMap quadruples Map.empty)

generateBlockForMovingToNextConjunction :: Grammar -> DebuggingQuadruples
generateBlockForMovingToNextConjunction grammar@(Grammar (nonterminals, terminals, relations, _)) = let
    transition = "Transition"
    qShiftingFromFold = "qShiftingKTJFFromFold"
    qRememberStart = "qRememberStart"

    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    relsList = Set.toList relations
    maxNumber = calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [0..maxNumber - 1]
    plus = "+"
    minus = "-"
    signs = [plus, minus]
    leftBracket = "("
    rightBracket = ")"
    space = " "
    brackets = [leftBracket, rightBracket]
    -- helper calculations of quads
    --qauds in this block - all conjs, which has next conj
    indicesWithNonterms = map (\i ->
                (i, filter (\t -> kthRelForNonterminalLong relsList t i) nonterminalsList)) indices
    quads' = concatMap (uncurry $ calculateQuads grammar) indicesWithNonterms
    -- if conj has next conj, then function calculateNextConjunctionInSameRule gives (Just s),
    -- where s - next conj (SymbolsPair)
    quads = filter (\(k, t, j, f) -> if checkIfConjHasNeg grammar (k, t, j, f)
        then let
            pair = SymbolsPair (Nonterminal t, read k :: Int, True,
                N $ Nonterminal j, N $ Nonterminal f) in
            case calculateNextConjunctionInSameRule grammar pair of
            Just _ -> True
            Nothing -> False
        else let
            pair = SymbolsPair (Nonterminal t, read k :: Int, False,
                N $ Nonterminal j, N $ Nonterminal f) in
            case calculateNextConjunctionInSameRule grammar pair  of
            Just _ -> True
            Nothing -> False) quads'
    qFoldRightBracketLast = "qFoldRightBracketLast"
    qFindNewSubstitution = "qFindNewSubstitution"
    stateTransitionOld = qFoldRightBracketLast ++ transition ++ qFindNewSubstitution
    -- whole transition is ((DState qFoldRightBracketLast, DSymbol " "),(D $ DSymbol rightBracket, DState stateTransition))
    differentTransition1Key = (DState qFoldRightBracketLast, DSymbol " ")
    -- whole transition is ((DState stateTransition, DSymbol rightBracket),(DebuggingTMTypes.R, DState qFindNewSubstitution))
    differentTransition2Key = (DState stateTransitionOld, DSymbol rightBracket)
    (DQuadruples quadruplesMap'') = generateBlockForFolding grammar
    quadruplesMap' = Map.delete differentTransition1Key quadruplesMap''
    quadruplesMap = Map.delete differentTransition2Key quadruplesMap'

    generatedQuadruples = generateFoldingForMidCongs quadruplesMap quads

    -- generate special case for transition1 and transition2
    symbolsFromRightBracketLastToQFindNewSubstitution = concatMap (\(k, t, j, f) -> let
        oldState = qFoldRightBracketLast ++ k ++ t ++ j ++ f
        newState = qShiftingFromFold ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol " "),(D $ DSymbol rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol rightBracket),(DebuggingTMTypes.R, DState newState))]) quads

    symbolsInQShiftingFromFoldKTJF = concatMap (\(k, t, j, f) -> map (\s -> let
        state = qShiftingFromFold ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol s),(DebuggingTMTypes.L, DState state)))
        $ nonterminalsList ++ terminalsList ++ brackets ++ signs ++ [space]) quads
    symbolsToQKTJFRememberStart = map (\(k, t, j, f) -> let
        oldState = qShiftingFromFold ++ k ++ t ++ j ++ f
        newState = qRememberStart ++ k ++ t ++ j ++ f in
        ((DState oldState, DSymbol k),(DebuggingTMTypes.R, DState newState))) quads
    newQuadruples = addCollectionToMap
        (symbolsFromRightBracketLastToQFindNewSubstitution
        ++ symbolsToQKTJFRememberStart ++ symbolsInQShiftingFromFoldKTJF)
        Map.empty
    allQuadruplesMap = Map.union generatedQuadruples newQuadruples
    in DQuadruples allQuadruplesMap

generateFoldingForMidCongs :: Map.Map (DebuggingState, DebuggingSymbol) (a, DebuggingState)
        -> [([Char], [Char], [Char], [Char])]
        -> Map.Map (DebuggingState, DebuggingSymbol) (a, DebuggingState)
generateFoldingForMidCongs quadruples quads = let
    quadruplesList = map (\(k, t, j, f) ->
        Map.map (\(move, DState state) -> (move, DState $ state ++ k ++ t ++ j ++ f))
            (Map.mapKeys (\(DState state, DSymbol symbol) ->
                let updatedState = state ++ k ++ t ++ j ++ f in
                (DState updatedState, DSymbol symbol)) quadruples)) quads
   in Map.unions quadruplesList


getShiftsDecrements :: Int -> String -> [(String, String)]
getShiftsDecrements shiftSize symbol = let
    fstPair = [(symbol, show shiftSize)]
    indices = [1..shiftSize]
    midPairs = map (\i -> if
        i == 1 then (show i, symbol)
        else (show i, show (i - 1))
        ) indices
    in (fstPair ++ midPairs)

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


