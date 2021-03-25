{-# LANGUAGE TupleSections #-}

module Boolean2TM where

import GrammarType
import DebuggingTMTypes
import qualified Boolean2TMConstants as Constants
import qualified Boolean2TMHelpers as Helpers

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe

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
        quadruples11 = generateBlockForRefiningConjunctionDetails grammar
        quadruples12 = generateBlockCheckIfWordsSplitCanBeChanged grammar
        quadruples13 = generateBlockForChangingWord grammar
        quadruples14 = generateTransitionFromConjunctionResult grammar
        quadruples15 = generateBlockForGettingAccepted grammar in
        (DTM $ unionQuadruples [quadruples1, quadruples2, quadruples3, quadruples4,
        quadruples5, quadruples7, quadruples8, quadruples9, quadruples10, quadruples11,
        quadruples12, quadruples13, quadruples14, quadruples15])

-- naming for blocks of TM?
generateBlockForFindingNewSubstitution :: Grammar -> DebuggingQuadruples
generateBlockForFindingNewSubstitution grammar@(Grammar (nonterminals, terminals, _, _)) = let
  qFindNewSubstitution = "qFindNewSubstitution"
  qCheckIfNotCompleted = "qCheckIfNotCompleted"
  qSubstituteOrFold = "qSubstituteOrFold"
  qSkipCompletedNonterminal = "qSkipCompletedNonterminal"
  qCountWordLength' = "qCountWordLength'"
  qCountWordLength = "qCountWordLength"
  qMoveToEndToScanResults1 = "qMoveToEndToScanResults1"
  qFold' = "qFold'"
  done = "Done"

  terminalsList = Set.toList terminals
  nonterminalsList = Set.toList nonterminals
  signsList = Constants.signs
  negation = [Constants.negation]
  indices= map show [0..maxNumberOfRules - 1]

  maxNumberOfRules = Helpers.calculateMaxNumberOfRulesForNonterminal grammar

  -- BLOCK for qFindNewSubstitution
  list = concat [indices, map terminalValue terminalsList, signsList, negation, Constants.brackets]
  -- there is no necessity in insertWithKey, since TM is deterministic
  symbolsInQFindNewSubstitutionQdrs = map (\t -> ((DState qFindNewSubstitution, DSymbol t),
        (DebuggingTMTypes.L, DState qFindNewSubstitution))) list
  -- in TMTypes EmptySymbol is 0
  symbolsFromQFindNewSubstitutionToAcceptQdrs = [((DState qFindNewSubstitution, DSymbol Constants.space),(DebuggingTMTypes.R, DState done))]
  symbolsWhichChangeStateQdrs =
    map ((\t -> ((DState qFindNewSubstitution, DSymbol t), (DebuggingTMTypes.R, DState qCheckIfNotCompleted))) . nonterminalValue) nonterminalsList

  -- BLOCK for qCheckIfNotCompleted
  symbolsToQSubstituteOrFoldQdrs = map (\t -> 
    ((DState qCheckIfNotCompleted, DSymbol t),(DebuggingTMTypes.R, DState qSubstituteOrFold))) indices
  symbolsToQSkipCompletedNonterminalQdrs = map 
    (\t -> ((DState qCheckIfNotCompleted, DSymbol t),(DebuggingTMTypes.L, DState qSkipCompletedNonterminal))) signsList
  symbolsFromQCheckIfNotCompletedToAcceptQdrs = [((DState qCheckIfNotCompleted, DSymbol Constants.space),(DebuggingTMTypes.R, DState done))]

  -- BLOCK for qSubstituteOrFold
  symbolsInQSubstituteOrFoldQdrs = [((DState qSubstituteOrFold, DSymbol Constants.leftBracket),(DebuggingTMTypes.R, DState qSubstituteOrFold))]
  symbolsToCountWordLength'Qdrs = map
    ((\t -> ((DState qSubstituteOrFold, DSymbol t),(DebuggingTMTypes.L, DState qCountWordLength'))) . terminalValue) terminalsList
  symbolsToQFold'Qdrs = map 
    (\t -> ((DState qSubstituteOrFold, DSymbol t),(DebuggingTMTypes.L, DState qFold'))) $ map nonterminalValue nonterminalsList ++ negation

  -- BLOCK for qCountWordLength'
  symbolsToCountWordLengthQdrs = [((DState qCountWordLength', DSymbol Constants.leftBracket),(DebuggingTMTypes.L, DState qCountWordLength))]

  -- BLOCK for qFold'
  symbolsToQMoveToEndToScanResults1 = [((DState qFold', DSymbol Constants.leftBracket),(DebuggingTMTypes.L, DState qMoveToEndToScanResults1))]

  -- BLOCK for qSkipCompletedNonterminal
  symbolsToQFindNewSubstitutionQdrs = map 
    ((\t -> ((DState qSkipCompletedNonterminal, DSymbol t),(DebuggingTMTypes.L, DState qFindNewSubstitution))) . nonterminalValue) nonterminalsList

  quadruples = symbolsInQFindNewSubstitutionQdrs ++ symbolsFromQFindNewSubstitutionToAcceptQdrs ++ symbolsWhichChangeStateQdrs 
    ++ symbolsToQSubstituteOrFoldQdrs ++ symbolsToQSkipCompletedNonterminalQdrs ++ symbolsFromQCheckIfNotCompletedToAcceptQdrs
    ++ symbolsInQSubstituteOrFoldQdrs ++ symbolsToCountWordLength'Qdrs ++ symbolsToQFold'Qdrs ++ symbolsToCountWordLengthQdrs 
    ++ symbolsToQMoveToEndToScanResults1 ++ symbolsToQFindNewSubstitutionQdrs
  in DQuadruples (Helpers.addCollectionToMap quadruples Map.empty)

generateBlockForScanResults :: Grammar -> DebuggingQuadruples
generateBlockForScanResults grammar@(Grammar (nonterminals, terminals, _, _)) = let
    qMoveToEndToScanResults1 = "qMoveToEndToScanResults1"
    qMoveToEndToScanResults2 = "qMoveToEndToScanResults2"
    qStart' = "qStart'"
    qSecPos = "qSecPos"
    qSecNeg = "qSecNeg"
    qBothPos = "qBothPos"
    qSomeNeg = "qSomeNeg"
    qWordsChangingMoveToBringSymbol = "qWordsChangingMoveToBringSymbol"
    -- parts for set of states q1FindNegation, q2FindNegation...qkFindNegation
    q = "q"
    findNegation = "FindNegation"

    maxNumberOfRules = Helpers.calculateMaxNumberOfRulesForNonterminal grammar

    -- BLOCK for qMoveToEndAndScanResults
    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    indices = map show [0..maxNumberOfRules - 1]

    symbols = Constants.negation : terminalsList ++ nonterminalsList ++ indices ++ Constants.brackets
    -- BLOCK for qMoveToEndToScanResults1Qdrs
    symbolsInQMoveToEndToScanResults1Qdrs = map
        (\t -> ((DState qMoveToEndToScanResults1, DSymbol t),
            (DebuggingTMTypes.R, DState qMoveToEndToScanResults1))) symbols
    symbolsToQMoveToEndToScanResults2Qdrs = map (\t ->
        ((DState qMoveToEndToScanResults1, DSymbol t),
        (DebuggingTMTypes.R, DState qMoveToEndToScanResults2))) Constants.signs

    -- BLOCK for qMoveToEndToScanResults2Qdrs
    symbolsInQMoveToEndToScanResults2Qdrs = map
        (\t -> ((DState qMoveToEndToScanResults2, DSymbol t),
        (DebuggingTMTypes.R, DState qMoveToEndToScanResults2)))
        $ nonterminalsList ++ Constants.brackets ++ terminalsList
    symbolsToQStart'Qdrs = map (\t ->
        ((DState qMoveToEndToScanResults2, DSymbol t),
        (DebuggingTMTypes.R, DState qStart'))) Constants.signs

    -- BLOCK for qStart'
    symbolsInQStart'Qdrs = map
        (\t -> ((DState qStart', DSymbol t),(DebuggingTMTypes.L, DState qStart'))) $ Constants.brackets ++ terminalsList
    symbolsToQSecPosQdrs = [((DState qStart', DSymbol Constants.plus),(DebuggingTMTypes.L, DState qSecPos))]
    symbolsToQSecNegQdrs = [((DState qStart', DSymbol Constants.minus),(DebuggingTMTypes.L, DState qSecNeg))]

    -- BLOCK for qSecPos
    symbolsInQSecPosQdrs = map
        (\t -> ((DState qSecPos, DSymbol t),(DebuggingTMTypes.L, DState qSecPos))) $ Constants.brackets ++ terminalsList ++ nonterminalsList
    symbolsToQBothPosQdrs = [((DState qSecPos, DSymbol "+"),(DebuggingTMTypes.L, DState qBothPos))]
    symbolsFromQSecPosToQSomeNegQdrs = [((DState qSecPos, DSymbol "-"),(DebuggingTMTypes.L, DState qSomeNeg))]

    -- BLOCK for qSecNeg: there is possibility to remove this state and move to qSomeNeg state
    symbolsInQSecNegQdrs = map
        (\t -> ((DState qSecNeg, DSymbol t),(DebuggingTMTypes.L, DState qSecNeg))) $ Constants.brackets ++ terminalsList ++ nonterminalsList
    symbolsFromQSecNegToQSomeNegQdrs =
      [((DState qSecNeg, DSymbol "+"),(DebuggingTMTypes.L, DState qSomeNeg)),
      ((DState qSecNeg, DSymbol "-"),(DebuggingTMTypes.L, DState qSomeNeg))]

    -- BLOCK for qBothPos
    symbolsInQBothPosQdrs = map
        (\t -> ((DState qBothPos, DSymbol t),(DebuggingTMTypes.L, DState qBothPos)))
        $ Constants.leftBracket : Constants.negation : nonterminalsList
    symbolsToQ_ithFindNegationQdrs = map
        (\t -> ((DState qBothPos, DSymbol t),(DebuggingTMTypes.R, DState $ q ++ t ++ findNegation))) indices

    -- BLOCK for qSomeNeg
    symbolsInSomeNegQdrs = map
        (\t -> ((DState qSomeNeg, DSymbol t),(DebuggingTMTypes.L, DState qSomeNeg)))
        $ Constants.leftBracket : Constants.negation : nonterminalsList
    symbolsToQWordsChangingMoveToBringSymbolQdrs = map
        (\t -> ((DState qSomeNeg, DSymbol t),(DebuggingTMTypes.R, DState qWordsChangingMoveToBringSymbol))) indices

    quadruples = symbolsInQMoveToEndToScanResults1Qdrs ++ symbolsToQMoveToEndToScanResults2Qdrs
        ++  symbolsInQMoveToEndToScanResults2Qdrs ++ symbolsToQStart'Qdrs ++ symbolsInQStart'Qdrs
        ++ symbolsToQSecPosQdrs ++ symbolsToQSecNegQdrs ++ symbolsInQSecPosQdrs ++ symbolsToQBothPosQdrs
        ++ symbolsFromQSecPosToQSomeNegQdrs ++ symbolsInQSecNegQdrs ++ symbolsFromQSecNegToQSomeNegQdrs
        ++ symbolsInQBothPosQdrs ++ symbolsToQ_ithFindNegationQdrs ++ symbolsInSomeNegQdrs
        ++ symbolsToQWordsChangingMoveToBringSymbolQdrs

    in DQuadruples (Helpers.addCollectionToMap quadruples Map.empty)

generateBlockForRefiningConjunctionDetails :: Grammar -> DebuggingQuadruples
generateBlockForRefiningConjunctionDetails grammar = let
    maxNumberOfRules = Helpers.calculateMaxNumberOfRulesForNonterminal grammar
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

    -- BLOCK for qkFindNegation
    symbolsInQkFindNegation = [((DState qkFindNegation, DSymbol Constants.leftBracket),
        (DebuggingTMTypes.R, DState qkFindNegation))]
    symbolsToQkMoveToStart = map
        (\t -> ((DState qkFindNegation, DSymbol t),(DebuggingTMTypes.L, DState qkMoveToStart))) nonterminalsList
    symbolsToQkMoveToStartNegation = [((DState qkFindNegation, DSymbol Constants.negation),
        (DebuggingTMTypes.L, DState qkMoveToStartNeg))]

    -- BLOCK for qkMoveToStart/qkMoveToStartNeg
    symbolsInQkMoveToStart = [((DState qkMoveToStart, DSymbol Constants.leftBracket),(DebuggingTMTypes.L, DState qkMoveToStart))]
    symbolsToQRulekFindNonterminal = [((DState qkMoveToStart, DSymbol k'),(DebuggingTMTypes.L, DState qRuleKFindNonterminal))]

    -- BLOCK for qRulekFindNonterminal
    nonterminalsWithKthRel = filter (\t -> Helpers.kthRelForNonterminalLong relsList t k') nonterminalsList
    symbolsToQRulekNonterminalFindFst = map (\t ->
            ((DState qRuleKFindNonterminal, DSymbol t),(DebuggingTMTypes.R, DState $ q ++ "Rule" ++ k' ++ t ++ "findFst")))
            nonterminalsWithKthRel

    -- BLOCK for qRulektFindFst
    symbolsInQRulektFindFst = concatMap (\t -> map (\s ->
            ((DState $ q ++ "Rule" ++ k' ++ t ++ "findFst", DSymbol s),
            (DebuggingTMTypes.R, DState $ q ++ "Rule" ++ k' ++ t ++ "findFst")))
            $ k' : Constants.leftBracket : [Constants.negation]) nonterminalsList

    triplets = Helpers.calculateTriplets grammar k' nonterminalsWithKthRel

    symbolsToQRulektjFindSnd = map
        (\(k, t, j) -> ((DState $ q ++ "Rule" ++ k ++ t ++ "findFst", DSymbol j),
            (DebuggingTMTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd"))) triplets

    -- BLOCK for qRulektjFindSnd
    quads = Helpers.calculateAllQuads grammar
    symbolsInRulektjFindSnd = concatMap (\s ->
        map (\(k, t, j) ->
            ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol s),
             (DebuggingTMTypes.R, DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd"))
            ) triplets) $ terminalsList ++ Constants.signs ++ Constants.brackets

    symbolsToQRulektjs = map (\(k,t,j,f) -> ((DState $ q ++ "Rule" ++ k ++ t ++ j ++ "findSnd", DSymbol f),
            (DebuggingTMTypes.L, DState $ q ++ "Rule" ++ k ++ t ++ j ++ f))) quads

    quadruples = symbolsToQRulekFindNonterminal ++ symbolsToQRulekNonterminalFindFst
            ++ symbolsInQRulektFindFst ++ symbolsToQRulektjFindSnd ++ symbolsInRulektjFindSnd ++ symbolsToQRulektjs
    commonQuadruples = symbolsInQkFindNegation ++ symbolsToQkMoveToStart
            ++ symbolsToQkMoveToStartNegation ++ symbolsInQkMoveToStart

    quadruplesMap = Helpers.addCollectionToMap quadruples Map.empty
    quadruplesNegMap = generateCaseForNegConj quadruplesMap
    commonQuadruplesMap = Helpers.addCollectionToMap commonQuadruples Map.empty

    in DQuadruples (Map.union commonQuadruplesMap $ Map.union quadruplesMap quadruplesNegMap)

generateCaseForNegConj :: Map.Map (DebuggingState, DebuggingSymbol) (DebuggingMove, DebuggingState)
                       -> Map.Map (DebuggingState, DebuggingSymbol) (DebuggingMove, DebuggingState)
generateCaseForNegConj quadruples = let
    quadruples' = Map.mapKeys (\(DState state, DSymbol symbol) -> (DState $ state ++ "Negation", DSymbol symbol)) quadruples
    in Map.map (\(move, DState state) -> (move, DState $ state ++ "Negation")) quadruples'

--BLOCK for moving from conjunction results to next blocks
generateTransitionFromConjunctionResult :: Grammar -> DebuggingQuadruples
generateTransitionFromConjunctionResult grammar@(Grammar (nonterminals, terminals, _, _)) = let
    q = "q"
    rewrite = "Rewrite"
    transition = "Transition"
    skipParentNonterminal = "SkipParentNonterminal"
    rule = "Rule"
    negation = "Negation"
    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals

    quads = Helpers.calculateAllQuads grammar

    -- BLOCK FOR qRulektjs
    quadruplesInRulektjs' = concatMap (\(k,t,j,f) -> concatMap (\s ->
        [((DState $ q ++ rule ++ k ++ t ++ j ++ f, DSymbol s),
        (DebuggingTMTypes.L, DState $ q ++ rule ++ k ++ t ++ j ++ f)),
        ((DState $ q ++ rule ++ k ++ t ++ j ++ f ++ negation, DSymbol s),
        (DebuggingTMTypes.L, DState $ q ++ rule ++ k ++ t ++ j ++ f ++ negation))])
        $ Constants.negation : Constants.brackets ++ Constants.signs ++ terminalsList ++ nonterminalsList) quads

    quadruplesInRulektjs'' = concatMap (\(k,t,j,s) ->
        [((DState $ q ++ rule ++ k ++ t ++ j ++ s ++ negation, DSymbol k),
        (DebuggingTMTypes.L, DState $ q ++ rule ++ k ++ t ++ j ++ s ++ negation)),
        ((DState $ q ++ rule ++ k ++ t ++ j ++ s ++ negation, DSymbol k),
        (DebuggingTMTypes.L, DState $ q ++ rule ++ k ++ t ++ j ++ s ++ negation))]) quads

    quadruplesInRulektjs = quadruplesInRulektjs' ++ quadruplesInRulektjs''

    -- BLOCK, which generates transitions to next subprograms - move to next rule, folding conj with sign, changing words
    -- fold and put +
    lastConjsWithoutNeg = filter (\t -> let
        hasNeg = Helpers.checkIfConjHasNeg grammar t
        pair = Helpers.constructSymbolsPairByQuad t hasNeg
        in isNothing (Helpers.calculateNextConjunctionInSameRule grammar pair)) quads
    quadruplesToRewritePlus = map (\(k,t,j,s) -> ((DState $ q ++ rule ++ k ++ t ++ j ++ s, DSymbol k),
                    (DebuggingTMTypes.L, DState $ q ++ rewrite ++ Constants.plus))) lastConjsWithoutNeg

    -- move to next conjunction
    midConjsWihoutNeg = filter (\t -> let
        hasNeg = Helpers.checkIfConjHasNeg grammar t
        pair = Helpers.constructSymbolsPairByQuad t hasNeg
        in isJust (Helpers.calculateNextConjunctionInSameRule grammar pair)) quads
    quadruplesToNextConj = map (\(k,t,j,s) -> let
        oldState = q ++ rule ++ k ++ t ++ j ++ s
        newState = q ++ skipParentNonterminal ++ k ++ t ++ j ++ s in
        ((DState oldState, DSymbol k),
        (DebuggingTMTypes.R, DState newState))) midConjsWihoutNeg

    -- move to next relation
    conjsInMidRuleWithNeg = filter (\t -> let
        hasNeg = Helpers.checkIfConjHasNeg grammar t
        pair = Helpers.constructSymbolsPairByQuad t hasNeg
        in isJust (Helpers.calculateFirstConjunctionInNextRule grammar pair)) quads
    quadruplesToNextRel = concatMap (\(k,t,j,s) -> let
        inc = DSymbol (show $ (read k :: Int) + 1)
        oldState = q ++ rule ++ k ++ t ++ j ++ s ++ negation
        newState = q ++ skipParentNonterminal
        stateTransition = oldState ++ transition ++ newState
        in [((DState oldState, DSymbol k), (D inc, DState stateTransition)),
        ((DState stateTransition, inc),(DebuggingTMTypes.R, DState newState))])
        conjsInMidRuleWithNeg

    -- fold and put -
    conjsInLastRuleWithNeg = filter (\t -> let
        hasNeg = Helpers.checkIfConjHasNeg grammar t
        pair = Helpers.constructSymbolsPairByQuad t hasNeg
        in isNothing (Helpers.calculateFirstConjunctionInNextRule grammar pair)) quads
    quadruplesToRewriteMinus = map (\(k,t,j,s) ->
        ((DState $ q ++ rule ++ k ++ t ++ j ++ s ++ negation, DSymbol k),
        (DebuggingTMTypes.L, DState $ q ++ rewrite ++ Constants.minus))) conjsInLastRuleWithNeg

    quadruples = quadruplesInRulektjs ++ quadruplesToRewritePlus ++ quadruplesToRewriteMinus
        ++ quadruplesToNextConj ++ quadruplesToNextRel 

    in (DQuadruples $ Helpers.addCollectionToMap quadruples Map.empty)

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
    maxNumberOfRules = Helpers.calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [0..maxNumberOfRules - 1]

    --BLOCK for qWordsChangingMoveToBringSymbol
    symbolsInMoveToBringSymbol = map
        (\t -> ((DState moveToBringSymbol, DSymbol t), (DebuggingTMTypes.R, DState moveToBringSymbol)))
        $ Constants.negation : Constants.brackets
    symbolsToMetFstNonterminalQdrs = map
        (\t -> ((DState moveToBringSymbol, DSymbol t), (DebuggingTMTypes.R, DState metFstNonterminal))) nonterminalsList

    --BLOCK for qWordsChangingMetFstNonterminal
    symbolsInMetFstNonterminal = map
        (\t -> ((DState metFstNonterminal, DSymbol t), (DebuggingTMTypes.R, DState metFstNonterminal)))
        $ Constants.brackets ++ Constants.signs ++ terminalsList
    symbolsToMetSndNonterminal = map
        (\t -> ((DState metFstNonterminal, DSymbol t), (DebuggingTMTypes.R, DState metSndNonterminal))) nonterminalsList

    --BLOCK for qWordsChangingMetSndNonterminal
    symbolsInMetSndNonterminal = map
        (\t -> ((DState metSndNonterminal, DSymbol t), (DebuggingTMTypes.R, DState metSndNonterminal))) Constants.signs
    symbolsToCheckIfSndIsWord = map
        (\t -> ((DState metSndNonterminal, DSymbol t), (DebuggingTMTypes.R, DState checkIfSndIsWord))) [Constants.leftBracket]

    --BLOCK for qWordsChangingCheckIfSndIsWord
    symbolsToSndIsWord = map
        (\t -> ((DState checkIfSndIsWord, DSymbol t), (DebuggingTMTypes.R, DState sndIsWord))) terminalsList

    --BLOCK for qWordsChangingSndIsWord
    symbolsToReturnToParentNonterminal = map
        (\t -> ((DState sndIsWord, DSymbol t), (DebuggingTMTypes.L, DState returnToParentNonterminal))) [Constants.rightBracket]
    symbolsToBringSymbol = map
        (\t -> ((DState sndIsWord, DSymbol t), (DebuggingTMTypes.L, DState bringSymbol))) terminalsList

    --BLOCK for qWordsChangingReturnToParentNonterminal
    notCounters = Constants.negation : nonterminalsList ++ terminalsList ++ Constants.brackets ++ Constants.signs
    symbolsInReturnToParentNonterminal = map
        (\t -> ((DState returnToParentNonterminal, DSymbol t),
        (DebuggingTMTypes.L, DState returnToParentNonterminal))) notCounters
    symbolsToWordsChangingFailed = map
        (\counter ->  ((DState returnToParentNonterminal, DSymbol counter),
        (DebuggingTMTypes.R, DState $ failedCheckNegation ++ counter))) indices

    --BLOCK for qWordsChangingFailedCheckNegationK
    symbolsInWordsChangingFailed = map
        (\counter -> ((DState $ failedCheckNegation ++ counter, DSymbol Constants.leftBracket),
        (DebuggingTMTypes.R, DState $ failedCheckNegation ++ counter))) indices
    symbolsToWordsChangingFailedNegationK = map
        (\counter -> ((DState $ failedCheckNegation ++ counter, DSymbol Constants.negation),
        (DebuggingTMTypes.L, DState $ failedNegation ++ counter))) indices
    symbolsToWordsChangingFailedK = concatMap
        (\counter -> map (\nonterm -> ((DState $ failedCheckNegation ++ counter, DSymbol nonterm),
        (DebuggingTMTypes.L, DState $ failed ++ counter))) nonterminalsList) indices

    --BLOCK for qWordsChangingFailedK
    symbolsToQkMoveToStart = map
        (\counter -> ((DState $ failed ++ counter, DSymbol Constants.leftBracket),
        (DebuggingTMTypes.L, DState $ "q" ++ counter ++ moveToStart ++ negationWord))) indices

    --BLOCK for qWordsChangingFailedNegationK
    symbolsToQkMoveToStartNegation = map
        (\counter -> ((DState $ failedNegation ++ counter, DSymbol Constants.leftBracket),
        (DebuggingTMTypes.L, DState $ "q" ++ counter ++ moveToStart))) indices

    quadruples = --symbolsInQWordsChangingMoveToBringSymbol ++ symbolsToQMoveToBringSymbol
        symbolsInMoveToBringSymbol ++ symbolsToMetFstNonterminalQdrs ++ symbolsInMetFstNonterminal
        ++ symbolsToMetSndNonterminal ++ symbolsInMetSndNonterminal ++ symbolsToCheckIfSndIsWord ++ symbolsToSndIsWord
        ++ symbolsToReturnToParentNonterminal ++ symbolsToBringSymbol ++ symbolsInReturnToParentNonterminal
        ++ symbolsToWordsChangingFailed ++ symbolsInWordsChangingFailed ++ symbolsToWordsChangingFailedNegationK
        ++ symbolsToWordsChangingFailedK ++ symbolsToQkMoveToStart ++ symbolsToQkMoveToStartNegation

    in (DQuadruples $ Helpers.addCollectionToMap quadruples Map.empty)

generateBlockForChangingWord :: Grammar -> DebuggingQuadruples
generateBlockForChangingWord (Grammar (nonterminals, terminals, _, _)) = let
    qWordsChanging = "qWordsChanging"
    bringSymbol = qWordsChanging ++ "BringSymbol"
    broughtSymbol = qWordsChanging ++ "BroughtSymbol"
    write = qWordsChanging ++ "Write"
    transition = "transition"
    writeBroughtSymbol = qWordsChanging ++ "WriteBroughtSymbol"
    createCounterForFstNonterminal = qWordsChanging ++ "CreateCounterForFstNonterminal"
    createCounterForSndNonterminal = qWordsChanging ++ "CreateCounterForSndNonterminal"
    moveToEnd = qWordsChanging ++ "moveToEnd"
    qFindNewSubstitution = "qFindNewSubstitution"
    fstCounter = "0"

    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals

    --BLOCK for qWordsChangingBringSymbol
    symbolsToBroughtSymbolt = concatMap (\t -> let
        oldState = bringSymbol
        newState = broughtSymbol ++ t
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol t),(D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.L, DState newState))
        ]) terminalsList
    --BLOCK for qWordsChangingBringSymbolt (list of states generated for each terminal t)
    symbolsInBroughtSymbolt = map (\t ->
        ((DState $ broughtSymbol ++ t, DSymbol Constants.space),(DebuggingTMTypes.L, DState $ broughtSymbol ++ t)))
        terminalsList
    remembered = Constants.signs ++ nonterminalsList ++ Constants.brackets
    pairs = concatMap (\t -> map (t,) remembered) terminalsList
    symbolsToWritetk = concatMap (\(t, k) -> let
        oldState = broughtSymbol ++ t
        newState = write ++ t ++ k
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol k),(D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.R, DState newState))]) pairs
    --BLOCK for qWordsChangingWritetk
    remembered' = Constants.signs ++ nonterminalsList ++ [Constants.leftBracket]
    pairs' = concatMap (\t -> map (t,) remembered') terminalsList
    symbolsToBroughtSymbolt' = concatMap (\(t, k) -> let
        oldState = write ++ t ++ k
        newState = broughtSymbol ++ t
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space),(D $ DSymbol k, DState stateTransition)),
        ((DState stateTransition, DSymbol k),(DebuggingTMTypes.L, DState newState))
        ]) pairs'
    -- special case for )
    symbolsToWriteBroughtSymbolt = concatMap (\t -> let
        oldState = write ++ t ++ Constants.rightBracket
        newState = writeBroughtSymbol ++ t
        stateTransition = oldState ++ transition ++ newState
        in
        [((DState oldState, DSymbol Constants.space),(D $ DSymbol Constants.rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.rightBracket),(DebuggingTMTypes.L, DState newState))]) terminalsList

    --BLOCK for qWordsChangingWriteBroughtSymbolt
    symbolsToCreateCounterForFstNonterminal = concatMap (\t -> let
        oldState = writeBroughtSymbol ++ t
        newState = createCounterForFstNonterminal
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space),(D $ DSymbol t, DState stateTransition)),
        ((DState stateTransition, DSymbol t),(DebuggingTMTypes.L, DState newState))]) terminalsList

    --BLOCK for qWordsChangingCreateCounterForFstNonterminal
    symbolsInCreateCounterForFstNonterminal = map (\t ->
        ((DState createCounterForFstNonterminal, DSymbol t),
        (DebuggingTMTypes.L, DState createCounterForFstNonterminal)))
        $ Constants.leftBracket : terminalsList
    symbolsToCreateCounterForSndNonterminal = concatMap (\t -> let
            oldState = createCounterForFstNonterminal
            newState = createCounterForSndNonterminal
            stateTransition = oldState ++ transition  ++ newState in
            [((DState oldState, DSymbol t),(D $ DSymbol fstCounter, DState stateTransition)),
            ((DState stateTransition, DSymbol fstCounter),(DebuggingTMTypes.R, DState newState))
            ]) Constants.signs
    symbolsInCreateCounterForSndNonterminal = map (\t ->
         ((DState createCounterForSndNonterminal, DSymbol t),
         (DebuggingTMTypes.R, DState createCounterForSndNonterminal)))
         $ Constants.leftBracket : Constants.rightBracket : terminalsList ++ nonterminalsList
    symbolsToMoveToEnd = concatMap (\t -> let
        oldState = createCounterForSndNonterminal
        newState = moveToEnd
        stateTransition = oldState ++ transition  ++ newState in
        [((DState oldState, DSymbol t),(D $ DSymbol fstCounter, DState stateTransition)),
        ((DState stateTransition, DSymbol fstCounter),(DebuggingTMTypes.R, DState newState))
        ]) Constants.signs

    --BLOCK for qWordsChangingMoveToEnd
    symbols = fstCounter  : Constants.brackets ++ terminalsList ++ nonterminalsList
    symbolsInMoveToEnd = map (\t -> ((DState moveToEnd, DSymbol t),(DebuggingTMTypes.R, DState moveToEnd))) symbols
    symbolsToQFindNewSubstitution = map (\t -> 
        ((DState moveToEnd, DSymbol t),(DebuggingTMTypes.L, DState qFindNewSubstitution))) $ Constants.space : Constants.signs

    quadruples = symbolsToBroughtSymbolt ++ symbolsInBroughtSymbolt ++ symbolsToWritetk
        ++ symbolsToBroughtSymbolt' ++ symbolsToWriteBroughtSymbolt ++ symbolsToCreateCounterForFstNonterminal
        ++ symbolsInCreateCounterForFstNonterminal ++ symbolsToCreateCounterForSndNonterminal 
        ++ symbolsInCreateCounterForSndNonterminal ++ symbolsToMoveToEnd ++ symbolsInMoveToEnd
        ++ symbolsToQFindNewSubstitution

    in (DQuadruples $ Helpers.addCollectionToMap quadruples Map.empty)

generateBlockForWritingSigns :: Grammar -> DebuggingQuadruples
generateBlockForWritingSigns grammar@(Grammar (nonterminals, _, rels,_)) = let
    qRewrite = "qRewrite"
    qRewriteWithMinus = "qRewriteWithMinus"
    qFindNewSubstitution = "qFindNewSubstitution"
    transition = "transition"
    qSkipParentNonterminal = "qSkipParentNonterminal"
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals

  --BLOCK for qRewriteNWithWord (N - nonterminal)
  -- case, when current rule has one terminal in right part: applying this rule to word with more
  -- than 1 symbol is impossible
    nonterminalsWithIndices = Map.toList $ Helpers.getNumbersOfShortRelations grammar
    symbolsToQFindNewSubstitution = concatMap (\(nonterm'@(Nonterminal nonterm), indices) -> concatMap (\i -> let
        oldState = qRewriteWithMinus ++ nonterm
        newState = qFindNewSubstitution
        stateTransition = oldState ++ transition ++ newState
        relsForNonterm = Helpers.calculateGroupRelationsByNonterminals (Set.toList rels) Map.! nonterm'
        in
        if (read i :: Int) == length relsForNonterm - 1
            then
            [((DState oldState, DSymbol i),(D $ DSymbol Constants.minus, DState stateTransition)),
            ((DState stateTransition, DSymbol Constants.minus),(DebuggingTMTypes.L, DState newState))]
            else
            let nextI = show (1 + read i :: Int) in
            [((DState oldState, DSymbol i),(D $ DSymbol nextI, DState stateTransition)),
            ((DState stateTransition, DSymbol nextI),(DebuggingTMTypes.L, DState newState))])
         indices) nonterminalsWithIndices

    --BLOCK for qRewriteMinus or qRewritePlus
    pairs = concatMap (\sign -> map (sign,) nonterminalsList) Constants.signs
    symbolsToQRewriteNSign = map (\(sign, nonterm) -> let
        newState =  qRewrite ++ nonterm ++ sign in
        ((DState $ qRewrite ++ sign, DSymbol nonterm),(DebuggingTMTypes.R, DState newState))) pairs

    --BLOCK for qRewriteNSign
    maxNumber = Helpers.calculateMaxNumberOfRulesForNonterminal grammar
    possibleIndices = map show [0..maxNumber - 1]
    symbolsToQSkipParentNonterminal = concatMap (\(sign, nonterm) -> let
        oldState = qRewrite ++ nonterm ++ sign
        newState = qSkipParentNonterminal
        stateTransition = oldState ++ transition ++ newState in
        concatMap (\index ->
            [((DState oldState, DSymbol index),(D $ DSymbol sign, DState stateTransition)),
             ((DState stateTransition, DSymbol sign),(DebuggingTMTypes.R, DState newState))])
             possibleIndices) pairs

    quadruples = symbolsToQFindNewSubstitution ++ symbolsToQRewriteNSign ++ symbolsToQSkipParentNonterminal
    in (DQuadruples $ Helpers.addCollectionToMap quadruples Map.empty)

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

    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals

    --BLOCK for qSkipParentNonterminal
    symbolsToQRemoveSymbols = [((DState qSkipParentNonterminal, DSymbol Constants.leftBracket),(DebuggingTMTypes.R, DState qRemoveSymbols))]

    --BLOCK for qRemoveSymbols
    symbolsInQRemoveSymbols = concatMap (\t -> let
        stateTransition = qRemoveSymbols ++ transition in
        [((DState qRemoveSymbols, DSymbol t),(D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.R, DState qRemoveSymbols))]) $ Constants.negation : nonterminalsList
    symbolsToQRemoveBracketAroundFstWord = concatMap (\sign -> let
        stateTransition = qRemoveSymbols ++ transition ++ qRemoveBracketsAroundFstWord in
        [((DState qRemoveSymbols, DSymbol sign),(D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.R, DState qRemoveBracketsAroundFstWord))]) Constants.signs

    --BLOCK for qRemoveBracketsAroundFstWord
    symbolsInQRemoveBracketsAroundFstWord' = let
        state = qRemoveBracketsAroundFstWord
        stateTransition = state ++ transition ++ state
        in [((DState state, DSymbol Constants.leftBracket),(D $ DSymbol Constants.space, DState stateTransition)),
            ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.R, DState state))]
    symbolsInQRemoveBracketsAroundFstWord'' = map (\t -> let
            state = qRemoveBracketsAroundFstWord in
            ((DState state, DSymbol t),(DebuggingTMTypes.R, DState state))) terminalsList
    symbolsInQRemoveBracketsAroundFstWord =
      symbolsInQRemoveBracketsAroundFstWord' ++ symbolsInQRemoveBracketsAroundFstWord''
    symbolsToQRemoveBracketsAroundSndWord = let
        oldState = qRemoveBracketsAroundFstWord
        newState = qRemoveBracketsAroundSndWord
        stateTransition = oldState ++ transition ++ newState
        in [((DState oldState, DSymbol Constants.rightBracket),(D $ DSymbol Constants.space, DState stateTransition)),
            ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.R, DState newState))]

    --BLOCK for qRemoveBracketsAroundSndWord
    symbolsInQRemoveBracketsAroundSndWord' = concatMap (\t -> let
        state = qRemoveBracketsAroundSndWord
        stateTransition = state ++ transition ++ state
        in [((DState state, DSymbol t),(D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.R, DState state))])
        $ nonterminalsList ++ Constants.signs ++ [Constants.leftBracket]
    symbolsInQRemoveBracketsAroundSndWord'' = map (\t -> let
        state = qRemoveBracketsAroundSndWord in
        ((DState state, DSymbol t),(DebuggingTMTypes.R, DState state))) terminalsList
    symbolsInQRemoveBracketsAroundSndWord =
      symbolsInQRemoveBracketsAroundSndWord' ++ symbolsInQRemoveBracketsAroundSndWord''
    symbolsToQMoveToStart = let
        oldState = qRemoveBracketsAroundSndWord
        newState = qMoveToStart
        stateTransition = oldState ++ transition ++ newState
        in [((DState oldState, DSymbol Constants.rightBracket),(D $ DSymbol Constants.space, DState stateTransition)),
            ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.L, DState newState))]

    --BLOCK for qMoveToStart
    symbolsInQMoveToStart = map (\t -> ((DState qMoveToStart, DSymbol t),
        (DebuggingTMTypes.L, DState qMoveToStart))) $ terminalsList ++ [Constants.space]
    symbolsToQFold = [((DState qMoveToStart, DSymbol Constants.leftBracket),(DebuggingTMTypes.R, DState qFold))]

    --BLOCK for qFold
    symbolsInQFold = [((DState qFold, DSymbol Constants.space),(DebuggingTMTypes.R, DState qFold))]
    symbolsToQFoldSLookForNewPlace = concatMap (\t -> let
        oldState = qFold
        newState = qFold ++ t ++ lookForNewPlace
        stateTransition = oldState ++ transition ++ newState
        in
        [((DState oldState, DSymbol t),(D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.L, DState newState))])
        $ Constants.leftBracket : terminalsList ++ nonterminalsList ++ Constants.signs
    symbolsToQFoldRightBracketLookForNewPlace = map (\t -> let
        oldState = qFold
        newState = qFold ++ t ++ lookForNewPlace in
        ((DState oldState, DSymbol t),(DebuggingTMTypes.L, DState newState))) [Constants.rightBracket]

    --BLOCK for qFoldSLookForNewPlace - it common for all symbols except ")"
    symbols = Constants.leftBracket : terminalsList ++ nonterminalsList ++ Constants.signs
    symbolsInQFoldSLookForNewPlace = map (\t -> let
        state = qFold ++ t ++ lookForNewPlace in
        ((DState state, DSymbol Constants.space),(DebuggingTMTypes.L, DState state))) symbols

    --BLOCK for qFoldNonterminalLookForNewPlace
    symbolsToQFoldNonterminalWrite = generateToQFoldSymbolWrite' [Constants.rightBracket] nonterminalsList
    --BLOCK for qFoldTerminalLookForNewPlace
    symbolsToQFoldTerminalWrite = generateToQFoldSymbolWrite' (terminalsList ++ [Constants.leftBracket]) terminalsList

    --BLOCK for qFoldSignLookForNewPlace
    symbolsToQFoldSignWrite = generateToQFoldSymbolWrite' nonterminalsList Constants.signs

    --BLOCK for qFold(LookForNewPlace
    symbolsToQFoldLeftBracketWrite = generateToQFoldSymbolWrite' Constants.signs [Constants.leftBracket]

    --BLOCK for qFold)LookForNewPlace
    qFoldRightBracketLookForNewPlace = qFold ++ Constants.rightBracket ++ lookForNewPlace
    symbolsInQFoldRightBracketLookForNewPlace =
        [((DState qFoldRightBracketLookForNewPlace, DSymbol Constants.space),
            (DebuggingTMTypes.R, DState qFoldRightBracketLookForNewPlace))]
    symbolsToQCheckLastRightBracket = let
        oldState = qFoldRightBracketLookForNewPlace
        newState = qCheckLast
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.rightBracket),(D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space),(DebuggingTMTypes.R, DState newState))]

    --BLOCK for qCheckLast)
    symbolsToQFoldRightBracketLast' = [((DState qCheckLast, DSymbol Constants.space),
        (DebuggingTMTypes.L, DState qFoldRightBracketLast'))]
    symbolsToQFoldRightBracket' = map (\t ->
        ((DState qCheckLast, DSymbol t),(DebuggingTMTypes.L, DState qFoldRightBracket'))
        ) $ Constants.brackets ++ nonterminalsList

    --BLOCK for case, when bracket is not last
    symbolsInQFoldRightBracket' = [((DState qFoldRightBracket', DSymbol Constants.space),
        (DebuggingTMTypes.L, DState qFoldRightBracket'))]
    symbolsToQFoldRightBracket = map (\t ->
        ((DState qFoldRightBracket', DSymbol t),(DebuggingTMTypes.R, DState qFoldRightBracket))
        ) $ Constants.rightBracket : terminalsList
    symbolsFromRightBracketToQFold = let
        oldState = qFoldRightBracket
        newState = qFold
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space),(D $ DSymbol Constants.rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.rightBracket),(DebuggingTMTypes.R, DState newState))]

    --BLOCK for case, when bracket is last
    symbolsInQFoldRightBracketLast' =
      [((DState qFoldRightBracketLast', DSymbol Constants.space),(DebuggingTMTypes.L, DState qFoldRightBracketLast'))]
    symbolsToQFoldRightBracketLast = map (\t ->
        ((DState qFoldRightBracketLast', DSymbol t),(DebuggingTMTypes.R, DState qFoldRightBracketLast)))
        $ Constants.rightBracket : terminalsList
    symbolsFromRightBracketLastToQFindNewSubstitution = let
        oldState = qFoldRightBracketLast
        newState = qFindNewSubstitution
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space),(D $ DSymbol Constants.rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.rightBracket),(DebuggingTMTypes.L, DState newState))]

    quadruples = symbolsInQRemoveSymbols ++ symbolsToQRemoveSymbols ++ symbolsToQRemoveBracketAroundFstWord
        ++ symbolsInQRemoveBracketsAroundFstWord ++ symbolsToQRemoveBracketsAroundSndWord
        ++ symbolsInQRemoveBracketsAroundSndWord
        ++ symbolsToQMoveToStart ++ symbolsInQMoveToStart ++ symbolsToQFold ++ symbolsInQFold
        ++ symbolsToQFoldSLookForNewPlace ++ symbolsToQFoldRightBracketLookForNewPlace
        ++ symbolsInQFoldSLookForNewPlace ++ symbolsToQFoldNonterminalWrite ++ symbolsToQFoldTerminalWrite
        ++ symbolsToQFoldSignWrite ++ symbolsToQFoldLeftBracketWrite
        ++ symbolsInQFoldRightBracketLookForNewPlace
        ++ symbolsToQCheckLastRightBracket ++ symbolsToQFoldRightBracketLast' ++ symbolsToQFoldRightBracket'
        ++ symbolsInQFoldRightBracket' ++ symbolsToQFoldRightBracket ++ symbolsInQFoldRightBracketLast'
        ++ symbolsToQFoldRightBracketLast ++ symbolsFromRightBracketToQFold
        ++ symbolsFromRightBracketLastToQFindNewSubstitution
    in (DQuadruples $ Helpers.addCollectionToMap quadruples Map.empty)


generateToQFoldSymbolWrite' :: [String] -> [String]
    -> [((DebuggingState, DebuggingSymbol), (DebuggingMove, DebuggingState))]
generateToQFoldSymbolWrite' inputSymbols stateSymbols = let
    qFold = "qFold"
    lookForNewPlace = "LookForNewPlace"
    write = "write"
    transition = "transition"
    symbolsToQSymbolWrite = concatMap (\t -> let
        oldState = qFold ++ t ++ lookForNewPlace
        newState = qFold ++ t ++ write in
        map (\s -> ((DState oldState, DSymbol s),(DebuggingTMTypes.R, DState newState)))
            inputSymbols) stateSymbols
    symbolsInQSymbolWrite = concatMap (\t -> let
        oldState = qFold ++ t ++ write
        newState = qFold
        transitionState = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space),(D $ DSymbol t, DState transitionState)),
        ((DState transitionState, DSymbol t),(DebuggingTMTypes.R, DState newState))]) stateSymbols
    in symbolsToQSymbolWrite ++ symbolsInQSymbolWrite

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
    qRewriteWithMinus = "qRewriteWithMinus"
    qFindNewSubstitution = "qFindNewSubstitution"
    q = "q"
    transition = "transition"
    one = "0"
    maxNumber = Helpers.calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [0..maxNumber - 1]
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals
    relationsList = Set.toList relations

    symbolsToQWriteStartCounterK = concatMap (\t -> let
        st@(DState oldState) = startDState
        newState = qWriteStartCounter ++ t
        stateTransition = oldState ++ transition ++ newState in
        [((st, DSymbol t),(D $ DSymbol one, DState stateTransition)),
        ((DState stateTransition, DSymbol one),(DebuggingTMTypes.L, DState newState))
        ]) nonterminalsList

    --BLOCK for qWriteStartCounterK
    symbolsToQCountWordLength = concatMap (\t -> let
        oldState = qWriteStartCounter ++ t
        newState = qCountWordLength
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space),(D $ DSymbol t, DState stateTransition)),
        ((DState stateTransition, DSymbol t),(DebuggingTMTypes.R, DState newState))
        ]) nonterminalsList

    --BLOCK for qCountWordLength
    symbolsInQCountWordLength = map (\t ->
        ((DState qCountWordLength, DSymbol t),(DebuggingTMTypes.R, DState qCountWordLength)))
        $ Constants.leftBracket : indices ++ nonterminalsList
    symbolsToQCheckSymbol = map (\t ->
        ((DState qCountWordLength, DSymbol t),(DebuggingTMTypes.R, DState qCheckSymbol))) terminalsList

    --BLOCK for qCheckSymbol
    symbolsToQSymbol = map (\t ->
        ((DState qCheckSymbol, DSymbol t),(DebuggingTMTypes.L, DState qSymbol))) [Constants.rightBracket]
    symbolsToQWord = map (\t ->
            ((DState qCheckSymbol, DSymbol t),(DebuggingTMTypes.L, DState qWord))) terminalsList

    --BLOCK for qSymbol
    symbolsInQSymbol = map (\t ->
        ((DState qSymbol, DSymbol t),(DebuggingTMTypes.L, DState qSymbol))) $ terminalsList ++ [Constants.leftBracket]
    symbolsToQStart = map (\t ->
        ((DState qSymbol, DSymbol t),(DebuggingTMTypes.L, DState qStart))) indices
    
    --BLOCK for qStart
    symbolsToQNonterminal = map (\t ->
        ((DState qStart, DSymbol t),(DebuggingTMTypes.R, DState $ q ++ t))) nonterminalsList

    --BLOCK for qNonterminal
    symbolsInQNonterminal = concatMap (\t -> let
        symbols = indices ++ [Constants.leftBracket]
        in map (\s ->
            ((DState $ q ++ t, DSymbol s),(DebuggingTMTypes.R, DState $ q ++ t)))
            symbols) nonterminalsList
    symbolsToQNonterminalSign = concatMap (\t -> map (\s ->
        if Helpers.symbolAcceptedByNonterminal grammar t s
            then ((DState $ q ++ t, DSymbol s),(DebuggingTMTypes.L, DState $ q ++ t ++ Constants.plus))
            else ((DState $ q ++ t, DSymbol s),(DebuggingTMTypes.L, DState $ q ++ t ++ Constants.minus))
        ) terminalsList) nonterminalsList

    --BLOCK for qNonterminal
    -- if there is no terminals, which is accepted by given nonterminal, there will be unreachable
    -- state qNonterminal+
    symbolsInQNonterminalSign = concatMap (\t -> map (\sign ->
        ((DState $ q ++ t ++ sign, DSymbol Constants.leftBracket),(DebuggingTMTypes.L, DState $ q ++ t ++ sign)))
        Constants.signs) nonterminalsList
    symbolsToQFindNewSubstitution = concatMap (\t -> concatMap (\sign -> let
        oldState = q ++ t ++ sign
        newState = qFindNewSubstitution
        stateTransition = oldState ++ transition ++ newState in
        concatMap (\i -> [((DState oldState, DSymbol i),(D $ DSymbol sign, DState stateTransition)),
        ((DState stateTransition, DSymbol sign),(DebuggingTMTypes.L, DState newState))]) indices
        ) Constants.signs) nonterminalsList

    --BLOCK for qWord
    symbolsInQWord = map (\t ->
        ((DState qWord, DSymbol t),(DebuggingTMTypes.L, DState qWord))) $ Constants.leftBracket : terminalsList
    symbolsToQChooseRelationI = map (\t ->
        ((DState qWord, DSymbol t),(DebuggingTMTypes.L, DState $ qChooseRelation ++ t))) indices

    --BLOCK for qChooseRelationI
    symbolsToQRemember = concatMap (\k -> let
        nonterminals' = filter (\n -> Helpers.kthRelForNonterminalLong relationsList n k) nonterminalsList
        oldState = qChooseRelation ++ k
        in map (\t -> let
            (SymbolsPair (_, number, _, nonterm1, nonterm2)) = Helpers.getFstConjInKthRel grammar t k
            nonterm1Val = Helpers.refineSymbolInConjunctionToNonterminal nonterm1
            nonterm2Val = Helpers.refineSymbolInConjunctionToNonterminal nonterm2
            newState = qRememberStart ++ show number ++ t ++ nonterm1Val ++ nonterm2Val
            in
            ((DState oldState, DSymbol t),(DebuggingTMTypes.R, DState newState)))
            nonterminals') indices

    symbolsToQRewriteWithMinusNonterminal = concatMap (\k -> let
        nonterminals' = filter (\n -> Helpers.kthRelForNonterminalLong relationsList n k) nonterminalsList
        rest = nonterminalsList \\ nonterminals'
        oldState = qChooseRelation ++ k
        in map (\t ->
            ((DState oldState, DSymbol t),(DebuggingTMTypes.R, DState $ qRewriteWithMinus ++ t)))
            rest) indices


    quadruples = symbolsToQWriteStartCounterK ++ symbolsToQCountWordLength ++ symbolsInQCountWordLength
        ++ symbolsToQCheckSymbol ++ symbolsToQSymbol ++ symbolsToQWord ++ symbolsInQSymbol
        ++ symbolsToQStart ++ symbolsInQWord ++ symbolsToQChooseRelationI ++ symbolsToQRemember
        ++ symbolsToQRewriteWithMinusNonterminal ++ symbolsToQNonterminal ++ symbolsInQNonterminal
        ++ symbolsToQNonterminalSign ++ symbolsInQNonterminalSign ++ symbolsToQFindNewSubstitution
    in (DQuadruples $ Helpers.addCollectionToMap quadruples Map.empty)

generateBlockForPreparingForSubstitution :: Grammar -> DebuggingQuadruples
generateBlockForPreparingForSubstitution grammar@(Grammar (nonterminals, terminals, _, _)) = let
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
    maxNumberOfRels = Helpers.calculateMaxNumberOfRulesForNonterminal grammar
    indices = map show [0..maxNumberOfRels - 1]

    quads = Helpers.calculateAllQuads grammar
    --BLOCK for qRememberStartCounterNonterm1Nonterm2Nonterm3
    rememberedSymbols = Constants.rightBracket : terminalsList
    quintets = concatMap (\(k, t, j, f) -> map (k, t, j, f,) rememberedSymbols) quads

    symbolsInQRememberStartLeftKTJF = concatMap (\(k, t, j, f) -> map (\symbol ->
        ((DState $ qRememberStart  ++ k ++ t ++ j ++ f, DSymbol symbol),
        (DebuggingTMTypes.L, DState $ qRememberStart  ++ k ++ t ++ j ++ f)))
        $ Constants.space : nonterminalsList)
        quads

    symbolsInQRememberStartRightKTJF = concatMap (\(k, t, j, f) -> map (\symbol ->
        ((DState $ qRememberStart  ++ k ++ t ++ j ++ f, DSymbol symbol),
        (DebuggingTMTypes.R, DState $ qRememberStart  ++ k ++ t ++ j ++ f))) $ Constants.leftBracket : indices)
        quads
    symbolsToQRememberSymbolMarkEndKTJF = concatMap (\(k, t, j, f, symbol) -> let
        oldState = qRememberStart  ++ k ++ t ++ j ++ f
        newState = qRemember ++ symbol ++ markEnd ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState
        in
        [((DState oldState, DSymbol symbol),(D $ DSymbol Constants.star, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.star),(DebuggingTMTypes.R, DState newState))]) quintets

    --BLOCK for qRememberSymbolMarkEndKTJF
    symbolsInQRememberSymbolMarkEndKTJF = concatMap (\(k, t, j, f, s) -> map (\symbol -> let
        state = qRemember ++ s ++ markEnd ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol symbol), (DebuggingTMTypes.R, DState state)))
         $ nonterminalsList ++ Constants.signs ++ Constants.brackets ++ terminalsList) quintets
    symbolsToQShiftWordSymbolKTJF = concatMap (\(k, t, j, f, s) -> let
        oldState = qRemember ++ s ++ markEnd ++ k ++ t ++ j ++ f
        newState = qShiftWord ++ s ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol Constants.hash, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.hash), (DebuggingTMTypes.R, DState newState))]) quintets

    --BLOCK for qShiftWordSymbolKTJF
    symbolsInQShiftWordSymbolKTJF = concatMap (\(k, t, j, f, s) -> let
        state = qShiftWord ++ s ++ k ++ t ++ j ++ f in
        [((DState state, DSymbol Constants.space), (DebuggingTMTypes.L, DState state))]) quintets

    symbolsWithSimilarStatesForTerms = Constants.hash : nonterminalsList ++ Constants.signs ++
        terminalsList ++ Constants.brackets

    tuple6 = concatMap (\(k, t, j, f, s) -> map (k, t, j, f, s,)
        symbolsWithSimilarStatesForTerms) quintets
    symbolsToQWriteSymbolKTJFsymbol = concatMap (\(k, t, j, f, s, s') -> let
        oldState = qShiftWord ++ s ++ k ++ t ++ j ++ f
        newState = qWriteSymbol ++ s ++ k ++ t ++ j ++ f ++ s'
        stateTransition = oldState ++ transition ++ newState
        in [((DState oldState, DSymbol s'), (D $ DSymbol Constants.space, DState stateTransition)),
            ((DState stateTransition, DSymbol Constants.space), (DebuggingTMTypes.R, DState newState))]
        ) tuple6

    -- case for star for terminals and bracket
    symbolsToQWriteSymbolKTJFStar = map (\(k, t, j, f, s) -> let
        oldState = qShiftWord ++ s ++ k ++ t ++ j ++ f
        newState = qWriteRemembered ++ s ++ k ++ t ++ j ++ f in
        ((DState oldState, DSymbol Constants.star), (DebuggingTMTypes.R, DState newState))) quintets

    --FIXME add docs to size of shift dependence
    shift = 6
    shifts = map show [1..shift]
    shiftNeg = 7
    shiftsNeg = map show [1..shiftNeg]
    -- case for shifts for terminals
    terminalQuintets = concatMap (\(k, t, j, f) ->
        map (k, t, j, f,) terminalsList) quads
    symbolsToQWriteSymbolKTJFshift = concatMap (\(k, t, j, f, s) ->
        if Helpers.checkIfConjHasNeg grammar (k, t, j, f) then
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
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol s', DState stateTransition)),
        ((DState stateTransition, DSymbol s'), (DebuggingTMTypes.L, DState newState))]) tuple6

    --BLOCK for qWriteRememberedsKTJF
    shiftsPairs = Helpers.getShiftsDecrements shift Constants.star
    shiftsPairsNeg = Helpers.getShiftsDecrements shiftNeg Constants.star
    symbolsToQWriteCountersKTJFterminals = concatMap (\(k, t, j, f, s) -> let
        oldState = qWriteRemembered ++ s ++ k ++ t ++ j ++ f
        newState = qWriteCounter ++ s ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState
        in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol s, DState stateTransition)),
        ((DState stateTransition, DSymbol s), (DebuggingTMTypes.L, DState newState))]) terminalQuintets
    -- BLOCK for qWriteCounterSymbolKTJF (terminals)
    symbolsInQWriteCountersKTJF = map (\(k, t, j, f, s) -> let
        oldState = qWriteCounter ++ s ++ k ++ t ++ j ++ f in
        ((DState oldState, DSymbol Constants.space), (DebuggingTMTypes.L, DState oldState))) terminalQuintets

    -- pair ("1", "*") = pair of end of shifting, there is special transition, this pair called endShift
    -- rest of pairs called midPairs
    endShift = head $ filter (\t -> snd t == Constants.star) shiftsPairs
    midShifts = shiftsPairs \\ [endShift]
    endShiftNeg = head $ filter (\t -> snd t == Constants.star) shiftsPairsNeg
    midShiftsNeg = shiftsPairsNeg \\ [endShiftNeg]

    symbolsToQUnmarkEndKTJF = concatMap (\(k, t, j, f, s) ->
        if Helpers.checkIfConjHasNeg grammar (k, t, j, f) then let
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
        if Helpers.checkIfConjHasNeg grammar (k, t, j, f) then
            concatMap (\ (i, iDecr) -> let
            oldState =  qWriteCounter ++ s ++ k ++ t ++ j ++ f
            newState = qMoveToEnd ++ s ++ k ++ t ++ j ++ f
            stateTransition = oldState ++ transition ++ newState in
            [((DState oldState, DSymbol i), (D $ DSymbol iDecr, DState stateTransition)),
            ((DState stateTransition, DSymbol iDecr), (DebuggingTMTypes.R, DState newState))]) midShiftsNeg
         else
            concatMap (\ (i, iDecr) -> let
            oldState =  qWriteCounter ++ s ++ k ++ t ++ j ++ f
            newState = qMoveToEnd ++ s ++ k ++ t ++ j ++ f
            stateTransition = oldState ++ transition ++ newState in
            [((DState oldState, DSymbol i), (D $ DSymbol iDecr, DState stateTransition)),
            ((DState stateTransition, DSymbol iDecr), (DebuggingTMTypes.R, DState newState))]) midShifts
        ) terminalQuintets

    --BLOCK for qMoveToEndsKTJF
    symbolsInQMoveToEndKTJF = concatMap (\(k, t, j, f, s) -> map (\symbol -> let
        state = qMoveToEnd ++ s ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol symbol), (DebuggingTMTypes.R, DState state)))
        $ Constants.space : nonterminalsList ++ Constants.signs ++ Constants.brackets ++ terminalsList) terminalQuintets
    symbolsFromQMoveToEndToQShiftWordSymbolKTJF = map (\(k, t, j, f, s) -> let
        oldState = qMoveToEnd ++ s ++ k ++ t ++ j ++ f
        newState = qShiftWord ++ s ++ k ++ t ++ j ++ f in
        ((DState oldState, DSymbol Constants.hash), (DebuggingTMTypes.R, DState newState))) terminalQuintets

    -- BLOCK for star (bracket - special case)
    symbolsInQWriteRememberedBracket = concatMap (\(k, t, j, f) -> let
        oldState = qWriteRemembered ++ Constants.rightBracket ++ k ++ t ++ j ++ f
        newState = oldState
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol Constants.rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.rightBracket), (DebuggingTMTypes.L, DState newState))]) quads
    symbolsToQUnmarkEndKTJFBracket = concatMap (\(k, t, j, f) -> let
        oldState = qWriteRemembered ++ Constants.rightBracket ++ k ++ t ++ j ++ f
        newState = qUnmarkEnd ++ k ++ t ++ j ++ f ++ Constants.rightBracket
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.star), (D $ DSymbol Constants.rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.rightBracket), (DebuggingTMTypes.R, DState newState))]
        ) quads

    symbolsInQUnmarkEndKTJF = concatMap (\(k, t, j, f) -> map (\s -> let
        state = qUnmarkEnd ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol s), (DebuggingTMTypes.R, DState state)))
        $ Constants.space : nonterminalsList ++ Constants.signs ++ Constants.brackets ++ terminalsList) quads
    symbolsToQWritingRelationKTJF = concatMap (\(k, t, j, f) -> let
            oldState = qUnmarkEnd ++ k ++ t ++ j ++ f
            newState = qWritingRelation ++ k ++ t ++ j ++ f
            stateTransition = oldState ++ transition ++ newState in
            [((DState oldState, DSymbol Constants.hash), (D $ DSymbol Constants.space, DState stateTransition)),
            ((DState stateTransition, DSymbol Constants.space), (DebuggingTMTypes.R, DState newState))]) quads

    symbolsInQUnmarkEndKTJFBracket = concatMap (\(k, t, j, f) -> map (\s -> let
        state = qUnmarkEnd ++ k ++ t ++ j ++ f ++ Constants.rightBracket in
        ((DState state, DSymbol s), (DebuggingTMTypes.R, DState state)))
        $ nonterminalsList ++ Constants.signs ++ Constants.brackets ++ terminalsList) quads
    qFindNewSubstitution = "qFindNewSubstitution"
    symbolsToQFindNewSubstitution = concatMap (\(k, t, j, f) -> let
        oldState = qUnmarkEnd ++ k ++ t ++ j ++ f ++ Constants.rightBracket
        newState = qFindNewSubstitution
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.hash), (D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space), (DebuggingTMTypes.L, DState newState))]) quads

    quadruples = symbolsInQRememberStartRightKTJF
        ++ symbolsInQRememberStartLeftKTJF ++ symbolsToQRememberSymbolMarkEndKTJF
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
    in (DQuadruples $ Helpers.addCollectionToMap quadruples Map.empty)

generateBlockForSubstitution :: Grammar -> DebuggingQuadruples
generateBlockForSubstitution grammar@(Grammar (nonterminals, terminals, _, _) ) = let
    qWritingRelation = "qWritingRelation"
    qWrite = "qWrite"
    transition = "transition"
    qWriteCounter = "qWriteCounter"
    qLetterWriting = "qLetterWriting"
    preparation = "Preparation"
    letter = "Letter"
    shiftWord = "ShiftWord"
    qRememberStart = "qRememberStart"

    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    terminalsList = map terminalValue $ Set.toList terminals

    quads = Helpers.calculateAllQuads grammar

    quadsNeg = filter (Helpers.checkIfConjHasNeg grammar) quads
    quadsPos = quads \\ quadsNeg

    --BLOCK for qWritingRelationKTJF
    symbols = Constants.space : terminalsList ++ nonterminalsList ++ Constants.brackets ++ Constants.signs
    symbolsInQWritingRelationKTJF = concatMap (\(k, t, j, f) -> map (\symbol ->
        let state = qWritingRelation ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol symbol), (DebuggingTMTypes.L, DState state))) symbols) quads

    fstNonterminal = "1"
    sndNonterminal = "2"
    -- block for case when conj (k, t, j, f) has neg
    symbolsFromQWritingRelationToQWriteKTJFT = concatMap (\(k, t, j, f) -> let
        oldState = qWritingRelation ++ k ++ t ++ j ++ f
        newState = qWrite ++ k ++ t ++ j ++ f ++ j
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.star), (D $ DSymbol Constants.negation, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.negation), (DebuggingTMTypes.R, DState newState))]) quadsNeg
    symbolsFromQWriteKTJFJtoQWriteCounterKTJFJ = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ j
        newState = qWriteCounter ++ k ++ t ++ j ++ f ++ j ++ fstNonterminal
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol j, DState stateTransition)),
        ((DState stateTransition, DSymbol j), (DebuggingTMTypes.R, DState newState))]) quadsNeg

    -- block for case when conj (k, t, j, f) does not have neg
    -- writing initial value to counter
    symbolsFromQWritingRelationToQWriteCounterKTJFT = concatMap (\(k, t, j, f) -> let
        oldState = qWritingRelation ++ k ++ t ++ j ++ f
        newState = qWriteCounter ++ k ++ t ++ j ++ f ++ j ++ fstNonterminal
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.star), (D $ DSymbol j, DState stateTransition)),
        ((DState stateTransition, DSymbol j), (DebuggingTMTypes.R, DState newState))]) quadsPos

    -- block for qWriteKTJFJCounter
    one = "0"
    symbolsToQWriteKTJFJleftBracket = concatMap (\(k, t, j, f) -> let
        oldState = qWriteCounter ++ k ++ t ++ j ++ f ++ j ++ fstNonterminal
        newState = qWrite ++ k ++ t ++ j ++ f ++ j ++ Constants.leftBracket ++ fstNonterminal
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol one, DState stateTransition)),
        ((DState stateTransition, DSymbol one), (DebuggingTMTypes.R, DState newState))]) quads

    -- block for qWriteKJFJSymbol
    symbolsToQWriteKTJFJSymbol = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ j ++ Constants.leftBracket ++ fstNonterminal
        newState = qWrite ++ k ++ t ++ j ++ f ++ j ++ letter
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol Constants.leftBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.leftBracket), (DebuggingTMTypes.R, DState newState))]) quads

     -- block for qWriteKTJFJSymbol
     -- creating empty space for first word letter, which will move here after
    symbolsToQWriteKTJFJRightBracket = map (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ j ++ letter
        newState = qWrite ++ k ++ t ++ j ++ f ++ j ++ Constants.rightBracket ++ fstNonterminal in
        ((DState oldState, DSymbol Constants.space), (DebuggingTMTypes.R, DState newState))) quads

    -- block for qWriteKTJFJRightBracket
    symbolsToQKTJFJletterWriting = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ j ++ Constants.rightBracket ++ fstNonterminal
        newState = qLetterWriting ++ k ++ t ++ j ++ f ++ j
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol Constants.rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.rightBracket), (DebuggingTMTypes.R, DState newState))]) quads

    -- block for qLetterWritingKTJFJs'
    quintets = concatMap (\(k, t, j, f) -> map (k, t, j, f,) terminalsList) quads
    symbolsInQLetterWritingKTJFJ = map (\(k, t, j, f) -> let
        state = qLetterWriting ++ k ++ t ++ j ++ f ++ j in
        ((DState state, DSymbol Constants.space), (DebuggingTMTypes.R, DState state))) quads
    symbolsToQletterWritingKTJFJs' = concatMap (\(k, t, j, f, s) -> let
        oldState = qLetterWriting ++ k ++ t ++ j ++ f ++ j
        newState = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s ++ Constants.comma
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol s), (D $ DSymbol Constants.space, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.space), (DebuggingTMTypes.L, DState newState))]) quintets

    -- block for qLetterWritingKTJFJs'
    skipSymbols = Constants.space : [Constants.rightBracket]
    symbolsInQLetterWritingKTJFJs' = concatMap (\(k, t, j, f, s) -> map (\skipSymbol -> let
        state = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s ++ Constants.comma in
        ((DState state, DSymbol skipSymbol), (DebuggingTMTypes.L, DState state))) skipSymbols) quintets
    symbolsToQletterWritingKTJFJs = map (\(k, t, j, f, s) -> let
        oldState = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s ++ Constants.comma
        newState = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s in
        ((DState oldState, DSymbol Constants.leftBracket), (DebuggingTMTypes.R, DState newState))) quintets

    -- block for qLetterWritingKTJF
    symbolsToQWriteKTJFF = concatMap (\(k, t, j, f, s) -> let
        oldState = qLetterWriting ++ k ++ t ++ j ++ f ++ j ++ s
        newState = qWrite ++ k ++ t ++ j ++ f ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol s, DState stateTransition)),
        ((DState stateTransition, DSymbol s), (DebuggingTMTypes.R, DState newState))]) quintets

    -- block for qWriteKTJFF
    symbolsInQWriteKTJFF = map (\(k, t, j, f) -> let
        state = qWrite ++ k ++ t ++ j ++ f ++ f in
        ((DState state, DSymbol Constants.rightBracket), (DebuggingTMTypes.R, DState state))) quads
    symbolsToQWriteCounterKTJFF = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ f
        newState = qWriteCounter ++ k ++ t ++ j ++ f ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol f, DState stateTransition)),
        ((DState stateTransition, DSymbol f), (DebuggingTMTypes.R, DState newState))]) quads

    --block for qWriteCounterKTJFF
    symbolsToQWriteKTJFFleftBracket = concatMap (\(k, t, j, f) -> let
        oldState = qWriteCounter ++ k ++ t ++ j ++ f ++ f
        newState = qWrite ++ k ++ t ++ j ++ f ++ f ++ Constants.leftBracket ++ sndNonterminal
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol one, DState stateTransition)),
        ((DState stateTransition, DSymbol one), (DebuggingTMTypes.R, DState newState))]) quads

    --block for qWriteKTJFFleftBracket
    symbolsToQWriteKTJFFRightBracketPreparation = concatMap (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ f ++ Constants.leftBracket ++ sndNonterminal
        newState = qWrite ++ k ++ t ++ j ++ f ++ f ++ Constants.rightBracket ++ sndNonterminal ++ preparation
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space), (D $ DSymbol Constants.leftBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.leftBracket), (DebuggingTMTypes.R, DState newState))]) quads

    --block for qWriteKTJFFRightBracketPreparation
    symbolsInQWriteKTJFFRightBracketPreparation = map (\(k, t, j, f, s) -> let
        state = qWrite ++ k ++ t ++ j ++ f ++ f ++ Constants.rightBracket ++ sndNonterminal ++ preparation in
        ((DState state, DSymbol s), (DebuggingTMTypes.R, DState state))) quintets
    symbolsToQKTJFFRightBracketShiftWord = map (\(k, t, j, f) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ f ++ Constants.rightBracket ++ sndNonterminal ++ preparation
        newState = qWrite ++ k ++ t ++ j ++ f ++ f ++ Constants.rightBracket ++ shiftWord in
        ((DState oldState, DSymbol Constants.rightBracket), (DebuggingTMTypes.L, DState newState))) quads
    
    --block for qWriteKTJFFRightBracketShiftWord -- transition state for moving to qRememberStartKTJF block
    -- we've generated in previous block case for shifting ')' on one cell, now we use it to move all
    -- symbols, which are more right then letters of word, to put ')' after word
    symbolsToQKTJFShiftWord = map (\(k, t, j, f, s) -> let
        oldState = qWrite ++ k ++ t ++ j ++ f ++ f ++ Constants.rightBracket ++ shiftWord
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
    in (DQuadruples $ Helpers.addCollectionToMap quadruples Map.empty)

generateBlockForMovingToNextConjunction :: Grammar -> DebuggingQuadruples
generateBlockForMovingToNextConjunction grammar@(Grammar (nonterminals, terminals, _, _)) = let
    transition = "Transition"
    qShiftingFromFold = "qShiftingKTJFFromFold"
    qRememberStart = "qRememberStart"

    terminalsList = map terminalValue $ Set.toList terminals
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    -- helper calculations of quads
    --qauds in this block - all conjs, which has next conj
    quads' = Helpers.calculateAllQuads grammar
    -- if conj has next conj, then function calculateNextConjunctionInSameRule gives (Just s),
    -- where s - next conj (SymbolsPair)
    quads = filter (\(k, t, j, f) -> if Helpers.checkIfConjHasNeg grammar (k, t, j, f)
        then let
            pair = SymbolsPair (Nonterminal t, read k :: Int, True,
                N $ Nonterminal j, N $ Nonterminal f) in
            case Helpers.calculateNextConjunctionInSameRule grammar pair of
            Just _ -> True
            Nothing -> False
        else let
            pair = SymbolsPair (Nonterminal t, read k :: Int, False,
                N $ Nonterminal j, N $ Nonterminal f) in
            case Helpers.calculateNextConjunctionInSameRule grammar pair  of
            Just _ -> True
            Nothing -> False) quads'
    qFoldRightBracketLast = "qFoldRightBracketLast"
    qFindNewSubstitution = "qFindNewSubstitution"
    stateTransitionOld = qFoldRightBracketLast ++ transition ++ qFindNewSubstitution
    -- whole transition is ((DState qFoldRightBracketLast, DSymbol " "),(D $ DSymbol rightBracket, DState stateTransition))
    differentTransition1Key = (DState qFoldRightBracketLast, DSymbol Constants.space)
    -- whole transition is ((DState stateTransition, DSymbol rightBracket),(DebuggingTMTypes.R, DState qFindNewSubstitution))
    differentTransition2Key = (DState stateTransitionOld, DSymbol Constants.rightBracket)
    (DQuadruples quadruplesMap'') = generateBlockForFolding grammar
    quadruplesMap' = Map.delete differentTransition1Key quadruplesMap''
    quadruplesMap = Map.delete differentTransition2Key quadruplesMap'

    generatedQuadruples = generateFoldingForMidCongs quadruplesMap quads

    -- generate special case for transition1 and transition2
    symbolsFromRightBracketLastToQFindNewSubstitution = concatMap (\(k, t, j, f) -> let
        oldState = qFoldRightBracketLast ++ k ++ t ++ j ++ f
        newState = qShiftingFromFold ++ k ++ t ++ j ++ f
        stateTransition = oldState ++ transition ++ newState in
        [((DState oldState, DSymbol Constants.space),(D $ DSymbol Constants.rightBracket, DState stateTransition)),
        ((DState stateTransition, DSymbol Constants.rightBracket),(DebuggingTMTypes.R, DState newState))]) quads

    symbolsInQShiftingFromFoldKTJF = concatMap (\(k, t, j, f) -> map (\s -> let
        state = qShiftingFromFold ++ k ++ t ++ j ++ f in
        ((DState state, DSymbol s),(DebuggingTMTypes.L, DState state)))
        $ nonterminalsList ++ Constants.space : terminalsList ++ Constants.brackets ++ Constants.signs) quads
    symbolsToQKTJFRememberStart = map (\quad@(k, t, j, f) -> let
        oldState = qShiftingFromFold ++ k ++ t ++ j ++ f
        hasNeg = Helpers.checkIfConjHasNeg grammar quad
        pair = Helpers.constructSymbolsPairByQuad quad hasNeg
        (SymbolsPair (Nonterminal t', k', _, j', f')) =
          case Helpers.calculateNextConjunctionInSameRule grammar pair of
            Just p -> p
            Nothing -> error "Next pair does not exist, though original was in list of mid conjs."
        j'' = Helpers.refineSymbolInConjunctionToNonterminal j'
        f'' = Helpers.refineSymbolInConjunctionToNonterminal f'
        newState = qRememberStart ++ show k' ++ t' ++ j'' ++ f'' in
        ((DState oldState, DSymbol k),(DebuggingTMTypes.R, DState newState))) quads
    newQuadruples = Helpers.addCollectionToMap
        (symbolsFromRightBracketLastToQFindNewSubstitution
        ++ symbolsToQKTJFRememberStart ++ symbolsInQShiftingFromFoldKTJF)
        Map.empty
    allQuadruplesMap = Map.union generatedQuadruples newQuadruples
    in DQuadruples allQuadruplesMap

generateBlockForGettingAccepted :: Grammar -> DebuggingQuadruples
generateBlockForGettingAccepted (Grammar (nonterminals, _, _, _)) = let
    done = "Done"
    nonterminalslList = map nonterminalValue $ Set.toList nonterminals
    symbolsInDone = map (\t -> ((DState done, DSymbol t),(DebuggingTMTypes.R, DState done))) nonterminalslList
    symbolsToAccepted = [((DState done, DSymbol Constants.plus),(DebuggingTMTypes.L, finalDState))]
    symbolsToNotAccepted = [((DState done, DSymbol Constants.minus),(DebuggingTMTypes.L, errorDState))]
    symbols = symbolsInDone ++ symbolsToAccepted ++ symbolsToNotAccepted
    in DQuadruples $ Helpers.addCollectionToMap symbols Map.empty

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

