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
    plus = "+"
    minus = "-"
    transition = "Transition"
    skipParentNonterminal = "skipParentNonterminal"
    rule = "Rule"
    terminalsList = map terminalValue $ Set.toList terminals
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


