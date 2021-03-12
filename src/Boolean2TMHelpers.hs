{-# LANGUAGE TupleSections #-}

module Boolean2TMHelpers where

import GrammarType
import DebuggingTMTypes
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.List.Split (splitOn)
import Data.Ord
import Data.Maybe

calculateMaxNumberOfRulesForNonterminal :: Grammar -> Int
calculateMaxNumberOfRulesForNonterminal (Grammar (_, _, relations, _)) = let
    listRelations = Set.toList relations
    groupedRelations = calculateGroupRelationsByNonterminals listRelations
    in (snd $ maximumBy (comparing snd) (Map.toList $ Map.map length groupedRelations))


calculateGroupRelationsByNonterminals :: [Relation] -> Map.Map Nonterminal [[GrammarType.Symbol]]
calculateGroupRelationsByNonterminals relations = let
    mapWithReversedRelationsOrder = Map.fromListWith (++) [(nonterminal, [symbols]) | Relation (nonterminal, symbols) <- relations]
    in Map.map sort mapWithReversedRelationsOrder

-- Helpers for working with conjunctions

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

getFstConjInKthRel :: Grammar -> String -> String -> SymbolsPair
getFstConjInKthRel (Grammar (_, _, relations, _)) nontermVal number = let
    number' = read number :: Int
    nonterminal = Nonterminal nontermVal
    groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    relationsForNonterminal = groupedRelations Map.! nonterminal
    relation = relationsForNonterminal !! number'
    conjunctionPairs = splitOn [O Conjunction] relation
    in convertListToConjunctionPair nonterminal number' $ head conjunctionPairs

-- Helpers for working with long/short relations

getLongRels :: [Relation] -> [Relation]
getLongRels = filter (not . relationHasOneTerminalInRightPart)
                    
getShortRightParts :: Nonterminal -> [[GrammarType.Symbol]] -> [String]
getShortRightParts nonterminal rightParts = let
    shortOrNotRels = map (\t -> relationHasOneTerminalInRightPart (Relation (nonterminal, t))) rightParts
    indices' = map (\t -> if t then elemIndex t shortOrNotRels else Nothing) shortOrNotRels
    indices = map show $ catMaybes indices'
    in indices
    
-- short relation is relation with one terminal in right part
getNumbersOfShortRelations :: Grammar -> Map.Map Nonterminal [String]
getNumbersOfShortRelations (Grammar (_, _, relations, _)) =
    Map.mapWithKey getShortRightParts $ calculateGroupRelationsByNonterminals $ Set.toList relations

-- Helpers for building conjunctions

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

-- Helpers for generating quads and triplets

calculateTriplets :: Grammar -> String -> [String] -> [(String, String, String)]
calculateTriplets grammar number
  = concatMap
      (\t -> let
        firstNonterminals = getFirstNonterminalsInConjunctionsOfGivenRelation grammar t number
        in map (number, t,) firstNonterminals)

calculateQuads' :: Grammar -> [([Char], [Char], [Char])] -> [(String, String, String, [String])]
calculateQuads' grammar = map (\ (k, t, j) -> (k, t, j, getSecondNonterminalsInConjunctionsOfGivenRelation grammar t k j))

calculateQuads :: Grammar -> String -> [String] -> [(String, String, String, String)]
calculateQuads grammar k' nonterminalsWithKthRel = let
    triplets = calculateTriplets grammar k' nonterminalsWithKthRel
    quads' = calculateQuads' grammar triplets
    in concatMap (\ (k, t, j, s) -> map (k, t, j,) s) quads'

calculateAllQuads :: Grammar -> [(String, String, String, String)]
calculateAllQuads gr@(Grammar (nonterminals, _, rels, _)) = let
    maxNumber = calculateMaxNumberOfRulesForNonterminal gr
    indices = map show [0..maxNumber - 1]
    nonterminalsList = map nonterminalValue $ Set.toList nonterminals
    indicesWithNonterms = map (\i -> (i, filter (\t ->
        kthRelForNonterminalLong (Set.toList rels) t i) nonterminalsList)) indices
    in concatMap (uncurry $ calculateQuads gr) indicesWithNonterms

-- Helpers which check sth

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
        
kthRelForNonterminalLong :: [Relation] -> String -> String -> Bool
kthRelForNonterminalLong relations nontermVal k = do
    let k' = read k :: Int
    let groupedRelations = calculateGroupRelationsByNonterminals relations
    let nonterminal = Nonterminal nontermVal
    let relationsForNonterm = groupedRelations Map.! nonterminal
    length relationsForNonterm > k' &&
        not (relationHasOneTerminalInRightPart $ Relation (nonterminal, relationsForNonterm !! k'))        

relationHasOneTerminalInRightPart :: Relation -> Bool
relationHasOneTerminalInRightPart (Relation (_, [T (Terminal _)])) = True
relationHasOneTerminalInRightPart _ = False

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

-- Helpers for converting/refining sth to sth

refineSymbolInConjunctionToNonterminal :: GrammarType.Symbol -> String
refineSymbolInConjunctionToNonterminal (N nonterminal) = nonterminalValue nonterminal
refineSymbolInConjunctionToNonterminal _ = error "Not a nonterminal, conjunction pair must be a pair of nonterminals"

addCollectionToMap :: (Ord k) => [(k, a)] -> Map.Map k a -> Map.Map k a
addCollectionToMap ((a,b) : xs) myMap = addCollectionToMap xs $ Map.insert a b myMap
addCollectionToMap [] myMap = myMap

refineSymbolToTerminalValue :: GrammarType.Symbol -> String
refineSymbolToTerminalValue (T t) = terminalValue t
refineSymbolToTerminalValue _ = error "Given symbol is not terminal"

constructSymbolsPairByQuad :: (String, String, String, String) -> Bool -> SymbolsPair
constructSymbolsPairByQuad (number, leftN, fstN, sndN) hasNeg =
    SymbolsPair (Nonterminal leftN, read number :: Int, hasNeg, N $ Nonterminal fstN, N $ Nonterminal sndN)

convertListToConjunctionPair :: Nonterminal -> Int -> [GrammarType.Symbol] -> SymbolsPair
convertListToConjunctionPair nonterminal relationNumber [_, N conjNonterminal1, N conjNonterminal2] =
    SymbolsPair (nonterminal, relationNumber, True, N conjNonterminal1 , N conjNonterminal2)
convertListToConjunctionPair nonterminal relationNumber [N conjNonterminal1, N conjNonterminal2] =
    SymbolsPair (nonterminal, relationNumber, False, N conjNonterminal1, N conjNonterminal2)
convertListToConjunctionPair _ _ _ = error "Conjunction must be pair of nonterminals. "

-- other

getShiftsDecrements :: Int -> String -> [(String, String)]
getShiftsDecrements shiftSize symbol = let
    fstPair = [(symbol, show shiftSize)]
    indices = [1..shiftSize]
    midPairs = map (\i -> if
        i == 1 then (show i, symbol)
        else (show i, show (i - 1))
        ) indices
    in (fstPair ++ midPairs)