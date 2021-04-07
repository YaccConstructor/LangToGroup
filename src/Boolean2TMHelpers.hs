{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Boolean2TMHelpers where

import GrammarType
import DebuggingTMTypes
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Ord
import Data.Maybe

calculateMaxNumberOfRulesForNonterminal :: Grammar -> Int
calculateMaxNumberOfRulesForNonterminal (Grammar (_, _, relations, _)) = let
    listRelations = Set.toList relations
    groupedRelations = calculateGroupRelationsByNonterminals listRelations
    in (snd $ maximumBy (comparing snd) (Map.toList $ Map.map length groupedRelations))


calculateGroupRelationsByNonterminals :: [Relation] -> Map.Map Nonterminal [[Conj]]
calculateGroupRelationsByNonterminals relations = let
    relations' = map (\case
        Relation (w, cfgRightPart) -> BooleanRelation (w, [PosConj cfgRightPart])
        boolRel -> boolRel) relations

    mapWithReversedRelationsOrder = Map.fromListWith (++)
        [(nonterminal, [conjs]) | BooleanRelation (nonterminal, conjs) <- relations']
    in Map.map sort mapWithReversedRelationsOrder

-- Helpers for working with conjunctions

calculateNextConjunctionInSameRule :: Grammar -> SymbolsPair -> Maybe SymbolsPair
calculateNextConjunctionInSameRule (Grammar (_, _, relations, _))
    (SymbolsPair (nonterminal, relNumber, hasNeg, N nonterminal1, N nonterminal2)) = let
    groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    relationsForNonterminal = groupedRelations Map.! nonterminal
    conjs = relationsForNonterminal !! relNumber
    list = if hasNeg
        then NegConj [N nonterminal1, N nonterminal2]
        else PosConj [N nonterminal1, N nonterminal2]
    conjPairIndex = elemIndex list conjs in
    case conjPairIndex of
        Just index | index == (length conjs - 1) -> Nothing
                   | otherwise -> let conj = conjs !! (index + 1) in
                        Just $ convertListToConjunctionPair nonterminal relNumber conj
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
        else let conjs = relationsForNonterminal !! nextRelationNumber in
        Just $ convertListToConjunctionPair nonterminal nextRelationNumber $ head conjs

getFstConjInKthRel :: Grammar -> String -> String -> SymbolsPair
getFstConjInKthRel (Grammar (_, _, relations, _)) nontermVal number = let
    number' = read number :: Int
    nonterminal = Nonterminal nontermVal
    groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    relationsForNonterminal = groupedRelations Map.! nonterminal
    conjs = relationsForNonterminal !! number'
    in convertListToConjunctionPair nonterminal number' $ head conjs

-- Helpers for working with long/short relations

getLongRels :: [Relation] -> [Relation]
getLongRels = filter (not . relationHasOneTerminalInRightPart)
                    
getShortRightParts :: Nonterminal -> [[Conj]] -> [String]
getShortRightParts nonterminal rightParts = let
    shortOrNotRels = map (\t ->
        (elemIndex t rightParts,
        relationHasOneTerminalInRightPart (BooleanRelation (nonterminal, t)))) rightParts
    indices' = map (\(i, t) -> if t then i else Nothing) shortOrNotRels
    indices = map show $ catMaybes indices'
    in indices
    
-- short relation is relation with one terminal in right part
getNumbersOfShortRelations :: Grammar -> Map.Map Nonterminal [String]
getNumbersOfShortRelations (Grammar (_, _, relations, _)) =
    Map.mapWithKey getShortRightParts $ calculateGroupRelationsByNonterminals $ Set.toList relations

-- Helpers for building conjunctions

-- grammar -> string (nonterminal in left part)
--         -> string (number of relation) -> list of first nonterminals
getFstNonterminalsInConjsOfGivenRel :: Grammar -> String -> String -> [String]
getFstNonterminalsInConjsOfGivenRel
    (Grammar (_, _, rels, _)) nonterminal number = do
    let groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList rels
    let conjs = (groupedRelations Map.! Nonterminal nonterminal) !! (read number :: Int)
    if relationHasOneTerminalInRightPart $ BooleanRelation (Nonterminal nonterminal, conjs)
        then []
        else let
        firstSymbolsInConjunctions = map (head . symbols) conjs
        in map refineSymbolInConjunctionToNonterminal firstSymbolsInConjunctions

-- grammar -> string (nonterminal in left part) -> string (number of relation)
--         -> string (first nonterminal in conjunction) -> list of first nonterminals
getSndNonterminalsInConjsOfGivenRel :: Grammar -> String -> String -> String -> [String]
getSndNonterminalsInConjsOfGivenRel
    (Grammar (_, _, rels, _)) leftNonterminal number fstNontermInConj = let
    groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList rels
    conjs = (groupedRelations Map.! Nonterminal leftNonterminal) !! (read number :: Int)
    symbol = N (Nonterminal fstNontermInConj)
    possibleConjs = filter (\t -> symbol == head (symbols t)) conjs
    possibleSndNonterminals = map (\t -> symbols t !! 1) possibleConjs
    in map refineSymbolInConjunctionToNonterminal possibleSndNonterminals

-- Helpers for generating quads and triplets

calculateTriplets :: Grammar -> String -> [String] -> [(String, String, String)]
calculateTriplets grammar number
  = concatMap (\t -> let
        firstNonterminals = getFstNonterminalsInConjsOfGivenRel grammar t number
        in map (number, t,) firstNonterminals)

calculateQuads' :: Grammar -> [([Char], [Char], [Char])] -> [(String, String, String, [String])]
calculateQuads' grammar = map (\(k, t, j)
    -> (k, t, j, getSndNonterminalsInConjsOfGivenRel grammar t k j))

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
    let relsForNonterminal = groupedRelations Map.! Nonterminal leftN
    let conjs = relsForNonterminal !! (read number :: Int)
    any (\t -> t == NegConj [N (Nonterminal fstN), N (Nonterminal sndN)]) conjs
        
kthRelForNonterminalLong :: [Relation] -> String -> String -> Bool
kthRelForNonterminalLong relations nontermVal k = do
    let k' = read k :: Int
    let groupedRelations = calculateGroupRelationsByNonterminals relations
    let nonterminal = Nonterminal nontermVal
    let relationsForNonterm = groupedRelations Map.! nonterminal
    let relation = BooleanRelation (nonterminal, relationsForNonterm !! k')
    length relationsForNonterm > k' &&
        not (relationHasOneTerminalInRightPart relation)

relationHasOneTerminalInRightPart :: Relation -> Bool
relationHasOneTerminalInRightPart (Relation (_, [T (Terminal _)])) = True
relationHasOneTerminalInRightPart (BooleanRelation (_, [PosConj [T (Terminal _)]])) = True
relationHasOneTerminalInRightPart _ = False

symbolAcceptedByNonterminal :: Grammar -> String -> String -> Bool
symbolAcceptedByNonterminal (Grammar (_, _, relations, _)) nontermValue symbol = let
    groupedRelations = calculateGroupRelationsByNonterminals $ Set.toList relations
    nonterminal = Nonterminal nontermValue
    nontermRels = groupedRelations Map.! nonterminal
    terminalsInRightPart' = map (\conjs ->
        if relationHasOneTerminalInRightPart (BooleanRelation (nonterminal, conjs))
            then Just $ head $ symbols $ head conjs
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

convertListToConjunctionPair :: Nonterminal -> Int -> Conj -> SymbolsPair
convertListToConjunctionPair nonterminal relationNumber
    (NegConj [N conjNonterminal1, N conjNonterminal2]) =
    SymbolsPair (nonterminal, relationNumber, True, N conjNonterminal1 , N conjNonterminal2)
convertListToConjunctionPair nonterminal relationNumber
    (PosConj [N conjNonterminal1, N conjNonterminal2]) =
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