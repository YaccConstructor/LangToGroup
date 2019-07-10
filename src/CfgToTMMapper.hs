module CfgToTMMapper where

import GrammarType
import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers

-- define a start states
startStateFirstTape = State "q_0^1"
startStateSecondTape = State "q_0^2"
-- define a final states
finalStateFirstTape = State "q_1^1"
finalStateSecondTape = State "q_2^2"

intermediateStateSecondTape = State "q_1^2"

mapSymbolToLetter x =
    case x of 
    T (Terminal c) -> c
    N (Nonterminal c) -> c
    E (Epsilon c) -> c

mapRelationSymbolToCommand workState prevLetter acc l inputTapeLetters =
        case l of
        [] -> (map (\x -> [
                SingleTapeCommand (
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter),
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (getDisjoinSymbol prevLetter, 
                            workState, 
                            rightBoundingLetter), 
                            (getDisjoinSymbol prevLetter, 
                            intermediateStateSecondTape, 
                            rightBoundingLetter)
                    )
                ]) inputTapeLetters) ++ acc
        (c : t) -> 
            mapRelationSymbolToCommand workState c 
                ((map (\x -> [
                    SingleTapeCommand (
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter),
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter)
                    ),
                    SingleTapeCommand (
                        (emptySymbol, 
                        workState, 
                        rightBoundingLetter), 
                        (getDisjoinSymbol c, 
                        workState,
                        rightBoundingLetter)
                        )
                ]) inputTapeLetters) ++ acc) t inputTapeLetters

mapRelationToTransition inputTapeLetters (Relation (Nonterminal nonterminalSymbol, symbols)) newState
        = mapRelationSymbolToCommand newState (head $ reverse symbols) (map (\x ->
            [
                SingleTapeCommand (
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter),
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter)
                ),
                SingleTapeCommand (
                    (Value nonterminalSymbol, 
                    intermediateStateSecondTape, 
                    rightBoundingLetter), 
                    (getDisjoinSymbol $ head $ reverse symbols, 
                    newState, 
                    rightBoundingLetter)
                    )
                ]) inputTapeLetters) (tail $ reverse symbols) inputTapeLetters

mapRelations f terminals relations i acc states =
    case relations of
        [] -> (acc, states)
        (Relation (Nonterminal start, [E (Epsilon eps)])) : t -> mapRelations f terminals t i ([
                                                                        [
                                                                        SingleTapeCommand (
                                                                            (leftBoundingLetter, 
                                                                            startStateFirstTape, 
                                                                            rightBoundingLetter),
                                                                            (leftBoundingLetter, 
                                                                            startStateFirstTape, 
                                                                            rightBoundingLetter)
                                                                        ),
                                                                        SingleTapeCommand (
                                                                            (Value start, 
                                                                            intermediateStateSecondTape, 
                                                                            rightBoundingLetter), 
                                                                            (emptySymbol, 
                                                                            intermediateStateSecondTape, 
                                                                            rightBoundingLetter)
                                                                            )
                                                                        ], 
                                                                        [
                                                                        SingleTapeCommand (
                                                                            (leftBoundingLetter, 
                                                                            startStateFirstTape, 
                                                                            rightBoundingLetter),
                                                                            (leftBoundingLetter, 
                                                                            startStateFirstTape, 
                                                                            rightBoundingLetter)
                                                                        ),
                                                                        SingleTapeCommand (
                                                                            (emptySymbol, 
                                                                            startStateSecondTape, 
                                                                            rightBoundingLetter), 
                                                                            (Value start, 
                                                                            intermediateStateSecondTape, 
                                                                            rightBoundingLetter)
                                                                            )
                                                                        ]] ++ (map (\x -> [
                                                                        SingleTapeCommand (
                                                                            (x, 
                                                                            startStateFirstTape, 
                                                                            rightBoundingLetter),
                                                                            (x, 
                                                                            startStateFirstTape, 
                                                                            rightBoundingLetter)
                                                                        ),
                                                                        SingleTapeCommand (
                                                                            (Value start, 
                                                                            intermediateStateSecondTape, 
                                                                            rightBoundingLetter), 
                                                                            (emptySymbol, 
                                                                            intermediateStateSecondTape, 
                                                                            rightBoundingLetter)
                                                                            )
                                                                        ]) terminals) ++ acc) states
        h : t -> mapRelations f terminals t (i + 1) ((f terminals h $ State ("q_" ++ show i ++ "^2")) ++ acc) (State ("q_" ++ show i ++ "^2") : states)
    

mapCfgToTM :: Grammar -> TM
mapCfgToTM 
    (Grammar
        (setOfNonterminals, 
        setOfTerminals, 
        setOfRelations, 
        Nonterminal startSymbol,
        Epsilon eps)
        ) = do
    let setOfNonterminalSquares = Set.map mapValue $ Set.map (\(Nonterminal x) -> x) setOfNonterminals
    let setOfTerminalSquares = Set.map mapValue $ Set.map (\(Terminal x) -> x) setOfTerminals 
    let setOfSecondTapeAlphabet = Set.union setOfNonterminalSquares $ Set.map getDisjoinSquare setOfTerminalSquares
    let tmInputAlphabet = InputAlphabet setOfTerminalSquares
    let tmTapeAlphabets = 
            [
                TapeAlphabet setOfTerminalSquares,
                TapeAlphabet setOfSecondTapeAlphabet
            ]
    let startStates = StartStates [startStateFirstTape, startStateSecondTape]
    let accessStates = AccessStates [finalStateFirstTape, finalStateSecondTape]
    -- define first transition
    let firstCommands = map (\x -> [
                SingleTapeCommand (
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter), 
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (emptySymbol, 
                    startStateSecondTape, 
                    rightBoundingLetter), 
                    (Value startSymbol, 
                    intermediateStateSecondTape, 
                    rightBoundingLetter)
                    )
                ]) $ Set.toList setOfTerminalSquares

    -- convert relations
    let listOfRelations = Set.elems setOfRelations
    --let listOfStatesForTransition = [State ("q_" ++ show i ++ "^2") | i <- [3..(length listOfRelations + 2)]]
    --let mappedRelationsSublists = zipWith (mapRelationToTransition $ Set.toList setOfTerminalLetters) listOfRelations listOfStatesForTransition
    let (mappedRelations, listOfStatesForTransition) = mapRelations mapRelationToTransition (Set.toList setOfTerminalSquares) listOfRelations 3 [] []
    -- map terminals to transitions
    let mappedTerminals = Set.map (\x -> 
            [
                SingleTapeCommand (
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter), 
                    (emptySymbol, 
                    startStateFirstTape, 
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (getDisjoinSquare x,
                    intermediateStateSecondTape,
                    rightBoundingLetter),
                    (emptySymbol,
                    intermediateStateSecondTape,
                    rightBoundingLetter)
                )
            ]) setOfTerminalSquares
    let acceptCommand = [SingleTapeCommand (
                (leftBoundingLetter, 
                startStateFirstTape, 
                rightBoundingLetter), 
                (leftBoundingLetter, 
                finalStateFirstTape, 
                rightBoundingLetter)
                ),
            SingleTapeCommand (
                (leftBoundingLetter, 
                intermediateStateSecondTape, 
                rightBoundingLetter), 
                (leftBoundingLetter, 
                finalStateSecondTape, 
                rightBoundingLetter)
                )
            ]
    let transitions = Set.union (Set.fromList (acceptCommand : firstCommands)) (Set.union mappedTerminals (Set.fromList mappedRelations))
    let multiTapeStates = MultiTapeStates [
            (Set.fromList [startStateFirstTape, finalStateFirstTape]),
            (Set.fromList (finalStateSecondTape : startStateSecondTape : intermediateStateSecondTape : listOfStatesForTransition))
            ]
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands transitions, startStates, accessStates)