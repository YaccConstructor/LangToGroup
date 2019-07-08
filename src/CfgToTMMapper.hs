module CfgToTMMapper where

import GrammarType
import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers

-- define a start states
startStateFirstTape = State "q_0^1"
startStateSecondTape = State "q_0^2"
-- define a intermediate states
intermediateStateFirstTape = State "q_1^1"
intermediateStateSecondTape = State "q_1^2"
-- define a final states
finalStateFirstTape = State "q_2^1"
finalStateSecondTape = State "q_2^2"

mapSymbolToLetter x =
    case x of 
    T (Terminal c) -> c
    N (Nonterminal c) -> c

mapRelationSymbolToCommand workState prevLetter acc l inputTapeLetters =
        case l of
        [] -> (map (\x -> [
                SingleTapeCommand (
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter),
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (leftBoundingLetter, 
                            workState, 
                            getDisjoinLetter prevLetter), 
                            (leftBoundingLetter, 
                            intermediateStateSecondTape, 
                            getDisjoinLetter prevLetter)
                    )
                ]) inputTapeLetters) ++ acc
        (c : t) -> 
            mapRelationSymbolToCommand workState c 
                ((concat $ map (\x -> [
                    [
                    SingleTapeCommand (
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter),
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter)
                    ),
                    SingleTapeCommand (
                        (emptySymbol, 
                        workState, 
                        getDisjoinLetter prevLetter), 
                        (getDisjoinLetter c, 
                        workState,
                        getDisjoinLetter prevLetter)
                        )], 
                [
                    SingleTapeCommand (
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter),
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter)
                    ),
                    SingleTapeCommand (
                        (getDisjoinLetter c, 
                        workState, 
                        emptySymbol), 
                        (emptySymbol, 
                        workState,
                        getDisjoinLetter c)
                        )]
                ]) inputTapeLetters) ++ acc) t inputTapeLetters

mapRelationToTransition inputTapeLetters (Relation (Nonterminal nonterminalSymbol, symbols)) newState
        = mapRelationSymbolToCommand newState (mapSymbolToLetter (head symbols)) (concat $ map (\x -> [ 
            [
                SingleTapeCommand (
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter),
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter)
                ),
                SingleTapeCommand (
                    (leftBoundingLetter, 
                    intermediateStateSecondTape, 
                    nonterminalSymbol), 
                    (leftBoundingLetter, 
                    newState, 
                    nonterminalSymbol)
                    )
                ],
            [
                SingleTapeCommand (
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter),
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter)
                ),
                SingleTapeCommand (
                    (leftBoundingLetter, 
                    newState, 
                    nonterminalSymbol), 
                    (leftBoundingLetter, 
                    newState, 
                    getDisjoinLetter $ mapSymbolToLetter (head symbols))
                    )
                ]
        ]) inputTapeLetters) (map mapSymbolToLetter (tail symbols)) inputTapeLetters
    
    

mapCfgToTM :: Grammar -> TM
mapCfgToTM 
    (Grammar
        (setOfNonterminals, 
        setOfTerminals, 
        setOfRelations, 
        Nonterminal startSymbol)
        ) = do
    let setOfTerminalLetters = Set.map (\(Terminal x) -> x) setOfTerminals
    let setOfNonterminalLetters = Set.map (\(Nonterminal x) -> x) setOfNonterminals
    let setOfSecondTapeAlphabet = Set.union setOfNonterminalLetters $ Set.map getDisjoinLetter setOfTerminalLetters
    let tmInputAlphabet = InputAlphabet setOfTerminalLetters
    let tmTapeAlphabets = 
            [
                TapeAlphabet setOfTerminalLetters,
                TapeAlphabet setOfSecondTapeAlphabet
            ]
    let startStates = StartStates [startStateFirstTape, startStateSecondTape]
    let accessStates = AccessStates [finalStateFirstTape, finalStateSecondTape]
    -- define first transition
    let firstCommands = concat $ map (\x -> [[ 
                SingleTapeCommand (
                    (x, 
                    startStateFirstTape, 
                    rightBoundingLetter), 
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (emptySymbol, 
                    startStateSecondTape, 
                    rightBoundingLetter), 
                    (startSymbol, 
                    startStateSecondTape, 
                    rightBoundingLetter)
                    )
                ],
            [ 
                SingleTapeCommand (
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter), 
                    (x, 
                    intermediateStateFirstTape,
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (startSymbol, 
                    startStateSecondTape, 
                    emptySymbol), 
                    (emptySymbol, 
                    intermediateStateSecondTape,
                    startSymbol)
                    )
                ]
            ]) $ Set.toList setOfTerminalLetters

    -- convert relations
    let listOfRelations = Set.elems setOfRelations
    let listOfStatesForTransition = [State ("q" ++ show i) | i <- [1..(length listOfRelations)]]
    let mappedRelationsSublists = zipWith (mapRelationToTransition $ Set.toList setOfTerminalLetters) listOfRelations listOfStatesForTransition
    let mappedRelations = foldl (++) [] mappedRelationsSublists
    -- map terminals to transitions
    let mappedTerminals = Set.map (\(Terminal x) -> 
            [
                SingleTapeCommand (
                    (x, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter), 
                    (emptySymbol, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (leftBoundingLetter,
                    intermediateStateSecondTape,
                    getDisjoinLetter x),
                    (leftBoundingLetter,
                    intermediateStateSecondTape,
                    emptySymbol)
                )
            ]) setOfTerminals
    let acceptCommand = [SingleTapeCommand (
                (leftBoundingLetter, 
                intermediateStateFirstTape, 
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
            ] : (map (\x -> [ 
                SingleTapeCommand (
                    (leftBoundingLetter, 
                    intermediateStateFirstTape, 
                    rightBoundingLetter), 
                    (leftBoundingLetter, 
                    finalStateFirstTape, 
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (leftBoundingLetter, 
                    intermediateStateSecondTape, 
                    x), 
                    (leftBoundingLetter, 
                    finalStateSecondTape, 
                    x)
                    )
                ]) $ Set.toList setOfSecondTapeAlphabet)
    let transitions = Set.union (Set.fromList (acceptCommand ++ firstCommands)) (Set.union mappedTerminals (Set.fromList mappedRelations))
    let multiTapeStates = MultiTapeStates [
            (Set.fromList [startStateFirstTape, intermediateStateFirstTape, finalStateFirstTape]),
            (Set.fromList (finalStateSecondTape : intermediateStateSecondTape : startStateSecondTape : listOfStatesForTransition))
            ]
    let generateEmptyAccessCommandsForSecondTape alphabet acc = case alphabet of
                                                                    (h : t) -> generateEmptyAccessCommandsForSecondTape t ([
                                                                        SingleTapeCommand (
                                                                            (leftBoundingLetter, 
                                                                            finalStateFirstTape, 
                                                                            rightBoundingLetter), 
                                                                            (leftBoundingLetter, 
                                                                            finalStateFirstTape, 
                                                                            rightBoundingLetter)
                                                                        ),
                                                                        SingleTapeCommand (
                                                                            (leftBoundingLetter, 
                                                                            finalStateSecondTape, 
                                                                            h), 
                                                                            (leftBoundingLetter, 
                                                                            finalStateSecondTape, 
                                                                            emptySymbol)
                                                                        )
                                                                        ] : acc)
                                                                    [] -> acc
    let transitionsWithEmptyAccessCommands = generateEmptyAccessCommandsForSecondTape (Set.toList setOfSecondTapeAlphabet) (Set.toList transitions)
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands (Set.fromList transitionsWithEmptyAccessCommands), startStates, accessStates)