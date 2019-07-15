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

mapRelationSymbolToCommand workState prevLetter acc l =
        case l of
        [] -> [
                SingleTapeCommand (
                    (emptySymbol, 
                    startStateFirstTape, 
                    rightBoundingLetter),
                    (emptySymbol, 
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
                ] : acc
        (c : t) -> 
            mapRelationSymbolToCommand workState c ([
                    SingleTapeCommand (
                    (emptySymbol, 
                    startStateFirstTape, 
                    rightBoundingLetter),
                    (emptySymbol, 
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
                ]  : acc) t

mapRelationToTransition (Relation (Nonterminal nonterminalSymbol, symbols)) newState
        = mapRelationSymbolToCommand newState (head $ reverse symbols) [[
                SingleTapeCommand (
                    (emptySymbol, 
                    startStateFirstTape, 
                    rightBoundingLetter),
                    (emptySymbol, 
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
                ]] (tail $ reverse symbols)

mapRelations f relations i acc states =
    case relations of
        [] -> (acc, states)
        (Relation (Nonterminal start, [E (Epsilon eps)])) : t -> mapRelations f t i ([
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
                                                                        ],
                                                                        [
                                                                        SingleTapeCommand (
                                                                            (emptySymbol, 
                                                                            startStateFirstTape, 
                                                                            rightBoundingLetter),
                                                                            (emptySymbol, 
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
                                                                        ]] ++ acc) states
        h : t -> mapRelations f t (i + 1) ((f h $ State ("q_" ++ show i ++ "^2")) ++ acc) (State ("q_" ++ show i ++ "^2") : states)
    

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
    let firstCommand = [
                SingleTapeCommand (
                    (emptySymbol, 
                    startStateFirstTape, 
                    rightBoundingLetter), 
                    (emptySymbol, 
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
                ]

    -- convert relations
    let listOfRelations = Set.elems setOfRelations
    let (mappedRelations, listOfStatesForTransition) = mapRelations mapRelationToTransition listOfRelations 3 [] []
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
    let transitions = Set.union (Set.fromList ([acceptCommand, firstCommand])) (Set.union mappedTerminals (Set.fromList mappedRelations))
    let multiTapeStates = MultiTapeStates [
            (Set.fromList [startStateFirstTape, finalStateFirstTape]),
            (Set.fromList (finalStateSecondTape : startStateSecondTape : intermediateStateSecondTape : listOfStatesForTransition))
            ]
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands transitions, startStates, accessStates)