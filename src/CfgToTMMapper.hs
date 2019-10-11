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

mapRelationSymbolToCommand states prevState acc l =
        case (l, states) of
        ([], []) -> [
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
                            prevState, 
                            rightBoundingLetter), 
                            (emptySymbol, 
                            intermediateStateSecondTape, 
                            rightBoundingLetter)
                    )
                ] : acc
        (symbol : t1, state : t2) -> 
            mapRelationSymbolToCommand t2 state ([
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
                        prevState, 
                        rightBoundingLetter), 
                        (getDisjoinSymbol symbol, 
                        state,
                        rightBoundingLetter)
                        )
                ]  : acc) t1

mapRelationToTransition (Relation (Nonterminal nonterminalSymbol, symbols)) newStates
        = mapRelationSymbolToCommand (tail newStates) startState [[
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
                    (getDisjoinSymbol startSymbol, 
                    startState, 
                    rightBoundingLetter)
                    )
                ]] (tail reversedSymbols)
            where   reversedSymbols = reverse symbols
                    startSymbol = head reversedSymbols
                    startState = head newStates

mapRelations relations i acc states =
    case relations of
        [] -> (acc, states)
        (Relation (Nonterminal start, [E (Epsilon eps)])) : t -> mapRelations t i ([
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
        h : t -> mapRelations t newI (commands ++ acc) $ newStates ++ states
            where   (Relation (_, symbols)) = h
                    newI = (+) i $ length symbols
                    newStates = map (\x -> State ("q_{" ++ show x ++ "}^2")) [i..newI-1]
                    commands = mapRelationToTransition h newStates
    

mapCfgToTM :: Grammar -> TM
mapCfgToTM 
    (Grammar
        (setOfNonterminals, 
        setOfTerminals, 
        setOfRelations, 
        Nonterminal startSymbol,
        Epsilon eps)
        ) = do
    let nonterminalSquares = mapValue $ map (\(Nonterminal x) -> x) $ Set.elems setOfNonterminals
    let terminalSquares = mapValue $ map (\(Terminal x) -> x) $ Set.elems setOfTerminals 
    let setOfSecondTapeAlphabet = Set.fromList $ (++) nonterminalSquares $ map getDisjoinSquare terminalSquares
    let setOfTerminalSquares = Set.fromList terminalSquares
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
    let (mappedRelations, listOfStatesForTransition) = mapRelations listOfRelations 3 [] []
    -- map terminals to transitions
    let mappedTerminals = map (\x -> 
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
            ]) terminalSquares
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
    let transitions = Set.fromList ([acceptCommand, firstCommand] ++ mappedTerminals ++ mappedRelations)
    let multiTapeStates = MultiTapeStates [
            (Set.fromList [startStateFirstTape, finalStateFirstTape]),
            (Set.fromList (finalStateSecondTape : startStateSecondTape : intermediateStateSecondTape : listOfStatesForTransition))
            ]
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands transitions, startStates, accessStates)