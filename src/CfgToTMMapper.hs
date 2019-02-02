module CfgToTMMapper where

import GrammarType
import TMType
import Data.Set (Set)
import qualified Data.Set as Set

-- define a start states
startStateFirstTape = State "q_0^1"
startStateSecondTape = State "q_0^2"
-- define a intermediate states
intermediateStateFirstTape = State "q_1^1"
intermediateStateSecondTape = State "q_1^2"
-- define a final states
finalStateFirstTape = State "q_2^1"
finalStateSecondTape = State "q_2^2"

getDisjoinLetter letter = letter ++ "'"

mapSymbolToLetter x =
    case x of 
    T (Terminal c) -> c
    N (Nonterminal c) -> c

mapRelationSymbolToCommand workState prevLetter acc l =
        case l of
        [] -> [
                NoCommand, 
                SingleTapeCommand (
                    (emptySymbol, 
                            workState, 
                            getDisjoinLetter prevLetter), 
                            (emptySymbol, 
                            intermediateStateSecondTape, 
                            getDisjoinLetter prevLetter)
                    )
                ]  : acc
        (c : t) -> 
            mapRelationSymbolToCommand workState c 
                ([[
                    NoCommand,
                    SingleTapeCommand (
                        (emptySymbol, 
                        workState, 
                        getDisjoinLetter prevLetter), 
                        (c, 
                        workState,
                        getDisjoinLetter prevLetter)
                        )], 
                [
                    NoCommand,
                    SingleTapeCommand (
                        (getDisjoinLetter c, 
                        workState, 
                        emptySymbol), 
                        (emptySymbol, 
                        workState,
                        getDisjoinLetter c)
                        )]
                ] ++ acc) t

mapRelationToTransition (Relation (Nonterminal nonterminalSymbol, symbols)) newState
        = mapRelationSymbolToCommand newState (mapSymbolToLetter (head symbols)) [ 
            [
                NoCommand,
                SingleTapeCommand (
                    (emptySymbol, 
                    intermediateStateSecondTape, 
                    nonterminalSymbol), 
                    (emptySymbol, 
                    newState, 
                    nonterminalSymbol)
                    )
                ],
            [
                NoCommand,
                SingleTapeCommand (
                    (emptySymbol, 
                    newState, 
                    nonterminalSymbol), 
                    (emptySymbol, 
                    newState, 
                    getDisjoinLetter $ mapSymbolToLetter (head symbols))
                    )
                ]
        ] (map mapSymbolToLetter (tail symbols))
    
    

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
    let tmInputAlphabet = InputAlphabet setOfTerminalLetters
    let tmTapeAlphabets = 
            [
                TapeAlphabet setOfTerminalLetters,
                TapeAlphabet (Set.union setOfNonterminalLetters (Set.map getDisjoinLetter setOfTerminalLetters))
            ]
    let startStates = StartStates [startStateFirstTape, startStateSecondTape]
    let accessStates = AccessStates [finalStateFirstTape, finalStateSecondTape]
    -- define first transition
    let firstCommands = 
            [[ 
                SingleTapeCommand (
                    (emptySymbol, 
                    startStateFirstTape, 
                    rightBoundingLetter), 
                    (emptySymbol, 
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
                NoCommand,
                SingleTapeCommand (
                    (startSymbol, 
                    startStateSecondTape, 
                    emptySymbol), 
                    (emptySymbol, 
                    intermediateStateSecondTape,
                    startSymbol)
                    )
                ]
            ]

    -- convert relations
    let listOfRelations = Set.elems setOfRelations
    let listOfStatesForTransition = [State ("q" ++ show i) | i <- [1..(length listOfRelations)]]
    let mappedRelationsSublists = zipWith mapRelationToTransition listOfRelations listOfStatesForTransition
    let mappedRelations = foldl (++) [] mappedRelationsSublists
    -- map terminals to transitions
    let mappedTerminals = Set.map (\(Terminal x) -> 
            [
                SingleTapeCommand (
                    (x, 
                    intermediateStateFirstTape, 
                    emptySymbol), 
                    (emptySymbol, 
                    intermediateStateFirstTape, 
                    x)
                    ),
                SingleTapeCommand (
                    (emptySymbol,
                    intermediateStateSecondTape,
                    getDisjoinLetter x),
                    (getDisjoinLetter x,
                    intermediateStateSecondTape,
                    emptySymbol)
                )
            ]) setOfTerminals
    let acceptCommand = [ 
                SingleTapeCommand (
                    (leftBoundingLetter, 
                    intermediateStateFirstTape, 
                    emptySymbol), 
                    (leftBoundingLetter, 
                    finalStateFirstTape, 
                    emptySymbol)
                    ),
                SingleTapeCommand (
                    (emptySymbol, 
                    intermediateStateSecondTape, 
                    emptySymbol), 
                    (emptySymbol, 
                    finalStateSecondTape, 
                    emptySymbol)
                    )
                ]
    let transitions = Set.union (Set.fromList (acceptCommand : firstCommands)) (Set.union mappedTerminals (Set.fromList mappedRelations))
    let multiTapeStates = MultiTapeStates [
            (Set.fromList [startStateFirstTape, intermediateStateFirstTape, finalStateFirstTape]),
            (Set.fromList (finalStateSecondTape : intermediateStateSecondTape : startStateSecondTape : listOfStatesForTransition))
            ]
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands transitions, startStates, accessStates)