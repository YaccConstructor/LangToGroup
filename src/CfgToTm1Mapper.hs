module CfgToTm1Mapper where

import GrammarType
import Tm1Type
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

mapSymbolToLetter x =
    case x of 
    T (Terminal c) -> Letter c
    N (Nonterminal c) -> Letter c

mapRelationSymbolToCommand workState prevLetter acc l =
        case l of
        [] -> Command [
                NoCommand, 
                SingleTapeCommand (
                    (emptySymbol, 
                            workState, 
                            prevLetter), 
                            (emptySymbol, 
                            intermediateStateSecondTape, 
                            prevLetter)
                    )
                ]  : acc
        (c : t) -> 
            mapRelationSymbolToCommand workState c 
                ([Command [
                    NoCommand,
                    SingleTapeCommand (
                        (emptySymbol, 
                        workState, 
                        prevLetter), 
                        (c, 
                        workState,
                        prevLetter)
                        )], 
                Command [
                    NoCommand,
                    SingleTapeCommand (
                        (c, 
                        workState, 
                        emptySymbol), 
                        (emptySymbol, 
                        workState,
                        c)
                        )]
                ] ++ acc) t

mapRelationToTransition (Relation (Nonterminal nonterminalSymbol, symbols)) newState
        = mapRelationSymbolToCommand newState (mapSymbolToLetter (head symbols)) [ 
            Command [
                NoCommand,
                SingleTapeCommand (
                    (emptySymbol, 
                    intermediateStateSecondTape, 
                    Letter nonterminalSymbol), 
                    (emptySymbol, 
                    newState, 
                    Letter nonterminalSymbol)
                    )
                ],
            Command [
                NoCommand,
                SingleTapeCommand (
                    (emptySymbol, 
                    newState, 
                    Letter nonterminalSymbol), 
                    (emptySymbol, 
                    newState, 
                    mapSymbolToLetter (head symbols))
                    )
                ]
        ] (map mapSymbolToLetter (tail symbols))
    
    

mapCfgToTm1 :: Grammar -> TM1
mapCfgToTm1 
    (Grammar
        (Nonterminals setOfNonterminals, 
        Terminals setOfTerminals, 
        Relations setOfRelations, 
        Nonterminal startSymbol)
        ) = do
    let setOfTerminalLetters = Set.map (\(Terminal x) -> Letter x) setOfTerminals
    let setOfNonterminalLetters = Set.map (\(Nonterminal x) -> Letter x) setOfNonterminals 
    let tm1InputAlphabet = InputAlphabet setOfTerminalLetters
    let tm1TapeAlphabet = TapeAlphabet (Set.union setOfTerminalLetters setOfNonterminalLetters)
    let startStates = StartStates [startStateFirstTape, startStateSecondTape]
    let accessStates = AccessStates [finalStateFirstTape, finalStateSecondTape]
    -- define first transition
    let firstCommands = 
            [ Command [ 
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
                    (Letter startSymbol, 
                    startStateSecondTape, 
                    rightBoundingLetter)
                    )
                ],
            Command [ 
                NoCommand,
                SingleTapeCommand (
                    (Letter startSymbol, 
                    startStateSecondTape, 
                    emptySymbol), 
                    (emptySymbol, 
                    intermediateStateSecondTape,
                    Letter startSymbol)
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
            Command [
                SingleTapeCommand (
                    (Letter x, 
                    intermediateStateFirstTape, 
                    emptySymbol), 
                    (emptySymbol, 
                    intermediateStateFirstTape, 
                    Letter x)
                    ),
                SingleTapeCommand (
                    (emptySymbol,
                    intermediateStateSecondTape,
                    Letter x),
                    (Letter x,
                    intermediateStateSecondTape,
                    emptySymbol)
                )
            ]) setOfTerminals
    let acceptCommand = Command [ 
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
            States (Set.fromList [startStateFirstTape, intermediateStateFirstTape, finalStateFirstTape]),
            States (Set.fromList (finalStateSecondTape : intermediateStateSecondTape : startStateSecondTape : listOfStatesForTransition))
            ]
    TM1 (tm1InputAlphabet, tm1TapeAlphabet, multiTapeStates, Commands transitions, startStates, accessStates)