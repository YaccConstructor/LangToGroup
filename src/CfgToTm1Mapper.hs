module CfgToTm1Mapper where

import GrammarType
import Tm1Type
import Data.Set (Set)
import qualified Data.Set as Set

-- define start states
startStateFirstTape = State "q_0^1"
startStateSecondTape = State "q_0^2"
-- define final states
finalStateFirstTape = State "q_1^1"
finalStateSecondTape = State "q_1^2"

mapSymbolToLetter x =
    case x of 
    T (Terminal c) -> Letter c
    N (Nonterminal c) -> Letter c

mapRelationSymbolToCommand workState prevLetter acc l =
        case l of
        [] -> acc
        [c] -> 
            [Command [
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
                    finalStateSecondTape,
                    c)
                    )]
            ] ++ acc
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
                        finalStateSecondTape,
                        c)
                        )]
                ] ++ acc) t

mapRelationToTransition (Relation (Nonterminal nonterminalSymbol, symbols)) newState
        = [ 
            Command [
                NoCommand,
                SingleTapeCommand (
                    (emptySymbol, 
                    finalStateSecondTape, 
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
        ] ++ mapRelationSymbolToCommand newState (mapSymbolToLetter (head symbols)) [] (map mapSymbolToLetter (tail symbols))
    
    

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
                    finalStateFirstTape, 
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
                    finalStateSecondTape,
                    Letter startSymbol)
                    )
                ]
            ]

    -- convert relations
    let listOfRelations = Set.elems setOfRelations
    let listOfStatesForTransition = [State ("q" ++ show i) | i <- [1..(length listOfRelations)]]
    let mappedRelationsSublists = zipWith (mapRelationToTransition) listOfRelations listOfStatesForTransition
    let mappedRelations = foldl (++) [] mappedRelationsSublists
    -- map terminals to transitions
    let mappedTerminals = Set.map (\(Terminal x) -> 
            Command [
                SingleTapeCommand (
                    (Letter x, 
                    finalStateFirstTape, 
                    emptySymbol), 
                    (emptySymbol, 
                    finalStateFirstTape, 
                    Letter x)
                    ),
                SingleTapeCommand (
                    (emptySymbol,
                    finalStateSecondTape,
                    Letter x),
                    (Letter x,
                    finalStateSecondTape,
                    emptySymbol)
                )
            ]) setOfTerminals
    let transitions = Set.union (Set.fromList firstCommands) (Set.union mappedTerminals (Set.fromList mappedRelations))
    let multiTapeStates = MultiTapeStates [
            States (Set.fromList [startStateFirstTape, finalStateFirstTape]),
            States (Set.fromList (finalStateSecondTape : (startStateSecondTape : listOfStatesForTransition)))
            ]
    TM1 (tm1InputAlphabet, tm1TapeAlphabet, multiTapeStates, Commands transitions, startStates, accessStates)