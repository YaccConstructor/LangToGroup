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
finalStateFirstTape = State "q_2^1"
finalStateSecondTape = State "q_2^2"

intermediateStateSecondTape = State "q_1^2"

mapSymbolToLetter x =
    case x of 
    T (Terminal c) -> c
    N (Nonterminal c) -> c

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
                    (getDisjoinLetter prevLetter, 
                            workState, 
                            rightBoundingLetter), 
                            (getDisjoinLetter prevLetter, 
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
                        (getDisjoinLetter c, 
                        workState,
                        rightBoundingLetter)
                        )
                ]) inputTapeLetters) ++ acc) t inputTapeLetters

mapRelationToTransition inputTapeLetters (Relation (Nonterminal nonterminalSymbol, symbols)) newState
        = mapRelationSymbolToCommand newState (mapSymbolToLetter (head $ reverse symbols)) (map (\x ->
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
                    (nonterminalSymbol, 
                    intermediateStateSecondTape, 
                    rightBoundingLetter), 
                    (getDisjoinLetter $ mapSymbolToLetter $ head $ reverse symbols, 
                    newState, 
                    rightBoundingLetter)
                    )
                ]) inputTapeLetters) (map mapSymbolToLetter (tail $ reverse symbols)) inputTapeLetters
    
    

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
                    (startSymbol, 
                    intermediateStateSecondTape, 
                    rightBoundingLetter)
                    )
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
                    startStateFirstTape, 
                    rightBoundingLetter), 
                    (emptySymbol, 
                    startStateFirstTape, 
                    rightBoundingLetter)
                    ),
                SingleTapeCommand (
                    (getDisjoinLetter x,
                    intermediateStateSecondTape,
                    rightBoundingLetter),
                    (emptySymbol,
                    intermediateStateSecondTape,
                    rightBoundingLetter)
                )
            ]) setOfTerminals
    let deleteStackCommands = map (\x -> [ 
                                            SingleTapeCommand (
                                                (leftBoundingLetter, 
                                                startStateFirstTape, 
                                                rightBoundingLetter), 
                                                (leftBoundingLetter, 
                                                startStateFirstTape, 
                                                rightBoundingLetter)
                                                ),
                                            SingleTapeCommand (
                                                (x, 
                                                intermediateStateSecondTape, 
                                                rightBoundingLetter), 
                                                (emptySymbol, 
                                                intermediateStateSecondTape, 
                                                rightBoundingLetter)
                                                )
                                            ]) $ Set.toList setOfSecondTapeAlphabet
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
    let transitions = Set.union (Set.fromList (acceptCommand : firstCommands ++ deleteStackCommands)) (Set.union mappedTerminals (Set.fromList mappedRelations))
    let multiTapeStates = MultiTapeStates [
            (Set.fromList [startStateFirstTape, finalStateFirstTape]),
            (Set.fromList (finalStateSecondTape : startStateSecondTape : intermediateStateSecondTape : listOfStatesForTransition))
            ]
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands transitions, startStates, accessStates)