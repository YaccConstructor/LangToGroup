module CfgToTm1Mapper where

import qualified PdaType
import qualified GrammarType
import qualified Tm1Type
import Data.Set (Set)
import qualified Data.Set as Set

-- at first we need convert a cfg to a pda
mapCfgToPda :: GrammarType.Grammar -> PdaType.Pda
mapCfgToPda 
    (GrammarType.Grammar
        (GrammarType.Nonterminals setOfNonterminals, 
        GrammarType.Terminals setOfTerminals, 
        GrammarType.Relations setOfRelations, 
        GrammarType.Nonterminal startSymbol)
        ) = do
    let setOfTerminalLetters = Set.map (\(GrammarType.Terminal x) -> PdaType.Letter x) setOfTerminals
    let setOfNonterminalLetters = Set.map (\(GrammarType.Nonterminal x) -> PdaType.Letter x) setOfNonterminals 
    let pdaInputAlphabet = PdaType.InputAlphabet setOfTerminalLetters
    let pdaStackAlphabet = PdaType.StackAlphabet (Set.union setOfTerminalLetters setOfNonterminalLetters)
    let startState = PdaType.State 's'
    let finalState = PdaType.State 'f'
    let states = PdaType.States (Set.fromList [startState, finalState])
    let mapSymbolToLetter x =
            case x of 
            GrammarType.T (GrammarType.Terminal c) -> PdaType.Letter c
            GrammarType.N (GrammarType.Nonterminal c) -> PdaType.Letter c
    let mapListOfSymbolsToListOfLetters = map mapSymbolToLetter
    let mappedRelations =
            Set.map (\(GrammarType.Relation (GrammarType.Nonterminal nonterminalSymbol, symbols)) -> 
                PdaType.TransitionRelation (
                    (finalState, PdaType.emptySymbol, PdaType.Letter nonterminalSymbol), 
                    (finalState, mapListOfSymbolsToListOfLetters symbols)
                    ))
                setOfRelations
    let transitionsFromTerminals = Set.map 
            (\letter -> 
                PdaType.TransitionRelation (
                    (finalState, letter, letter), 
                    (finalState, [PdaType.emptySymbol])
                    )) 
                    setOfTerminalLetters
    let transitions = Set.insert 
            (PdaType.TransitionRelation (
                (startState, PdaType.emptySymbol, PdaType.emptySymbol), 
                (finalState, [PdaType.Letter startSymbol])
                )) 
            (Set.union mappedRelations transitionsFromTerminals)
    PdaType.Pda (
        states, 
        pdaInputAlphabet, 
        pdaStackAlphabet, 
        PdaType.TransitionRelations transitions, 
        startState, 
        PdaType.InitialStackSymbols [], 
        PdaType.AcceptingStates (PdaType.States (Set.fromList [finalState]))
        )

-- mapPdaToTm1 :: PdaType.Pda -> Tm1Type.TM1
-- mapPdaToTm1 
--     (PdaType.Pda 
--         (PdaType.States setOfStates, 
--         PdaType.InputAlphabet setOfInputLetters, 
--         PdaType.StackAlphabet setOfStackLetters, 
--         PdaType.TransitionRelations setOfTransitions, 
--         startState, 
--         PdaType.InitialStackSymbols listOfInitialStackLetters, 
--         PdaType.AcceptingStates)
--         ) = do