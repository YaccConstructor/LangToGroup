module CFG2TM where

import GrammarType
import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers

-- define a start states
sSFT = State "q_{0}^{1}"
sSST = State "q_{0}^{2}"
-- define a final states
fSFT = State "q_{1}^{1}"
fSST = State "q_{2}^{2}"

iSST = State "q_{1}^{2}"

genRelationCommand :: Relation -> [State] -> ([State], [[TapeCommand]])
genRelationCommand (Relation (Nonterminal start, [E (Epsilon eps)])) states = 
    (states,
    [[SingleTapeCommand ((eL, sSFT, rBL), (eL, sSFT, rBL)),
    SingleTapeCommand ((Value start, iSST, rBL), (eL, iSST, rBL))]])
genRelationCommand (Relation (Nonterminal nonterminalSymbol, symbols)) states = (newStates, lcmd : commands)
    where
        reversedSymbols = reverse symbols
        foldFunc acc x = (nextState : prevStates, cmd : prevCmds)
            where 
                (prevStates@(prevState : _), prevCmds) = acc
                nextState = genNextStateList prevStates
                cmd = [ SingleTapeCommand ((eL, sSFT, rBL),(eL, sSFT, rBL)),
                        SingleTapeCommand ((eL, prevState, rBL), (getDisjoinSymbol x, nextState, rBL))]
        hsymbol : tsymbols = reversedSymbols
        startState = genNextStateList states
        fcmd = [SingleTapeCommand ((eL, sSFT, rBL), (eL, sSFT, rBL)),
                SingleTapeCommand ((Value nonterminalSymbol, iSST, rBL), (getDisjoinSymbol hsymbol, startState, rBL))]
        (newStates, commands) = foldl foldFunc (startState : states, [fcmd]) tsymbols
        lcmd = [SingleTapeCommand ((eL, sSFT, rBL), (eL, sSFT, rBL)),
                SingleTapeCommand ((eL, head newStates, rBL), (eL, iSST, rBL))]
                


    
genEraseCommand :: Terminal -> [TapeCommand]
genEraseCommand (Terminal terminal) =  [SingleTapeCommand ((x, sSFT, rBL), (eL, sSFT, rBL)),
                                        SingleTapeCommand ((getDisjoinSquare x, iSST, rBL), (eL, iSST, rBL))]
                where 
                    x = Value terminal
                    

cfg2tm :: Grammar -> TM
cfg2tm 
    (Grammar
        (setOfNonterminals, 
        setOfTerminals, 
        setOfRelations, 
        Nonterminal startSymbol,
        Epsilon eps)
        ) = do
    let terminalsList = Set.toList setOfTerminals
    let nonterminalSquares = mapValue $ map (\(Nonterminal x) -> x) $ Set.elems setOfNonterminals
    let terminalSquares = mapValue $ map (\(Terminal x) -> x) $ terminalsList 
    let setOfSecondTapeAlphabet = Set.fromList $ (++) nonterminalSquares $ map getDisjoinSquare terminalSquares
    let setOfTerminalSquares = Set.fromList terminalSquares
    let tmInputAlphabet = InputAlphabet setOfTerminalSquares
    let tmTapeAlphabets = 
            [
                TapeAlphabet setOfTerminalSquares,
                TapeAlphabet setOfSecondTapeAlphabet
            ]
    let startStates = StartStates [sSFT, sSST]
    let accessStates = AccessStates [fSFT, fSST]
    -- define first transition
    let firstCommand = [SingleTapeCommand ((eL, sSFT, rBL), (eL, sSFT, rBL)),
                        SingleTapeCommand ((eL, sSST, rBL), (Value startSymbol, iSST, rBL))]

    -- convert relations
    let proxyGenRelation (states, acccmds) x = (newStates, cmds ++ acccmds)
            where
                (newStates, cmds) = genRelationCommand x states
    let (listOfStates, mappedRelations) = foldl proxyGenRelation ([fSST, sSST, iSST], []) $ Set.elems setOfRelations
    -- map terminals to transitions
    let mappedTerminals = map genEraseCommand terminalsList
    let acceptCommand = [SingleTapeCommand ((lBL, sSFT, rBL), (lBL, fSFT, rBL)),
                        SingleTapeCommand ((lBL, iSST, rBL), (lBL, fSST, rBL))]
    let transitions = Set.fromList ([acceptCommand, firstCommand] ++ mappedTerminals ++ mappedRelations)
    let multiTapeStates = MultiTapeStates [
            (Set.fromList [sSFT, fSFT]),
            (Set.fromList listOfStates)
            ]
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands transitions, startStates, accessStates)