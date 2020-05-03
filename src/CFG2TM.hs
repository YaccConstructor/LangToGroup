module CFG2TM where

import GrammarType
import TMType
import qualified Data.Set as Set
import Helpers
import Data.List (elemIndices, groupBy, sortBy)

-- define a start states
sSFT :: State
sSFT = State "q_{0}^{1}"
sSST :: State
sSST = State "q_{0}^{2}"
-- define a final states
fSFT :: State
fSFT = State "q_{1}^{1}"
fSST :: State
fSST = State "q_{2}^{2}"
iSST :: State
iSST = State "q_{1}^{2}"

first :: [Relation] -> Symbol -> [String]
first _ (T (Terminal t)) = [t]
first rels e@(E _) = concat . map (\(Relation (n, _)) -> follow rels (N n)) . filter (\(Relation (_, eps : _)) -> e == eps) $ rels
first rels (N n) = concat . map (\(Relation (_, h : _)) -> first rels h) . filter (\(Relation (x, _)) -> x == n) $ rels

follow :: [Relation] -> Symbol -> [String]
follow rels n = concat 
                    . map (first rels) 
                    . concat 
                    . map (\(Relation (_, symb)) -> map ((!!) symb) . filter (< (length symb)) . map (+ 1) $ elemIndices n symb) 
                    . filter (\(Relation (_, symb)) -> elem n symb) $ rels

first2 :: [Symbol] -> [Relation] -> [String] -> [String]
first2 symb rels acc =
    case symb of
        (E _) : t -> first2 t rels acc
        (N n) : t -> concat . map (\(Relation (_, s)) -> first2 (s ++ t) rels acc) . filter (\(Relation (x, _)) -> x == n) $ rels
        (T (Terminal term)) : t | length acc == 1 -> [term]
                                | otherwise -> first2 t rels (term : acc)
        [] -> []


genRelationCommand :: (Relation, State) -> [State] -> [Relation] -> ([State], [[TapeCommand]])
genRelationCommand (Relation (ns@(Nonterminal start), [E _]), st) states rels = 
    (states,
    [[SingleTapeCommand ((lBL, sSFT, rBL), (lBL, sSFT, rBL)),
    SingleTapeCommand ((Value start, iSST, rBL), (eL, iSST, rBL))],
    [SingleTapeCommand ((lBL, sSFT, rBL), (lBL, sSFT, rBL)),
    SingleTapeCommand ((eL, sSST, rBL), (Value start, iSST, rBL))]] ++ followcmds)
    where
        followcmds = map (\fns -> [SingleTapeCommand ((Value fns, st, rBL), (Value fns, st, rBL)),
                                        SingleTapeCommand ((Value start, iSST, rBL), (eL, iSST, rBL))]) $ follow rels $ N ns
genRelationCommand (Relation (Nonterminal nonterminalSymbol, [symbol]), st) states rels = 
    (states, 
    [[SingleTapeCommand ((Value fnt, st, rBL), (Value fnt, st, rBL)),
    SingleTapeCommand ((Value nonterminalSymbol, iSST, rBL), (getDisjoinSymbol symbol, iSST, rBL))]])
    where
        [fnt] = first rels symbol
genRelationCommand (Relation (Nonterminal nonterminalSymbol, symbols), st) states rels = (newStates, lcmd : commands)
    where
        reversedSymbols = reverse symbols
        foldFunc acc x = (nextState : prevStates, cmd : prevCmds)
            where 
                (prevStates@(prevState : _), prevCmds) = acc
                nextState = genNextStateList prevStates
                cmd = [ SingleTapeCommand ((eL, st, rBL),(eL, st, rBL)),
                        SingleTapeCommand ((eL, prevState, rBL), (getDisjoinSymbol x, nextState, rBL))]
        hsymbol : tsymbols = reversedSymbols
        startState = genNextStateList states
        fnts = first rels $ head symbols
        makefcmd fnt = [SingleTapeCommand ((Value fnt, st, rBL), (Value fnt, st, rBL)),
                        SingleTapeCommand ((Value nonterminalSymbol, iSST, rBL), (getDisjoinSymbol hsymbol, startState, rBL))]
        fcmds = map makefcmd fnts
        (newStates, commands) = foldl foldFunc (startState : states, fcmds) tsymbols
        lcmd = [SingleTapeCommand ((eL, st, rBL), (eL, sSFT, rBL)),
                SingleTapeCommand ((eL, head newStates, rBL), (eL, iSST, rBL))]
                


    
genEraseCommand :: Terminal -> [TapeCommand]
genEraseCommand (Terminal terminal) =  [SingleTapeCommand ((x, sSFT, rBL), (eL, sSFT, rBL)),
                                        SingleTapeCommand ((getDisjoinSquare x, iSST, rBL), (eL, iSST, rBL))]
                where 
                    x = Value terminal
                    
genPreviewCommand :: [Relation] -> [State] -> ([State], [[TapeCommand]], [(Relation, State)])
genPreviewCommand rels states = if all checkDeterm groups then foldl func (states, [], []) groups else (states, [], map (\r -> (r, sSFT)) rels)
    where
        groups = groupBy (\(Relation (n1, h1 : _)) (Relation (n2, h2 : _)) -> n1 == n2 && (first rels h1) == (first rels h2)) . 
                    sortBy (\(Relation (n1, h1 : _)) (Relation (n2, h2 : _)) -> compare (n1, first rels h1) (n2, first rels h2) ) $ rels
        getFirst2 (Relation (_, symb)) = first2 symb rels []
        getFirst (Relation (_, h : _)) = first rels h
        getFirst (Relation (_, [])) = error "Empty symb"
        getNonterm (Relation (Nonterminal n, _)) = n
        isOne = (==) 1 . length . Set.fromList . getFirst2
        isAllDiff gr = (==) (length gr) . length . Set.fromList . concat . map getFirst2 $ gr
        checkDeterm [_] = True
        checkDeterm gr = all isOne gr && isAllDiff gr
        func (sts, commands, relState) [h] = (sts, commands, (h, sSFT) : relState)
        func (sts, commands, relState) gr = (newStates, newcmds, rs)
            where 
                fL = Value $ head $ getFirst $ head gr
                nterm = Value $ getNonterm $ head gr
                startState = genNextStateList states
                gencmds (newstates, cmds, rules) r = (endState : findState : newstates, newCmds ++ cmds, (r, endState) : rules)
                            where 
                                f2 = Value $ head $ getFirst2 r
                                findState = genNextStateList newstates
                                endState = genNextStateList (findState : newstates)
                                newCmds = [ [SingleTapeCommand ((f2, startState, fL), (f2, findState, fL)), 
                                            SingleTapeCommand ((nterm, iSST, rBL), (nterm, iSST, rBL))],
                                            [SingleTapeCommand ((eL, findState, fL), (fL, findState, eL)), 
                                            SingleTapeCommand ((nterm, iSST, rBL), (nterm, iSST, rBL))],
                                            [SingleTapeCommand ((fL, findState, rBL), (fL, endState, rBL)), 
                                            SingleTapeCommand ((nterm, iSST, rBL), (nterm, iSST, rBL))]]
                startCmd = [SingleTapeCommand ((fL, sSFT, eL), (eL, startState, fL)), 
                            SingleTapeCommand ((nterm, iSST, rBL), (nterm, iSST, rBL))]
                (newStates, newcmds, rs) = foldl gencmds (startState : sts, startCmd : commands, relState) gr
                

cfg2tm :: Grammar -> TM
cfg2tm 
    (Grammar
        (setOfNonterminals, 
        setOfTerminals, 
        setOfRelations, 
        Nonterminal startSymbol,
        _)
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
    let rels = Set.elems setOfRelations
    let (firstStatesAfterPreview, previewCmds, relStates) = genPreviewCommand rels [sSFT, fSFT]
    let proxyGenRelation (states, acccmds) x = (newStates, cmds ++ acccmds)
            where
                (newStates, cmds) = genRelationCommand x states rels
    let (listOfStates, mappedRelations) = foldl proxyGenRelation ([fSST, sSST, iSST], []) $ relStates
    -- map terminals to transitions
    let mappedTerminals = map genEraseCommand terminalsList
    let acceptCommand = [SingleTapeCommand ((lBL, sSFT, rBL), (lBL, fSFT, rBL)),
                        SingleTapeCommand ((lBL, iSST, rBL), (lBL, fSST, rBL))]
    let transitions = Set.fromList ([acceptCommand, firstCommand] ++ mappedTerminals ++ mappedRelations ++ previewCmds)
    let multiTapeStates = MultiTapeStates [
            (Set.fromList firstStatesAfterPreview),
            (Set.fromList listOfStates)
            ]
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands transitions, startStates, accessStates)