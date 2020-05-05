module CFG2TM where

import GrammarType
import TMType
import qualified Data.Set as Set
import Helpers
import Data.List (elemIndices, groupBy, sortBy, sort, group)

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
first rels Eps = concatMap (\(Relation (n, _)) -> follow rels (N n)) . filter (\(Relation (_, e : _)) -> e == Eps) $ rels
first rels (N n) = concatMap (\(Relation (_, h : _)) -> first rels h) . filter (\(Relation (x, _)) -> x == n) $ rels

follow :: [Relation] -> Symbol -> [String]
follow rels n = concatMap (first rels) 
                    . concatMap (\(Relation (_, symb)) -> map ((!!) symb) . filter (< (length symb)) . map (+ 1) $ elemIndices n symb) 
                    . filter (\(Relation (_, symb)) -> elem n symb) $ rels

first2 :: [Symbol] -> [Relation] -> [String] -> [String]
first2 symb rels acc =
    case symb of
        Eps : t -> first2 t rels acc
        (N n) : t -> concatMap (\(Relation (_, s)) -> first2 (s ++ t) rels acc) . filter (\(Relation (x, _)) -> x == n) $ rels
        (T (Terminal term)) : t | length acc == 1 -> [term]
                                | otherwise -> first2 t rels (term : acc)
        [] -> []


genRelationCommand :: (Relation, State) -> [State] -> [Relation] -> ([State], [[TapeCommand]])
genRelationCommand (Relation (ns@(Nonterminal start), [Eps]), st) states rels = 
    (states,
        [[SingleTapeCommand ((LBS, sSFT, RBS), (LBS, sSFT, RBS)),
        SingleTapeCommand ((defValue start, iSST, RBS), (ES, iSST, RBS))],
        [SingleTapeCommand ((LBS, sSFT, RBS), (LBS, sSFT, RBS)),
        SingleTapeCommand ((ES, sSST, RBS), (defValue start, iSST, RBS))]] ++ followcmds)
            where
            followcmds = map (\fns -> [SingleTapeCommand ((defValue fns, st, RBS), (defValue fns, st, RBS)),
                                            SingleTapeCommand ((defValue start, iSST, RBS), (ES, iSST, RBS))]) $ follow rels $ N ns
genRelationCommand (Relation (Nonterminal nonterminalSymbol, [symbol]), st) states rels = 
    (states, 
        [[SingleTapeCommand ((defValue fnt, st, RBS), (defValue fnt, st, RBS)),
        SingleTapeCommand ((defValue nonterminalSymbol, iSST, RBS), (disjoinIfTerminal symbol, iSST, RBS))]])
            where
            [fnt] = first rels symbol
genRelationCommand (Relation (_, []), _) _ _ = error "Relation production is empty"
genRelationCommand (Relation (Nonterminal nonterminalSymbol, symbols), st) states rels = (newStates, lcmd : commands)
    where
        reversedSymbols = reverse symbols
        foldFunc acc x = (nextState : prevStates, cmd : prevCmds)
            where 
                (prevStates@(prevState : _), prevCmds) = acc
                nextState = genNextStateList prevStates
                cmd = [ SingleTapeCommand ((ES, st, RBS),(ES, st, RBS)),
                        SingleTapeCommand ((ES, prevState, RBS), (disjoinIfTerminal x, nextState, RBS))]
        hsymbol : tsymbols = reversedSymbols
        startState = genNextStateList states
        fnts = first rels $ head symbols
        makefcmd fnt = [SingleTapeCommand ((defValue fnt, st, RBS), (defValue fnt, st, RBS)),
                        SingleTapeCommand ((defValue nonterminalSymbol, iSST, RBS), (disjoinIfTerminal hsymbol, startState, RBS))]
        fcmds = map makefcmd fnts
        (newStates, commands) = foldl foldFunc (startState : states, fcmds) tsymbols
        lcmd = [SingleTapeCommand ((ES, st, RBS), (ES, sSFT, RBS)),
                SingleTapeCommand ((ES, head newStates, RBS), (ES, iSST, RBS))]
                


    
genEraseCommand :: Terminal -> [TapeCommand]
genEraseCommand (Terminal terminal) =  [SingleTapeCommand ((x, sSFT, RBS), (ES, sSFT, RBS)),
                                        SingleTapeCommand ((getDisjoinSquare x, iSST, RBS), (ES, iSST, RBS))]
                where 
                    x = defValue terminal
                    
genPreviewCommand :: [Relation] -> [State] -> ([State], [[TapeCommand]], [(Relation, State)])
genPreviewCommand rels states = if all checkDeterm groups then foldl func (states, [], []) groups else (states, [], map (\r -> (r, sSFT)) rels)
    where
        groups = groupBy (\(Relation (n1, h1 : _)) (Relation (n2, h2 : _)) -> n1 == n2 && (first rels h1) == (first rels h2)) . 
                    sortBy (\(Relation (n1, h1 : _)) (Relation (n2, h2 : _)) -> compare (n1, first rels h1) (n2, first rels h2) ) $ rels
        getFirst2 (Relation (_, symb)) = first2 symb rels []
        getFirst (Relation (_, h : _)) = first rels h
        getFirst (Relation (_, [])) = error "Empty symb"
        getNonterm (Relation (Nonterminal n, _)) = n
        isOne = (==) 1 . length . group . sort . getFirst2
        isAllDiff gr = (==) (length gr) . length . group . sort . concatMap getFirst2 $ gr
        checkDeterm [_] = True
        checkDeterm gr = all isOne gr && isAllDiff gr
        func (sts, commands, relState) [h] = (sts, commands, (h, sSFT) : relState)
        func (sts, commands, relState) gr = (newStates, newcmds, rs)
            where 
                fL = defValue $ head $ getFirst $ head gr
                nterm = defValue $ getNonterm $ head gr
                startState = genNextStateList states
                gencmds (newstates, cmds, rules) r = (endState : findState : newstates, newCmds ++ cmds, (r, endState) : rules)
                            where 
                                f2 = defValue $ head $ getFirst2 r
                                findState = genNextStateList newstates
                                endState = genNextStateList (findState : newstates)
                                newCmds = [ [SingleTapeCommand ((f2, startState, fL), (f2, findState, fL)), 
                                            SingleTapeCommand ((nterm, iSST, RBS), (nterm, iSST, RBS))],
                                            [SingleTapeCommand ((ES, findState, fL), (fL, findState, ES)), 
                                            SingleTapeCommand ((nterm, iSST, RBS), (nterm, iSST, RBS))],
                                            [SingleTapeCommand ((fL, findState, RBS), (fL, endState, RBS)), 
                                            SingleTapeCommand ((nterm, iSST, RBS), (nterm, iSST, RBS))]]
                startCmd = [SingleTapeCommand ((fL, sSFT, ES), (ES, startState, fL)), 
                            SingleTapeCommand ((nterm, iSST, RBS), (nterm, iSST, RBS))]
                (newStates, newcmds, rs) = foldl gencmds (startState : sts, startCmd : commands, relState) gr
                

cfg2tm :: Grammar -> TM
cfg2tm 
    (Grammar
        (setOfNonterminals, 
        setOfTerminals, 
        setOfRelations, 
        Nonterminal startSymbol)) = do
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
    let firstCommand = [SingleTapeCommand ((ES, sSFT, RBS), (ES, sSFT, RBS)),
                        SingleTapeCommand ((ES, sSST, RBS), (defValue startSymbol, iSST, RBS))]
    -- convert relations
    let rels = Set.elems setOfRelations
    let (firstStatesAfterPreview, previewCmds, relStates) = genPreviewCommand rels [sSFT, fSFT]
    let proxyGenRelation (states, acccmds) x = (newStates, cmds ++ acccmds)
            where
                (newStates, cmds) = genRelationCommand x states rels
    let (listOfStates, mappedRelations) = foldl proxyGenRelation ([fSST, sSST, iSST], []) $ relStates
    -- map terminals to transitions
    let mappedTerminals = map genEraseCommand terminalsList
    let acceptCommand = [SingleTapeCommand ((LBS, sSFT, RBS), (LBS, fSFT, RBS)),
                        SingleTapeCommand ((LBS, iSST, RBS), (LBS, fSST, RBS))]
    let transitions = Set.fromList ([acceptCommand, firstCommand] ++ mappedTerminals ++ mappedRelations ++ previewCmds)
    let multiTapeStates = MultiTapeStates [
            (Set.fromList firstStatesAfterPreview),
            (Set.fromList listOfStates)
            ]
    TM (tmInputAlphabet, tmTapeAlphabets, multiTapeStates, Commands transitions, startStates, accessStates)