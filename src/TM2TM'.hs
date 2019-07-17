module TM2TM' where

import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers
import Text.Regex.TDFA
import Debug.Trace

disjoinAcceptCommandWithOthers commands accessStates = do
    let isAcceptCommand command access = 
            case (command, access) of 
                ((SingleTapeCommand ((_, _, _), (_, sh, _))) : st, ah : at) -> if sh == ah then isAcceptCommand st at else False
                ([], []) -> True
    let disjoinAcceptCommandWithOthersInternal allCommands acc = 
            case allCommands of
                (h : t) -> if isAcceptCommand h accessStates then (h, acc ++ t) else disjoinAcceptCommandWithOthersInternal t (h : acc)
                [] -> error "No accept command"

    disjoinAcceptCommandWithOthersInternal commands []

firstPhaseFinalStatesTransmition tape = do
    let tapeList = Set.toList tape
    let getNumber (State s) = read n :: Int
            where (_, _, _, [n]) = s =~ "q_{?([0-9]+)}?\\^{?[0-9]+\\.?[0-9]*}?" :: (String, String, String, [String])
    let stateNumber = (+) 1 $ maximum $ map getNumber tapeList
    let getTapeNumber (State s) = n
            where (_, _, _, [n]) = s =~ "q_{?[0-9]+}?\\^{?([0-9]+\\.?[0-9]*)}?" :: (String, String, String, [String])
    let tapeNumber = getTapeNumber $ head tapeList
    State $ "q_{" ++ (show stateNumber) ++ "}^{" ++ tapeNumber ++ "}"
          

firstPhase startKPlusOneTapeState kplus1tapeState finalKPlusOneTapeState acceptCommand otherCommands startStates multiTapeStates = do

    let generateFirstPhaseCommand command states acc =
            case states of
                [] -> 
                    reverse $ (SingleTapeCommand ((emptySymbol, kplus1tapeState, rightBoundingLetter), (BCommand command, kplus1tapeState, rightBoundingLetter))) : acc
                s : ss  | acc == [] -> 
                    generateFirstPhaseCommand command ss $ (SingleTapeCommand ((emptySymbol, s, rightBoundingLetter), (emptySymbol, s, rightBoundingLetter))) : acc 
                        | otherwise -> 
                    generateFirstPhaseCommand command ss $ (SingleTapeCommand ((leftBoundingLetter, s, rightBoundingLetter), (leftBoundingLetter, s, rightBoundingLetter))) : acc 
    
    let firstPhaseFinalCommand multiTapeStates startStates acc =
            case (multiTapeStates, startStates) of
                ([], []) -> 
                    reverse $ (SingleTapeCommand ((emptySymbol, kplus1tapeState, rightBoundingLetter), (emptySymbol, finalKPlusOneTapeState, rightBoundingLetter))) : acc
                (tape : tt, s : ss)  | acc == [] -> 
                    firstPhaseFinalCommand tt ss $ (SingleTapeCommand ((emptySymbol, s, rightBoundingLetter), (emptySymbol, firstPhaseFinalStatesTransmition tape, rightBoundingLetter))) : acc 
                        | otherwise -> 
                    firstPhaseFinalCommand tt ss $ (SingleTapeCommand ((leftBoundingLetter, s, rightBoundingLetter), (leftBoundingLetter, firstPhaseFinalStatesTransmition tape, rightBoundingLetter))) : acc 
    
    let firstPhaseStartCommand startStates acc =
            case startStates of
                [] -> 
                    reverse $ (SingleTapeCommand ((emptySymbol, startKPlusOneTapeState, rightBoundingLetter), (BCommand acceptCommand, kplus1tapeState, rightBoundingLetter))) : acc
                s : ss  | acc == [] -> 
                    firstPhaseStartCommand ss $ (SingleTapeCommand ((emptySymbol, s, rightBoundingLetter), (emptySymbol, s, rightBoundingLetter))) : acc 
                        | otherwise -> 
                    firstPhaseStartCommand ss $ (SingleTapeCommand ((leftBoundingLetter, s, rightBoundingLetter), (leftBoundingLetter, s, rightBoundingLetter))) : acc     

    let firstPhaseInternal commands acc = 
            case commands of
                [] -> acc
                h : t -> firstPhaseInternal t $ ((generateFirstPhaseCommand h startStates [])) : acc

    (firstPhaseStartCommand startStates []) : (firstPhaseFinalCommand multiTapeStates startStates []) : (firstPhaseInternal otherCommands [])

transformStartStatesInCommands startStates commands multiTapeStates = do
    let transformCommand states command multiTapeStates acc =
            case (states, command, multiTapeStates) of
                (h : t, (SingleTapeCommand ((l1, s1, r1), (l2, s2, r2))) : tcommands, tape : tt) 
                    | s1 == h && s2 == h -> 
                        transformCommand t tcommands tt $ (SingleTapeCommand ((l1, newState, r1), (l2, newState, r2))) : acc
                    | s1 == h -> 
                        transformCommand t tcommands tt $ (SingleTapeCommand ((l1, newState, r1), (l2, s2, r2))) : acc
                    | s2 == h -> 
                        transformCommand t tcommands tt $ (SingleTapeCommand ((l1, s1, r1), (l2, newState, r2))) : acc
                    | otherwise -> 
                        transformCommand t tcommands tt $ (SingleTapeCommand ((l1, s1, r1), (l2, s2, r2))) : acc
                            where newState = firstPhaseFinalStatesTransmition tape
                ([], [], []) -> reverse acc

    let transformStartStatesInCommandsInternal commands acc = 
            case commands of
                h : t -> transformStartStatesInCommandsInternal t $ (transformCommand startStates h multiTapeStates []) : acc
                [] -> reverse acc
            
    transformStartStatesInCommandsInternal commands []

generateEmptyStayCommands states acc =
    case states of
        [] -> reverse acc
        h : t -> generateEmptyStayCommands t $ (SingleTapeCommand ((leftBoundingLetter, h, rightBoundingLetter), (leftBoundingLetter, h, rightBoundingLetter))) : acc
        

secondPhase finalKPlusOneTapeState commands startStates accessStates multiTapeStates = do
    let transformedCommands = transformStartStatesInCommands startStates commands multiTapeStates
    let addKPlusOneTapeCommands c1 c2 acc = 
            case (c1, c2) of
                (h1 : t1, h2 : t2) -> 
                    addKPlusOneTapeCommands t1 t2 $ (h1 ++ [SingleTapeCommand ((BCommand h2, finalKPlusOneTapeState, emptySymbol), (emptySymbol, finalKPlusOneTapeState, BCommand h2))]) : acc
                ([], []) -> acc

    let returnToRightEndmarkerCommands commands acc = 
            case commands of
                h : t -> returnToRightEndmarkerCommands t $ 
                                ((generateEmptyStayCommands accessStates []) ++ 
                                [SingleTapeCommand ((emptySymbol, finalKPlusOneTapeState, BCommand h), (BCommand h, finalKPlusOneTapeState, emptySymbol))]
                                ) : acc
                [] -> acc

    (addKPlusOneTapeCommands transformedCommands commands []) ++ (returnToRightEndmarkerCommands commands [])

thirdPhase finalKPlusOneTapeState commands accessStates = do
    let thirdPhaseInternal commands acc =
            case commands of
                h : t -> thirdPhaseInternal t $ ((generateEmptyStayCommands accessStates []) ++ 
                                                [SingleTapeCommand ((BCommand h, finalKPlusOneTapeState,  rightBoundingLetter), (emptySymbol, finalKPlusOneTapeState, rightBoundingLetter))]
                                                ) : acc
                [] -> acc
    thirdPhaseInternal commands []

symCommands commands = do
    let reverseCommands commands acc =
            case commands of
                SingleTapeCommand ((a, s, b), (a1, s1, b1)) : t -> reverseCommands t (SingleTapeCommand ((a1, s1, b1), (a, s, b)) : acc)
                [] -> reverse acc

    let reverseAllCommands commands acc =
            case commands of
                h : t -> reverseAllCommands t ((reverseCommands h []) : acc)
                [] -> acc
    reverseAllCommands commands commands

mapTM2TMAfterThirdPhase :: TM -> TM
mapTM2TMAfterThirdPhase 
    (TM
        (inputAlphabet,
        tapeAlphabets, 
        MultiTapeStates multiTapeStates, 
        Commands commands, 
        StartStates startStates, 
        AccessStates accessStates)
    ) = do
        let kPlus1 = (+) 1 $ length multiTapeStates
        let startKPlusOneTapeState = State $ "q_0^" ++ show kPlus1
        let kplus1tapeState = State $ "q_1^" ++ show kPlus1
        let finalKPlusOneTapeState = State $ "q_2^" ++ show kPlus1
        let commandsList = Set.toList commands
        let (acceptCommand, otherCommands) = disjoinAcceptCommandWithOthers commandsList accessStates
        let commandsFirstPhase = firstPhase startKPlusOneTapeState kplus1tapeState finalKPlusOneTapeState acceptCommand otherCommands startStates multiTapeStates
        let commandsSecondPhase = secondPhase finalKPlusOneTapeState commandsList startStates accessStates multiTapeStates
        let commandsThirdPhase = thirdPhase finalKPlusOneTapeState commandsList accessStates

        let newTMCommands = Commands $ Set.fromList $ symCommands $ commandsFirstPhase ++ commandsSecondPhase ++ commandsThirdPhase

        let newTMTapeAlphabets = tapeAlphabets ++ [TapeAlphabet $ Set.map (\c -> BCommand c) commands]

        let newTMStartStates = StartStates (startStates ++ [startKPlusOneTapeState])

        let newTMMultiTapeStates = MultiTapeStates (
                (map (\set -> Set.insert (firstPhaseFinalStatesTransmition set) set) multiTapeStates) 
                ++ [Set.fromList [startKPlusOneTapeState, kplus1tapeState, finalKPlusOneTapeState]]
                )

        let newTMAccessStates = AccessStates (accessStates ++ [finalKPlusOneTapeState])

        TM (inputAlphabet, newTMTapeAlphabets, newTMMultiTapeStates, newTMCommands, newTMStartStates, newTMAccessStates)

doubleCommandsStateDisjoinFunction s = do
    let getNumber (State s) = n
            where (_, _, _, [n]) = s =~ "q_{?([0-9]+)}?\\^{?[0-9]+}?" :: (String, String, String, [String])
    let stateNumber = getNumber s
    let getTapeNumber (State s) = read n :: Float
            where (_, _, _, [n]) = s =~ "q_{?[0-9]+}?\\^{?([0-9]+)}?" :: (String, String, String, [String])
    let tapeNumber = (+) 0.5 $ getTapeNumber s
    State $ "q_{" ++ stateNumber ++ "}^{" ++ (show tapeNumber) ++ "}"    

doubleCommands startStates accessStates tapeAlphabets multiTapeStates commands = do
    let divideCommands commands acc =
            case commands of 
                SingleTapeCommand ((a, s, b), (a1, s1, b1)) : t 
                        | b == rightBoundingLetter -> divideCommands t (acc ++ [
                            SingleTapeCommand ((a, s, rightBoundingLetter), (a1, s1, rightBoundingLetter)),
                            SingleTapeCommand ((leftBoundingLetter, doubleCommandsStateDisjoinFunction s, rightBoundingLetter), (leftBoundingLetter, doubleCommandsStateDisjoinFunction s1, rightBoundingLetter))                                                                                                          
                                                                                ]) 
                        | otherwise -> divideCommands t (acc ++ [
                            SingleTapeCommand ((a, s, rightBoundingLetter), (a1, s1, rightBoundingLetter)),
                            SingleTapeCommand ((getDisjoinSquare2 b, doubleCommandsStateDisjoinFunction s, rightBoundingLetter), (getDisjoinSquare2 b1, doubleCommandsStateDisjoinFunction s1, rightBoundingLetter))
                                                                 ])
                [] -> acc

    let doubleCommandsInternal commands acc =
            case commands of 
                h : t -> doubleCommandsInternal t ((divideCommands h []) : acc)
                [] -> acc
    
    let doubleMultitapeStates states = [states, Set.map doubleCommandsStateDisjoinFunction states]
    let doubleStates state = [state, doubleCommandsStateDisjoinFunction state]
    let doubleTapeAlphabets tapeAlphabet = [tapeAlphabet, TapeAlphabet $ Set.map getDisjoinSquare2 a]
                where (TapeAlphabet a) = tapeAlphabet

    (concat $ map doubleStates startStates, 
        concat $ map doubleStates accessStates, 
        concat $ map doubleTapeAlphabets tapeAlphabets, 
        concat $ map doubleMultitapeStates multiTapeStates, 
        doubleCommandsInternal commands [])

intermediateStateOne2TwoKTransform = firstPhaseFinalStatesTransmition

one2TwoKCommands (multiTapeStates, commands) = do
    let getStartAndFinalStatesOfCommand command (starts, finals) = 
            case command of
                SingleTapeCommand ((l1, s1, r1), (l2, s2, r2)) : t -> getStartAndFinalStatesOfCommand t (s1 : starts, s2 : finals)
                [] -> (reverse starts, reverse finals)
    let oneActionCommand tapeStates (starts, finals) command n i (newTapeStates, newStarts, acc) =
            case (tapeStates, command, starts, finals) of
                ([], [], [], []) -> (reverse newTapeStates, reverse newStarts, reverse acc)
                (tape : tt, SingleTapeCommand ((l1, s1, r1), (l2, s2, r2)) : t, start : st, final : ft)   
                        | n == i || l1 == emptySymbol && l2 == emptySymbol -> 
                            oneActionCommand tt (st, ft) t n (i + 1) (tape : newTapeStates, final : newStarts, (SingleTapeCommand ((l1, start, r1), (l2, final, r2))) : acc)
                        | i < n && start == final -> 
                            oneActionCommand tt (st, ft) t n (i + 1) (tape : newTapeStates, final : newStarts, (SingleTapeCommand ((emptySymbol, start, r1), (emptySymbol, final, r2))) : acc)
                        | otherwise -> 
                            oneActionCommand tt (st, ft) t n (i + 1) ((Set.insert intermediateState tape) : newTapeStates, intermediateState : newStarts, (SingleTapeCommand ((emptySymbol, start, r1), (emptySymbol, intermediateState, r2))) : acc)
                                where intermediateState = intermediateStateOne2TwoKTransform tape
                _ -> error $ "Non-exhaustive patterns in case " ++ show (reverse newTapeStates, reverse newStarts, reverse acc)

    let one2TwoKCommand tapeStates (starts, finals) i command immutCommand acc =
            case command of
                SingleTapeCommand ((l1, s1, r1), (l2, s2, r2)) : t  | l1 == emptySymbol && l2 == emptySymbol -> 
                                                                            one2TwoKCommand tapeStates (starts, finals) (i + 1) t immutCommand acc
                                                                    | otherwise -> 
                                                                            one2TwoKCommand newTapeStates (newStarts, finals) (i + 1) t immutCommand $ newCommand : acc 
                                                                                where (newTapeStates, newStarts, newCommand) = oneActionCommand tapeStates (starts, finals) immutCommand i 0 ([], [], [])
                [] -> (tapeStates, acc)
    let one2TwoKCommandsInternal tapeStates commands acc =
            case commands of
                h : t -> one2TwoKCommandsInternal newTapeStates t $ newCommands ++ acc
                    where   (starts, finals) = getStartAndFinalStatesOfCommand h ([], []) 
                            (newTapeStates, newCommands) = one2TwoKCommand tapeStates (starts, finals) 0 h h []
                [] -> (tapeStates, acc)


    one2TwoKCommandsInternal multiTapeStates commands []

intermediateStateSingleInsertDeleteTransform = intermediateStateOne2TwoKTransform

transform2SingleInsertDeleteCommand (tapeStates, commands) = do 
    let transformCommand tapeStates command acc1 acc2 accTape =
            case (command, tapeStates) of
                (SingleTapeCommand ((l1, s1, r1), (l2, s2, r2)) : t, tape : tt)
                    | l1 /= emptySymbol && l1 /= leftBoundingLetter && l2 /= emptySymbol -> 
                        transformCommand tt t (SingleTapeCommand ((l1, s1, r1), (emptySymbol, intermediateState, r2)) : acc1) (SingleTapeCommand ((emptySymbol, intermediateState, r1), (l2, s2, r2)) : acc2) (Set.insert intermediateState tape : accTape)
                    | otherwise -> 
                        transformCommand tt t (SingleTapeCommand ((l1, s1, r1), (l2, s2, r2)) : acc1) (SingleTapeCommand ((l1, s1, r1), (l2, s2, r2)) : acc2) (tape : accTape)
                            where intermediateState = intermediateStateSingleInsertDeleteTransform tape
                ([], [])    | acc1 == acc2 -> (reverse accTape, [reverse acc1]) 
                            | otherwise -> (reverse accTape, [reverse acc1, reverse acc2])

    let transform2SingleInsertDeleteCommandInternal tapeStates commands acc =
            case commands of
                h : t -> transform2SingleInsertDeleteCommandInternal newTapeStates t $ newCommands ++ acc
                            where   (newTapeStates, newCommands) = transformCommand tapeStates h [] [] []
                [] -> (tapeStates, acc)
                    
    transform2SingleInsertDeleteCommandInternal tapeStates commands []

renameRightLeftBoundings commands = do
    let e i = Value ("E_{" ++ (show i) ++ "}")
    let f (State q) = Value ("F_{" ++ q ++ "}")
    let renameRightLeftBoundingsInternal i acc command =
            case command of
                SingleTapeCommand ((l1, s1, r1), (l2, s2, r2)) : t 
                    | l1 == leftBoundingLetter -> renameRightLeftBoundingsInternal (i + 1) ((PreSMCommand ((newRight, newS1), (newRight, newS2))) : acc) t
                    | otherwise -> renameRightLeftBoundingsInternal (i + 1) ((PreSMCommand ((l1, newS1), (l2, newS2))) : acc) t
                        where   newRight = e i
                                newS1 = f s1
                                newS2 = f s2
                [] -> reverse acc
    map (renameRightLeftBoundingsInternal 1 []) commands 

mapTM2TM' :: TM -> TM
mapTM2TM' tm = do
    let (TM (inputAlphabet,
            tapeAlphabets, 
            MultiTapeStates multiTapeStates, 
            Commands commandsSet, 
            StartStates startStates,
            AccessStates accessStates)
            ) = mapTM2TMAfterThirdPhase tm

    let commands = Set.toList commandsSet
    let (newStartStates, newAccessStates, newTapeAlphabets, doubledTapeStates, doubledCommands) = doubleCommands startStates accessStates tapeAlphabets multiTapeStates commands
    let (newTapeStates, newTMCommands) =    transform2SingleInsertDeleteCommand $ 
                                            one2TwoKCommands (doubledTapeStates, doubledCommands)
    TM (inputAlphabet, 
        newTapeAlphabets, 
        MultiTapeStates newTapeStates, 
        Commands (Set.fromList $ renameRightLeftBoundings newTMCommands), 
        StartStates newStartStates, 
        AccessStates newAccessStates)