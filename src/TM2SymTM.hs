module TM2SymTM where

import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers
import Text.Regex.TDFA
import Data.List (partition)
          
firstPhase :: [State] -> [TapeCommand] -> [[TapeCommand]] -> [State] -> [Set State] -> [State] -> [[TapeCommand]]
firstPhase kplus1tapeStates acceptCommand otherCommands startStates multiTapeStates startFirstPhaseStates = do
    let [startKPlusOneTapeState, kplus1tapeState, finalKPlusOneTapeState] = kplus1tapeStates

    let generateFirstPhaseCommand command states acc =
            case states of
                [] -> 
                    reverse $ (SingleTapeCommand ((ES, kplus1tapeState, RBS), (BCommand command, kplus1tapeState, RBS))) : acc
                s : ss  | acc == [] -> gfpc ES
                        | otherwise -> gfpc LBS
                    where 
                        gfpc r = generateFirstPhaseCommand command ss $ (SingleTapeCommand ((r, s, RBS), (r, s, RBS))) : acc 
    
    let firstPhaseFinalCommand tapeStates states sStates acc =
            case (tapeStates, states, sStates) of
                ([], [], []) -> 
                    reverse $ (SingleTapeCommand ((ES, kplus1tapeState, RBS), (ES, finalKPlusOneTapeState, RBS))) : acc
                (_ : tt, s1 : ss1, s2 : ss2)    | acc == [] -> fpfc ES
                                                | otherwise -> fpfc LBS
                    where
                        fpfc r = firstPhaseFinalCommand tt ss1 ss2 $ (SingleTapeCommand ((r, s1, RBS), (r, s2, RBS))) : acc
                _ -> error "States don't match"
    
    let firstPhaseStartCommand states acc =
            case states of
                [] -> 
                    reverse $ (SingleTapeCommand ((ES, startKPlusOneTapeState, RBS), (BCommand acceptCommand, kplus1tapeState, RBS))) : acc
                s : ss  | acc == [] -> fpsc ES
                        | otherwise -> fpsc LBS
                    where 
                        fpsc r = firstPhaseStartCommand ss $ (SingleTapeCommand ((r, s, RBS), (r, s, RBS))) : acc 

    let firstPhaseInternal commands acc = 
            case commands of
                [] -> acc
                h : t -> firstPhaseInternal t $ ((generateFirstPhaseCommand h startFirstPhaseStates [])) : acc

    (firstPhaseStartCommand startFirstPhaseStates []) : (firstPhaseFinalCommand multiTapeStates startFirstPhaseStates startStates []) : (firstPhaseInternal otherCommands [])

generateEmptyStayCommands :: [State]
                                   -> [TapeCommand] -> [TapeCommand]
generateEmptyStayCommands states acc =
    case states of
        [] -> reverse acc
        h : t -> generateEmptyStayCommands t $ (SingleTapeCommand ((LBS, h, RBS), (LBS, h, RBS))) : acc
        
secondPhase :: State -> [[TapeCommand]] -> [State] -> [[TapeCommand]]
secondPhase finalKPlusOneTapeState allcommands accessStates = do
    let addKPlusOneTapeCommands cmd acc = 
            case cmd of
                h : t -> 
                    addKPlusOneTapeCommands t $ (h ++ [SingleTapeCommand ((BCommand h, finalKPlusOneTapeState, ES), (ES, finalKPlusOneTapeState, BCommand h))]) : acc
                [] -> acc

    let returnToRightEndmarkerCommands commands acc = 
            case commands of
                h : t -> returnToRightEndmarkerCommands t $ 
                                ((generateEmptyStayCommands accessStates []) ++ 
                                [SingleTapeCommand ((ES, finalKPlusOneTapeState, BCommand h), (BCommand h, finalKPlusOneTapeState, ES))]
                                ) : acc
                [] -> acc

    (addKPlusOneTapeCommands allcommands []) ++ (returnToRightEndmarkerCommands allcommands [])

thirdPhase :: State -> [[TapeCommand]] -> [State] -> [[TapeCommand]]
thirdPhase finalKPlusOneTapeState allcommands accessStates = do
    let thirdPhaseInternal commands acc =
            case commands of
                h : t -> thirdPhaseInternal t $ ((generateEmptyStayCommands accessStates []) ++ 
                                                [SingleTapeCommand ((BCommand h, finalKPlusOneTapeState,  RBS), (ES, finalKPlusOneTapeState, RBS))]
                                                ) : acc
                [] -> acc
    thirdPhaseInternal allcommands []

symCommands :: [[TapeCommand]] -> [[TapeCommand]] 
symCommands allcommands = do
    let reverseCommands commands acc =
            case commands of
                SingleTapeCommand ((a, s, b), (a1, s1, b1)) : t -> reverseCommands t (SingleTapeCommand ((a1, s1, b1), (a, s, b)) : acc)
                [] -> reverse acc
                _ -> error "Must be SingleTapeCommand"

    let reverseAllCommands commands acc =
            case commands of
                h : t -> reverseAllCommands t ((reverseCommands h []) : acc)
                [] -> acc
    reverseAllCommands allcommands allcommands

threePhaseProcessing :: TM -> TM
threePhaseProcessing 
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
        let acceptKPlusOneTapeState = State $ "q_3^" ++ show kPlus1
        let commandsList = Set.toList commands
        let ([acceptCommand], otherCommands) = if cmds1 == [] then error "No accept command" else (cmds1, cmds2)
                where
                    isAcceptCommand command = all (\((SingleTapeCommand (_, (_, sh, _))), ah) -> sh == ah) $ zip command accessStates
                    (cmds1, cmds2) = partition isAcceptCommand commandsList
        let startFirstPhaseStates = map (\tape -> genNextState tape) multiTapeStates
        let commandsFirstPhase = firstPhase [startKPlusOneTapeState, kplus1tapeState, finalKPlusOneTapeState] acceptCommand otherCommands startStates multiTapeStates startFirstPhaseStates
        let commandsSecondPhase = secondPhase finalKPlusOneTapeState commandsList accessStates
        let commandsThirdPhase = thirdPhase finalKPlusOneTapeState commandsList accessStates
        
        let tmAcceptCommand = 
                (map (\st -> SingleTapeCommand ((LBS, st, RBS), (LBS, st, RBS))) accessStates) ++
                [SingleTapeCommand ((LBS, finalKPlusOneTapeState, RBS), (LBS, acceptKPlusOneTapeState, RBS))]

        let newTMCommands = Commands $ Set.fromList $ commandsFirstPhase ++ commandsSecondPhase ++ commandsThirdPhase ++ [tmAcceptCommand]

        let newTMTapeAlphabets = tapeAlphabets ++ [TapeAlphabet $ Set.map (\c -> BCommand c) commands]

        let newTMStartStates = StartStates (startFirstPhaseStates ++ [startKPlusOneTapeState])

        let newTMMultiTapeStates = MultiTapeStates (
                (zipWith (\st set -> Set.insert st set) startFirstPhaseStates multiTapeStates)
                ++ [Set.fromList [startKPlusOneTapeState, kplus1tapeState, finalKPlusOneTapeState, acceptKPlusOneTapeState]]
                )

        let newTMAccessStates = AccessStates (accessStates ++ [acceptKPlusOneTapeState])

        TM (inputAlphabet, newTMTapeAlphabets, newTMMultiTapeStates, newTMCommands, newTMStartStates, newTMAccessStates)

doubleCommands :: [State] -> [State] -> [TapeAlphabet] -> [Set State] -> [[TapeCommand]] -> ([State], [State], [TapeAlphabet], [Set State], [[TapeCommand]])
doubleCommands startStates accessStates tapeAlphabets multiTapeStates allcommands = do

    let doubleCommandsStateDisjoinFunction st = State $ "q_{" ++ stateNumber ++ "}^{" ++ (show tapeNumber) ++ "}" 
            where
                getNumber (State s) = n
                        where (_, _, _, [n]) = s =~ "q_{?([0-9]+)}?\\^{?[0-9]+}?" :: (String, String, String, [String])
                stateNumber = getNumber st
                getTapeNumber (State s) = read n :: Float
                        where (_, _, _, [n]) = s =~ "q_{?[0-9]+}?\\^{?([0-9]+)}?" :: (String, String, String, [String])
                tapeNumber = (+) 0.5 $ getTapeNumber st  

    let divideCommands commands acc =
            case commands of 
                SingleTapeCommand ((a, s, b), (a1, s1, b1)) : t 
                        | b == RBS -> divideCommands t (acc ++ [
                            SingleTapeCommand ((a, s, RBS), (a1, s1, RBS)),
                            SingleTapeCommand ((LBS, doubleCommandsStateDisjoinFunction s, RBS), (LBS, doubleCommandsStateDisjoinFunction s1, RBS))                                                                                                          
                                                                                ]) 
                        | otherwise -> divideCommands t (acc ++ [
                            SingleTapeCommand ((a, s, RBS), (a1, s1, RBS)),
                            SingleTapeCommand ((getDisjoinSquare2 b, doubleCommandsStateDisjoinFunction s, RBS), (getDisjoinSquare2 b1, doubleCommandsStateDisjoinFunction s1, RBS))
                                                                 ])
                [] -> acc
                _ -> error "Must be SingleTapeCommand"

    let doubleCommandsInternal commands acc =
            case commands of 
                h : t -> doubleCommandsInternal t ((divideCommands h []) : acc)
                [] -> acc
    
    let doubleMultitapeStates states = [states, Set.map doubleCommandsStateDisjoinFunction states]
    let doubleStates st = [st, doubleCommandsStateDisjoinFunction st]
    let doubleTapeAlphabets tapeAlphabet = [tapeAlphabet, TapeAlphabet $ Set.map getDisjoinSquare2 a]
                where (TapeAlphabet a) = tapeAlphabet

    (concatMap doubleStates startStates, 
        concatMap doubleStates accessStates, 
        concatMap doubleTapeAlphabets tapeAlphabets, 
        concatMap doubleMultitapeStates multiTapeStates, 
        doubleCommandsInternal allcommands [])

one2KCmds :: ([Set State], [[TapeCommand]]) -> ([Set State], [[TapeCommand]])
one2KCmds (multiTapeStates, allcommands) = do
    let getStartAndFinalStatesOfCommand command (starts, finals) = 
            case command of
                SingleTapeCommand ((_, s1, _), (_, s2, _)) : t -> getStartAndFinalStatesOfCommand t (s1 : starts, s2 : finals)
                [] -> (reverse starts, reverse finals)
                _ -> error "Must be SingleTapeCommand"
    let oneActionCommand states (starts, finals) command n i (newTapeStates, newStarts, acc) =
            case (states, command, starts, finals) of
                ([], [], [], []) -> (reverse newTapeStates, reverse newStarts, reverse acc)
                (tape : tt, SingleTapeCommand ((l1, _, r1), (l2, _, r2)) : t, start : st, final : ft)   
                        | n == i || l1 == ES && l2 == ES -> 
                            oneActionCommand tt (st, ft) t n (i + 1) (tape : newTapeStates, final : newStarts, (SingleTapeCommand ((l1, start, r1), (l2, final, r2))) : acc)
                        | i < n && start == final -> 
                            oneActionCommand tt (st, ft) t n (i + 1) (tape : newTapeStates, final : newStarts, (SingleTapeCommand ((ES, start, r1), (ES, final, r2))) : acc)
                        | otherwise -> 
                            oneActionCommand tt (st, ft) t n (i + 1) ((Set.insert intermediateState tape) : newTapeStates, intermediateState : newStarts, (SingleTapeCommand ((ES, start, r1), (ES, intermediateState, r2))) : acc)
                                where intermediateState = genNextState tape
                _ -> error $ "Non-exhaustive patterns in case " ++ show (reverse newTapeStates, reverse newStarts, reverse acc)

    let one2KCmd states (starts, finals) i command immutCommand acc =
            case command of
                SingleTapeCommand ((l1, _, _), (l2, _, _)) : t  | l1 == ES && l2 == ES -> 
                                                                            one2KCmd states (starts, finals) (i + 1) t immutCommand acc
                                                                    | otherwise -> 
                                                                            one2KCmd newTapeStates (newStarts, finals) (i + 1) t immutCommand $ newCommand : acc 
                                                                                where (newTapeStates, newStarts, newCommand) = oneActionCommand states (starts, finals) immutCommand i 0 ([], [], [])
                [] -> (states, acc)
                _ -> error "Must be SingleTapeCommand"
    let one2KCmdsInternal states commands acc =
            case commands of
                h : t -> one2KCmdsInternal newTapeStates t $ newCommands ++ acc
                    where   (starts, finals) = getStartAndFinalStatesOfCommand h ([], []) 
                            (newTapeStates, newCommands) = one2KCmd states (starts, finals) 0 h h []
                [] -> (states, acc)


    one2KCmdsInternal multiTapeStates allcommands []

cmd2SIDCmd :: ([Set State], [[TapeCommand]]) -> ([Set State], [[TapeCommand]])
cmd2SIDCmd (tSts, allcommands) = do 
    let transformCommand states command = (newTapeStates, [c1, c2])
            where 
                func (SingleTapeCommand ((l1, s1, r1), (l2, s2, r2))) tape = (SingleTapeCommand ((l1, s1, r1), (ES, intermediateState, r1)), SingleTapeCommand ((ES, intermediateState, r1), (l2, s2, r2)), Set.insert intermediateState tape)
                    where 
                        intermediateState = genNextState tape
                func _ _ = error "Must be SingleTapeCommand"
                (c1, c2, newTapeStates) = unzip3 $ zipWith func command states
    
    let checkLeftBounding command = all (\(SingleTapeCommand ((l1, _, _), (l2, _, _))) -> l1 == LBS || l1 == ES || l2 == ES) command

    let cmd2SIDCmdInternal states commands acc =
            case commands of
                h : t   | checkLeftBounding h -> cmd2SIDCmdInternal states t $ h : acc
                        | otherwise -> cmd2SIDCmdInternal newTapeStates t $ newCommands ++ acc
                            where   (newTapeStates, newCommands) = transformCommand states h
                [] -> (states, acc)
                    
    cmd2SIDCmdInternal tSts allcommands []

symTM :: TM -> TM
symTM tm = do
    let (TM (inputAlphabet,
            tapeAlphabets, 
            MultiTapeStates multiTapeStates, 
            Commands commandsSet, 
            StartStates startStates,
            AccessStates accessStates)
            ) = threePhaseProcessing tm

    let commands = Set.toList commandsSet
    let (newStartStates, newAccessStates, newTapeAlphabets, doubledTapeStates, doubledCommands) = doubleCommands startStates accessStates tapeAlphabets multiTapeStates commands
    let (newTapeStates, newTMCommands) =    cmd2SIDCmd $ 
                                            one2KCmds (doubledTapeStates, doubledCommands)
    TM (inputAlphabet, 
        newTapeAlphabets, 
        MultiTapeStates newTapeStates, 
        Commands (Set.fromList $ symCommands newTMCommands),
        StartStates newStartStates, 
        AccessStates newAccessStates)

symDetTM :: TM -> TM
symDetTM (TM (inputAlphabet,
            tapeAlphabets, 
            MultiTapeStates multiTapeStates, 
            Commands commandsSet, 
            StartStates startStates,
            AccessStates accessStates)
            ) = do

    let commands = Set.toList commandsSet
    let (newStartStates, newAccessStates, newTapeAlphabets, doubledTapeStates, doubledCommands) = doubleCommands startStates accessStates tapeAlphabets multiTapeStates commands
    let (newTapeStates, newTMCommands) =    cmd2SIDCmd $ 
                                            one2KCmds (doubledTapeStates, doubledCommands)
    TM (inputAlphabet, 
        newTapeAlphabets, 
        MultiTapeStates newTapeStates,
        Commands (Set.fromList $ symCommands newTMCommands), 
        StartStates newStartStates, 
        AccessStates newAccessStates)