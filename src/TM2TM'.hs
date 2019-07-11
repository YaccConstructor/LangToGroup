module TM2TM' where

import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers

disjoinAcceptCommandWithOthers commands accessStates = do
    let isAcceptCommand command access = 
            case (command, access) of 
                ((SingleTapeCommand ((_, _, _), (_, sh, _))) : st, ah : at) -> if sh == ah then isAcceptCommand st at else False
                ([], []) -> True
    let disjoinAcceptCommandWithOthersInternal allCommands acc = 
            case allCommands of
                (h : t) -> if isAcceptCommand h accessStates then (h, acc ++ t) else disjoinAcceptCommandWithOthersInternal t (h : acc)
                [] -> error "No accept command"

    disjoinAcceptCommandWithOthers commands []

-- -- надо *2к команды, дисджойня стейты и обновляя их алфавиты
-- generateSingleMoveCommands commandA commandA1 i oldstates newstates acc =
--     case (i, oldstates, newstates) of
--         (-1, oldstate : t1, newstate : t2) -> 
--             generateSingleMoveCommands commandA commandA1 i t1 t2 (SingleTapeCommand ((emptySymbol, oldstate, rightBoundingLetter), (emptySymbol, newstate, rightBoundingLetter)) : acc)
--         (-1, [], []) -> reverse acc
--         (0, oldstate : t1, newstate : t2) -> 
--             generateSingleMoveCommands commandA commandA1 (i - 1) t1 t2 (SingleTapeCommand ((commandA, oldstate, rightBoundingLetter), (commandA1, newstate, rightBoundingLetter)) : acc)
--         (_, oldstate : t1, newstate : t2) -> 
--             generateSingleMoveCommands commandA commandA1 (i - 1) t1 t2 (SingleTapeCommand ((emptySymbol, oldstate, rightBoundingLetter), (emptySymbol, newstate, rightBoundingLetter)) : acc)

-- getCurrentStates command acc = 
--     case command of
--         SingleTapeCommand ((_, s, _), (_, s1, _)) : t -> getCurrentStates t ((s, s1) : acc)
--         [] -> reverse acc

-- disjoinStates states = do
--     let disjoinStatesInternal states acc =
--             case states of
--                 State h : t ->  disjoinStatesInternal t ((State (getDisjoinLetter h)) : acc)
--                 [] -> reverse acc
--     disjoinStatesInternal states []

-- commandOnetify command = do
--     let (startStates, endStates) = getCurrentStates command []
-- -- как-то так, теперь надо алфавит стейтов засейвить
--     let commandOnetifyInternal oldStates newStates command i acc = 
--             case command of
--                 [SingleTapeCommand ((a, _, _), (a1, _, _))] -> 
--                     (generateSingleMoveCommands a a1 i oldStates endStates []) : acc
--                 SingleTapeCommand ((a, _, _), (a1, _, _)) : t -> 
--                     commandOnetifyInternal newStates (disjoinStates newStates) t (i + 1) ((generateSingleMoveCommands a a1 i oldStates newStates []) : acc)            
--     commandOnetifyInternal startStates (disjoinStates startStates) command -1 []

-- commandsOnetify commands acc =
--     case commands of
--         h : t -> 
--             commandsOnetify t ((commandOnetify h) ++ acc)
--         [] -> acc


startKPlusOneTapeState = State "q_0^{k+1}"
kplus1tapeState = State "q"
firstPhaseFinalStatesTransmition (State s) = State (s ++ "'")
finalKPlusOneTapeState = firstPhaseFinalStatesTransmition kplus1tapeState           

firstPhase acceptCommand otherCommands startStates = do

    let generateFirstPhaseCommand command states acc =
            case states of
                [] -> reverse $ (SingleTapeCommand ((emptySymbol, kplus1tapeState, rightBoundingLetter), (Command command, kplus1tapeState, rightBoundingLetter))) : acc
                s : ss  | acc == [] -> generateFirstPhaseCommand command ss $ (SingleTapeCommand ((emptySymbol, s, rightBoundingLetter), (emptySymbol, s, rightBoundingLetter))) : acc 
                        | otherwise -> generateFirstPhaseCommand command ss $ (SingleTapeCommand ((leftBoundingLetter, s, rightBoundingLetter), (leftBoundingLetter, s, rightBoundingLetter))) : acc 
    
    let firstPhaseFinalCommand startStates acc =
            case startStates of
                [] -> reverse $ (SingleTapeCommand ((emptySymbol, kplus1tapeState, rightBoundingLetter), (emptySymbol, finalKPlusOneTapeState, rightBoundingLetter))) : acc
                s : ss  | acc == [] -> firstPhaseFinalCommand ss $ (SingleTapeCommand ((emptySymbol, s, rightBoundingLetter), (emptySymbol, firstPhaseFinalStatesTransmition s, rightBoundingLetter))) : acc 
                        | otherwise -> firstPhaseFinalCommand ss $ (SingleTapeCommand ((leftBoundingLetter, s, rightBoundingLetter), (leftBoundingLetter, firstPhaseFinalStatesTransmition s, rightBoundingLetter))) : acc 
    
    let firstPhaseStartCommand startStates acc =
            case startStates of
                [] -> reverse $ (SingleTapeCommand ((emptySymbol, startKPlusOneTapeState, rightBoundingLetter), (Command acceptCommand, kplus1tapeState, rightBoundingLetter))) : acc
                s : ss  | acc == [] -> firstPhaseStartCommand ss $ (SingleTapeCommand ((emptySymbol, s, rightBoundingLetter), (emptySymbol, s, rightBoundingLetter))) : acc 
                        | otherwise -> firstPhaseStartCommand ss $ (SingleTapeCommand ((leftBoundingLetter, s, rightBoundingLetter), (leftBoundingLetter, s, rightBoundingLetter))) : acc     

    let firstPhaseInternal commands acc = 
            case commands of
                [] -> acc
                h : t -> firstPhaseInternal t $ ((generateFirstPhaseCommand h startStates [])) : acc

    (firstPhaseStartCommand startStates []) : (firstPhaseFinalCommand startStates []) : (firstPhaseInternal otherCommands [])

transformStartStatesInCommands startStates commands = do
    let transformCommand states command acc =
            case (states, command) of
                (h : t, (SingleTapeCommand ((l1, s1, r1), (l2, s2, r2))) : tcommands) 
                    | s1 == h && s2 == h -> transformCommand t tcommands $ (SingleTapeCommand ((l1, firstPhaseFinalStatesTransmition s1, r1), (l2, firstPhaseFinalStatesTransmition s2, r2))) : acc
                    | s1 == h -> transformCommand t tcommands $ (SingleTapeCommand ((l1, firstPhaseFinalStatesTransmition s1, r1), (l2, s2, r2))) : acc
                    | s2 == h -> transformCommand t tcommands $ (SingleTapeCommand ((l1, s1, r1), (l2, firstPhaseFinalStatesTransmition s2, r2))) : acc
                ([], []) -> reverse acc

    let transformStartStatesInCommandsInternal commands acc = 
            case commands of
                h : t -> transformStartStatesInCommandsInternal t $ (transformCommand startStates h []) : acc
                [] -> reverse acc
            
    transformStartStatesInCommandsInternal commands []

generateEmptyStayCommands states acc =
    case states of
        [] -> reverse acc
        h : t -> generateEmptyStayCommands t $ (SingleTapeCommand ((leftBoundingLetter, h, rightBoundingLetter), (leftBoundingLetter, h, rightBoundingLetter))) : acc
        

secondPhase commands startStates accessStates = do
    let transformedCommands = transformStartStatesInCommands startStates commands
    let addKPlusOneTapeCommands c1 c2 acc = 
            case (c1, c2) of
                (h1 : t1, h2 : t2) -> addKPlusOneTapeCommands t1 t2 $ (h1 ++ [SingleTapeCommand ((Command h2, finalKPlusOneTapeState, emptySymbol), (emptySymbol, finalKPlusOneTapeState, Command h2))]) : acc
                ([], []) -> acc

    let returnToRightEndmarkerCommands commands acc = 
            case commands of
                h : t -> returnToRightEndmarkerCommands t $ 
                                ((generateEmptyStayCommands accessStates []) ++ 
                                [SingleTapeCommand ((emptySymbol, finalKPlusOneTapeState, Command h), (Command h, finalKPlusOneTapeState, emptySymbol))]
                                ) : acc
                [] -> acc

    (addKPlusOneTapeCommands transformedCommands commands []) ++ (returnToRightEndmarkerCommands commands [])

thirdPhase commands accessStates = do
    let thirdPhaseInternal commands acc =
            case commands of
                h : t -> thirdPhaseInternal t $ ((generateEmptyStayCommands accessStates []) ++ 
                                                [SingleTapeCommand ((Command h, finalKPlusOneTapeState,  rightBoundingLetter), (emptySymbol, finalKPlusOneTapeState, rightBoundingLetter))]
                                                ) : acc
                [] -> acc
    thirdPhaseInternal commands []

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
        let commandsList = Set.toList commands
        let (acceptCommand, otherCommands) = disjoinAcceptCommandWithOthers commandsList accessStates
        let commandsFirstPhase = firstPhase acceptCommand otherCommands startStates
        let commandsSecondPhase = secondPhase commandsList startStates accessStates
        let commandsThirdPhase = thirdPhase commandsList accessStates

        let newTMCommands = Commands $ Set.fromList $ commandsFirstPhase ++ commandsSecondPhase ++ commandsThirdPhase

        let newTMTapeAlphabets = tapeAlphabets ++ [TapeAlphabet $ Set.map (\c -> Command c) commands]

        let newTMStartStates = StartStates (startStates ++ [startKPlusOneTapeState])

        let newTMMultiTapeStates = MultiTapeStates (
                (zipWith (\set start -> Set.insert (firstPhaseFinalStatesTransmition start) set) multiTapeStates startStates) 
                ++ [Set.fromList [startKPlusOneTapeState, kplus1tapeState, finalKPlusOneTapeState]]
                )

        let newTMAccessStates = AccessStates (accessStates ++ [finalKPlusOneTapeState])

        TM (inputAlphabet, newTMTapeAlphabets, newTMMultiTapeStates, newTMCommands, newTMStartStates, newTMAccessStates)

-- mapTM2TM' :: TM -> TM
-- mapTM2TM' 
--     (TM
--         (inputAlphabet,
--         tapeAlphabets, 
--         MultiTapeStates multiTapeStates, 
--         Commands commands, 
--         StartStates startStates, 
--         AccessStates accessStates)
--     ) = do
--         let commandsList = Set.toList commands
--         let (acceptCommand, otherCommands) = disjoinAcceptCommandWithOthers commandsList accessStates
--         let commandsFirstPhase = firstPhase acceptCommand otherCommands startStates
--         let transformedCommands = transformStartStatesInCommands startStates commandsList
--         let commandsSecondPhase = secondPhase transformedCommands startStates
        
        

--         let reverseCommands commands acc = -- fix []
--                 case commands of
--                     SingleTapeCommand ((a, s, b), (a1, s1, b1)) : t -> reverseCommands t (SingleTapeCommand ((a1, s1, b1), (a, s, b)) : acc)
--                     [] -> reverse acc

--         let reverseAllCommands commands acc =
--                 case commands of
--                     h : t -> reverseAllCommands t ((reverseCommands h []) : acc)
--                     [] -> acc
--         let symCommandsList = reverseAllCommands commands commands
        
--         -- пора раздвоить команды

--         let divideCommands commands acc =
--                 case commands of 
--                     SingleTapeCommand ((a, s, b), (a1, s1, b1)) : t -> if b == rightBoundingLetter then 
--                                                                             divideCommands t (acc ++ [
--                                                                                                         SingleTapeCommand ((a, s, rightBoundingLetter), (a1, s1, rightBoundingLetter)),
--                                                                                                         SingleTapeCommand ((leftBoundingLetter, getDisjoinLetter s, rightBoundingLetter), (leftBoundingLetter, getDisjoinLetter s1, rightBoundingLetter))                                                                                                          
--                                                                                                         ]) 
--                                                                         else 
--                                                                             divideCommands t (acc ++ [
--                                                                                                         SingleTapeCommand ((a, s, rightBoundingLetter), (a1, s1, rightBoundingLetter)),
--                                                                                                         SingleTapeCommand ((b, getDisjoinLetter s, rightBoundingLetter), (b1, getDisjoinLetter s1, rightBoundingLetter))
--                                                                                                         ]) 
--                     [] -> acc

--         let doubleTheCommands commands acc =
--                 case commands of 
--                     h : t -> doubleTheCommands t ((divideCommands h []) : acc)
--                     [] -> reverse acc

--         let reversedSymmCommandsList = doubleTheCommands symCommandsList []

--         -- самое время обновить алфавиты. а может это сделать в конце?

        
--         --TM(inputAlphabet, ...)        