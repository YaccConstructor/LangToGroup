module TM2TM' where

import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers

disjoinAcceptCommandWithOthers commands accessStates = do
    let isAcceptCommand command access = case (command, access) of 
            (((SingleTapeCommand ((_, _, _), (_, sh, _))), ah) : (st, at)) -> if sh == ah then isAcceptCommand st at else false
            [] : [] -> true
            _ -> false
    let disjoinAcceptCommandWithOthersInternal allCommands acc = case allCommands of
            (h : t) -> if isAcceptCommand h accessStates then (h, acc ++ t) else disjoinAcceptCommandWithOthersInternal t (h : acc)
            [] -> error "No accept command"

    disjoinAcceptCommandWithOthers commands []

mapTM2TM' :: TMType -> TMType
mapTM2TM' 
    (TM
        (inputAlphabet, -- нужно ли оно вообще?
        tapeAlphabets, 
        MultiTapeStates multiTapeStates, 
        Commands commands, 
        startStates, 
        AccessStates accessStates)
    ) = do
        
        -- let (acceptCommand, othersCommand) = disjoinAcceptCommandWithOthers commands accessStates
        -- let kplus1tapeState = State "kplus1state"
        -- let kplus1tapeAlphabetList = map (\i -> "t" ++ show i) [1 .. (Set.size commands)]
        -- let tm'CommandsList = 
        --         (zipWith 
        --             (\cmd letter -> cmd ++ [SingleTapeCommand ((letter, kplus1tapeState, emptySymbol), (emptySymbol, kplus1tapeState, letter))])
        --             (Set.toList commands)
        --             kplus1tapeAlphabetList)-- нам не нужна к + 1 лента
        
        

        let reverseCommands commands acc = -- fix []
            case commands of
                SingleTapeCommand ((a, s, b), (a1, s1, b1)) : t -> reverseCommands t (SingleTapeCommand ((a1, s1, b1), (a, s, b)) : acc)
                [] -> reverse acc

        let reverseAllCommands commands acc =
            case commands of
                h : t -> reverseAllCommands t ((reverseCommands h []) : acc)
                [] -> acc
        let symCommandsList = reverseAllCommands commands commands
        
        -- пора раздвоить команды

        -- надо бы избавиться от NoCommand, для этого (RuleLetter, State, *) -> (RuleLetter, State, *)

        let divideCommands commands acc =
            case commands of 
                SingleTapeCommand ((a, s, b), (a1, s1, b1)) : t -> if b = rightBoundingLetter then 
                                                                        divideCommands t (acc ++ [
                                                                                                    SingleTapeCommand ((a, s, rightBoundingLetter), (a1, s1, rightBoundingLetter)),
                                                                                                    SingleTapeCommand ((leftBoundingLetter, getDisjoinLetter s, rightBoundingLetter), (leftBoundingLetter, getDisjoinLetter s1, rightBoundingLetter))                                                                                                          
                                                                                                    ]) 
                                                                    else 
                                                                        divideCommands t (acc ++ [
                                                                                                    SingleTapeCommand ((a, s, rightBoundingLetter), (a1, s1, rightBoundingLetter)),
                                                                                                    SingleTapeCommand ((b, getDisjoinLetter s, rightBoundingLetter), (b1, getDisjoinLetter s1, rightBoundingLetter))
                                                                                                    ]) 
                [] -> acc

        let doubleTheCommands commands acc =
            case commands of 
                h : t -> doubleTheCommands t ((divideCommands h []) : acc)
                [] -> reverse acc

        let reversedSymmCommandsList = doubleTheCommands symCommandsList []

        -- самое время обновить алфавиты

        --TM(inputAlphabet, ...)        