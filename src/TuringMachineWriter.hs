-- |This module provides functionality for presenting the Turing machine 'TM' in a text format appropriate to this service: https://turingmachinesimulator.com/
-- Below this format will be called Tms.
module TuringMachineWriter (showAsTms) where

import GHC.Unicode (isAlphaNum)
import Data.List.Utils (replace)
import Data.List (intercalate)

import TMType

showAsTms :: TM -> String
showAsTms m = show $ Tms
    ("MyTM", [State "q01", State "q02"], [[State "q22", State "q15"]], [
            TmsCommand
                [
                    TmsSingleTapeCommand (Leave,                State "q0", State "q1", Stay),
                    TmsSingleTapeCommand (ChangeFromTo '0' '1', State "q2", State "q4", MoveLeft)
                ]], ["01", "01"])

-- |Type of Tms tape square action.
--
-- 'Leave' is leave any character unchanged.
--
-- 'ChangeFromTo f t' is change it from 'f' to 't'.
--
-- 'ChangeTo t' is change it from anything to 't'.
data TmsTapeSquare = Leave | ChangeFromTo Char Char | ChangeTo Char

-- |Type of Tms tape head movement
data TmsTapeHeadMovement = MoveLeft | Stay | MoveRight

instance Show TmsTapeHeadMovement where
    show MoveLeft  = "<"
    show Stay      = "-"
    show MoveRight = ">"

-- |Type of Tms command for one tape.
-- TmsSingleTapeCommand (action, prevState, nextState, movement).
newtype TmsSingleTapeCommand = TmsSingleTapeCommand (TmsTapeSquare, State, State, TmsTapeHeadMovement)

-- |Type of Tms command for entire Turing machine.
newtype TmsCommand = TmsCommand [TmsSingleTapeCommand]

-- |Type of Tms format.
-- Tms (name, initState, acceptStates, commands, tapeAlphabets).
newtype Tms = Tms (String, [State], [[State]], [TmsCommand], [[Char]])

instance Show Tms where
    show
        (Tms
            (name,
            init,
            acStates,
            commands,
            tapeAlphabets)
        ) = "name: " ++ name ++ "\n" ++
        "init: " ++ mergeMultipleTapeStates init ++ "\n" ++
        "accept: " ++ intercalate ", " (map mergeMultipleTapeStates acStates) ++ "\n\n" ++
        intercalate "\n\n" (map showTmsCommand commands)
        where
            showTmsCommand :: TmsCommand -> String
            showTmsCommand (TmsCommand tapeCommands) = intercalate "\n" $ map showSingleCmd $ combine $ map extCommand (zip tapeAlphabets tapeCommands)
            extCommand :: ([Char], TmsSingleTapeCommand) -> [((State, Char), (State, Char, TmsTapeHeadMovement))]
            extCommand (alph, (TmsSingleTapeCommand (Leave, iniSt, folSt, mv))) =
                [((iniSt, ch), (folSt, ch, mv)) | ch <- '_' : alph]
            extCommand (alph, (TmsSingleTapeCommand (ChangeFromTo cF cT, iniSt, folSt, mv))) =
                [((iniSt, cF), (folSt, cT, mv))]
            extCommand (alph, (TmsSingleTapeCommand (ChangeTo cT, iniSt, folSt, mv))) =
                [((iniSt, cF), (folSt, cT, mv)) | cF <- '_' : alph]
            combine = map reverse . foldl (\combs cur -> flip (:) <$> combs <*> cur) [[]]
            showSingleCmd :: [((State, Char), (State, Char, TmsTapeHeadMovement))] -> String
            showSingleCmd cmds = intercalate ", " (iniStateName : iniSquares) ++ "\n" ++
                                 intercalate ", " (folStateName : folSquares ++ moves) ++ "\n"
                where
                    iniStateName = mergeMultipleTapeStates $ map (\((ini, _), (_, _, _)) -> ini) cmds
                    folStateName = mergeMultipleTapeStates $ map (\((_, _), (fol, _, _)) -> fol) cmds
                    iniSquares = map (\((_, iniSq), (_, _, _)) -> return iniSq) cmds
                    folSquares = map (\((_, _), (_, folSq, _)) -> return folSq) cmds
                    moves = map (\((_, _), (_, _, mv)) -> show mv) cmds

-- Section of helper functions.

-- |Process string so that it does not contain illegal characters.
filterStateName :: String -> String
filterStateName = replace "^" "v" . filter (\c -> isAlphaNum c || elem c ['_', '^'])

-- |Concat and filter list of states.
mergeMultipleTapeStates :: [State] -> String
mergeMultipleTapeStates = ("Q__" ++ ) . intercalate "__" . map filterStateName . enum
    where
        enum ss = fmap (\(num, (State s)) -> show num ++ "__" ++ s) (zip [0 ..] ss)
