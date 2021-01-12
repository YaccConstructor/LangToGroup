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
                    TmsSingleTapeCommand ((State "q0", Any), (State "q1", Label '0', MoveLeft)),
                    TmsSingleTapeCommand ((State "q0", Any), (State "q1", Label '0', MoveLeft))
                ]], ["01", "01"])

-- |Type of Tms tape cell.
--
-- 'Any' is any type of cell.
--
-- 'Label' is labeled type of cell.
data TmsTapeSquare = Any | Label Char

tmssq2chars :: [Char] -> TmsTapeSquare -> [Char]
tmssq2chars alphabet Any       = alphabet
tmssq2chars alphabet (Label c) = pure c

-- |Type of Tms tape head movement
data TmsTapeHeadMovement = MoveLeft | Stay | MoveRight

instance Show TmsTapeHeadMovement where
    show MoveLeft  = "<"
    show Stay      = "-"
    show MoveRight = ">"

-- |Type of Tms command for one tape.
newtype TmsSingleTapeCommand = TmsSingleTapeCommand ((State, TmsTapeSquare), (State, TmsTapeSquare, TmsTapeHeadMovement))

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
            extCommand (alph, (TmsSingleTapeCommand ((iniSt, iniSq), (folSt, folSq, mv)))) =
                [((iniSt, iniChar), (folSt, folChar, mv)) | iniChar <- tmssq2chars alph iniSq,
                                                            folChar <- tmssq2chars alph folSq]
            combine = foldl (\combs cur -> flip (:) <$> combs <*> cur) [[]]
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

filterStateName :: String -> String
filterStateName = replace "^" "v" . filter (\c -> isAlphaNum c || elem c ['_', '^'])

mergeMultipleTapeStates :: [State] -> String
mergeMultipleTapeStates = ("Q__" ++ ) . intercalate "__" . map filterStateName . enum
    where
        enum ss = fmap (\(num, (State s)) -> show num ++ "__" ++ s) (zip [0 ..] ss)
