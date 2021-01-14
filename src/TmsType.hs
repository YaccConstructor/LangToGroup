-- |This module provides types for representing turing machines in a format suitable for the service: https://turingmachinesimulator.com/
-- Below this format will be called Tms.

module TmsType where

import Data.List (intercalate)
import Data.List.Utils (replace)
import GHC.Unicode (isAlphaNum)

import TMType

-- |Type of Tms tape square action.
--
-- 'Leave' is leave any character unchanged.
--
-- 'ChangeFromTo f t' is change it from 'f' to 't'.
--
-- 'ChangeTo t' is change it from anything to 't'.
data TmsTapeSquare = Leave | ChangeFromTo Char Char | ChangeTo Char
    deriving (Eq)

-- |Type of Tms tape head movement
data TmsTapeHeadMovement = MoveLeft | Stay | MoveRight
    deriving (Eq)

instance Show TmsTapeHeadMovement where
    show MoveLeft  = "<"
    show Stay      = "-"
    show MoveRight = ">"

-- |Type of Tms command for one tape.
-- TmsSingleTapeCommand (action, prevState, nextState, movement).
newtype TmsSingleTapeCommand = TmsSingleTapeCommand (TmsTapeSquare, State, State, TmsTapeHeadMovement)
    deriving (Eq)

-- |Type of Tms command for entire Turing machine.
newtype TmsCommand = TmsCommand [TmsSingleTapeCommand]
    deriving (Eq)

-- |Type of Tms format.
-- Tms            (name,   init     accept     commands,     tapeAlphabets).
newtype Tms = Tms (String, [State], [[State]], [TmsCommand], [[Char]])
    deriving (Eq)

instance Show Tms where
    show
        (Tms
            (name,
            initial,
            acStates,
            commands,
            tapeAlphabets)
        ) = "name: " ++ name ++ "\n" ++
        "init: " ++ mergeMultipleTapeStates initial ++ "\n" ++
        "accept: " ++ intercalate ", " (map mergeMultipleTapeStates acStates) ++ "\n\n" ++
        intercalate "\n\n" (map showTmsCommand commands)
        where
            showTmsCommand :: TmsCommand -> String
            showTmsCommand (TmsCommand tapeCommands) = intercalate "\n" $ map showSingleCmd $ combine $ map extCommand (zip tapeAlphabets tapeCommands)
            extCommand :: ([Char], TmsSingleTapeCommand) -> [((State, Char), (State, Char, TmsTapeHeadMovement))]
            extCommand (alph, (TmsSingleTapeCommand (Leave, iniSt, folSt, mv))) =
                [((iniSt, ch), (folSt, ch, mv)) | ch <- '_' : alph]
            extCommand (_,    (TmsSingleTapeCommand (ChangeFromTo cF cT, iniSt, folSt, mv))) =
                [((iniSt, cF), (folSt, cT, mv))]
            extCommand (alph, (TmsSingleTapeCommand (ChangeTo cT, iniSt, folSt, mv))) =
                [((iniSt, cF), (folSt, cT, mv)) | cF <- '_' : alph]
            combine = map Prelude.reverse . foldl (\combs cur -> flip (:) <$> combs <*> cur) [[]]
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
