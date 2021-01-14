-- |This module provides types for representing turing machines in a format suitable for the service: https://turingmachinesimulator.com/
-- Below this format will be called Tms.

module TmsType where

import Data.List (intercalate)
import Data.List.Utils (replace)
import GHC.Unicode (isAlphaNum)

import qualified Data.Text.Lazy as TL
import Prettyprinter

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

-- |Type of Tms State.
newtype TmsState = TmsState String
    deriving (Eq)

-- |Type of Tms command for one tape.
-- TmsSingleTapeCommand (action, prevState, nextState, movement).
newtype TmsSingleTapeCommand = TmsSingleTapeCommand (TmsTapeSquare, TmsState, TmsState, TmsTapeHeadMovement)
    deriving (Eq)

-- |Type of Tms command for entire Turing machine.
newtype TmsCommand = TmsCommand [TmsSingleTapeCommand]
    deriving (Eq)

-- |Type of Tms format.
-- Tms            (name,   init        accept        commands,     tapeAlphabets).
newtype Tms = Tms (String, [TmsState], [[TmsState]], [TmsCommand], [[Char]])
    deriving (Eq)

instance Show Tms where
  show
    ( Tms
        ( name,
          initial,
          acStates,
          commands,
          tapeAlphabets
          )
      ) = show $
      (printKeyValue
        [ ["name", name],
          ["init", mergeMultipleTapeStates initial],
          ["accept", intercalate ", " (map mergeMultipleTapeStates acStates)]
        ])
        <> line
        <> vcat (punctuate line (map (pretty . showTmsCommand) commands))
        where
            printKeyValue :: [[String]] -> Doc String
            printKeyValue = vcat . fmap sep . fmap (punctuate colon) . fmap (fmap pretty)
            showTmsCommand :: TmsCommand -> String
            showTmsCommand (TmsCommand tapeCommands) = intercalate "\n" $ map showSingleCmd $ combine $ map extCommand (zip tapeAlphabets tapeCommands)
            extCommand :: ([Char], TmsSingleTapeCommand) -> [((TmsState, Char), (TmsState, Char, TmsTapeHeadMovement))]
            extCommand (alph, (TmsSingleTapeCommand (Leave, iniSt, folSt, mv))) =
                [((iniSt, ch), (folSt, ch, mv)) | ch <- '_' : alph]
            extCommand (_,    (TmsSingleTapeCommand (ChangeFromTo cF cT, iniSt, folSt, mv))) =
                [((iniSt, cF), (folSt, cT, mv))]
            extCommand (alph, (TmsSingleTapeCommand (ChangeTo cT, iniSt, folSt, mv))) =
                [((iniSt, cF), (folSt, cT, mv)) | cF <- '_' : alph]
            combine = map reverse . foldl (\combs cur -> flip (:) <$> combs <*> cur) [[]]
            showSingleCmd :: [((TmsState, Char), (TmsState, Char, TmsTapeHeadMovement))] -> String
            showSingleCmd cmds = show $
                                    sep (punctuate comma (iniStateName : iniSquares)) <> line <>
                                    sep (punctuate comma (folStateName : folSquares ++ moves))
                where
                    iniStateName = pretty $ mergeMultipleTapeStates $ map (\((ini, _), (_, _, _)) -> ini) cmds
                    folStateName = pretty $ mergeMultipleTapeStates $ map (\((_, _), (fol, _, _)) -> fol) cmds
                    iniSquares = map (\((_, iniSq), (_, _, _)) -> pretty [iniSq]) cmds
                    folSquares = map (\((_, _), (_, folSq, _)) -> pretty [folSq]) cmds
                    moves = map (\((_, _), (_, _, mv)) -> pretty $ show mv) cmds

-- Section of helper functions.

-- |Process string so that it does not contain illegal characters.
filterStateName :: String -> String
filterStateName = replace "^" "v" . filter (\c -> isAlphaNum c || elem c ['_', '^'])

-- |Concat and filter list of states.
mergeMultipleTapeStates :: [TmsState] -> String
mergeMultipleTapeStates = ("Q__" ++ ) . intercalate "__" . map filterStateName . enum
    where
        enum ss = fmap (\(num, (TmsState s)) -> show num ++ "__" ++ s) (zip [0 ..] ss)
