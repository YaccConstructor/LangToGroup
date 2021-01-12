{-# LANGUAGE FlexibleInstances #-}

-- |This module provides functionality for presenting the Turing machine 'TM' in a text format appropriate to this service: https://turingmachinesimulator.com/
module TuringMachineWriter (toTMSimulator) where

import GHC.Unicode (isAlphaNum)
import Data.List.Utils (replace)
import Data.List (intercalate)

import TMType

class ShowTMSimulator a where
    toTMSimulator :: a -> String

filterStateName :: String -> String
filterStateName = replace "^" "v" . filter (\c -> isAlphaNum c || elem c ['_', '^'])

mergeMultipleTapeStates :: [State] -> String
mergeMultipleTapeStates = ("Q__" ++ ) . intercalate "__" . map filterStateName . enum
    where
        enum ss = fmap (\(num, (State s)) -> show num ++ "__" ++ s) (zip [0 ..] ss)

instance ShowTMSimulator [TapeCommand] where
    toTMSimulator cmds = "command placeholder"

instance ShowTMSimulator TM where
    toTMSimulator
        (TM
            (inputAlphabet,
            tapeAlphabets, 
            MultiTapeStates multiTapeStates, 
            Commands commands, 
            StartStates startStates, 
            AccessStates accessStates)
        ) =
        "name: TM\n" ++
        "init: " ++ mergeMultipleTapeStates startStates ++ "\n" ++
        "accept: " ++ mergeMultipleTapeStates accessStates ++ "\n\n" ++
        concatMap ((++ "\n\n") . toTMSimulator) commands
