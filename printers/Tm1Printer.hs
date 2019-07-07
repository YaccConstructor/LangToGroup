{-# LANGUAGE OverloadedStrings #-}

module Tm1Printer where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Inputenc
import qualified Data.Set as Set

import Tm1Type
import Lib


instance ShowLaTeX State where
    doLaTeX (State st) = raw $ fromString st


showStates :: [State] -> LaTeXM ()
showStates = helper where
    helper [state]    = doLaTeX state
    helper (state:ss) = do { doLaTeX state; ","; showStates ss }


instance ShowLaTeX InputAlphabet where
    doLaTeX (InputAlphabet alphabet) = math $ fromString $ Set.toList alphabet


instance ShowLaTeX TapeAlphabet where
    doLaTeX (TapeAlphabet alphabet) = math $ fromString $ Set.toList alphabet


instance ShowLaTeX MultiTapeStates where
    doLaTeX (MultiTapeStates statesList) = do
        enumerate $ mapM_ (\states -> do { item Nothing; math $ showStates $ Set.toList $ states}) statesList


instance ShowLaTeX StartStates where
    doLaTeX (StartStates states) = math $ showStates states


instance ShowLaTeX AccessStates where
    doLaTeX (AccessStates states) = math $ showStates states


instance ShowLaTeX SingleTapeCommand where
    doLaTeX (SingleTapeCommand ((u, q, v), (u', q', v'))) = do
        let showTriple :: Char -> State -> Char -> LaTeX
            showTriple a s b = (fromString [a]) <> "," <> (toLaTeX s) <> "," <> (fromString [b]) 
        math $ textell $ ((showTriple u q v) <> rightarrow <> (showTriple u' q' v'))
    
    doLaTeX NoCommand = "nope"


instance ShowLaTeX Commands where
    doLaTeX (Commands commandsSet) = do
        let commands     = Set.toList commandsSet
        let tapesCount   = length $ head commands
        let columnsCount = tapesCount + 1 + tapesCount
        let tapeSpec     = [DVerticalLine, CenterColumn, VerticalLine, CenterColumn, VerticalLine, CenterColumn]
        let halfSpec     = tail $ concat $ replicate tapesCount tapeSpec
        let columnsSpec  = halfSpec ++ [DVerticalLine, CenterColumn, DVerticalLine] ++ halfSpec
        let tapesNames   = map (\num -> "Tape "++ show num) [1..tapesCount]
        let halfHeader   = foldl1 (&) $ map (\cur -> (multicolumn 3 [CenterColumn] $ fromString cur)) tapesNames
        
        let showTriple (u, q, v) = (math $ fromString [u]) & (math $ doLaTeX q) & (math $ fromString [v])
        
        let showFrom (SingleTapeCommand (q, _)) = showTriple q
            showFrom NoCommand = mempty & mempty & mempty

        let showTo (SingleTapeCommand (_, q)) = showTriple q
            showTo NoCommand = mempty & mempty & mempty

        let showLine (lineNumber, singleTapeCommands) = do
                foldl1 (&) ((map showFrom singleTapeCommands) ++ [fromString $ show lineNumber] ++ (map showTo singleTapeCommands))
                tabularnewline
                hline

        let tableBody = do
                halfHeader & "№" & halfHeader
                tabularnewline
                hline
                mapM_ showLine $ zip [1..] commands

        tabular Nothing columnsSpec tableBody


instance ShowLaTeX TM1 where
    doLaTeX (TM1 (
        inputAlphabet,
        tapeAlphabet,
        multiTapeStates,
        commands,
        startStates,
        accessStates)) = do
           subsection_ "Input alphabet"
           doLaTeX inputAlphabet
           subsection_ "Tape alphabet"
           doLaTeX tapeAlphabet
           subsection_ "States"
           doLaTeX multiTapeStates
           subsection_ "Start states"
           doLaTeX startStates
           subsection_ "Access states"
           doLaTeX accessStates
           subsection_ "Commands"
           doLaTeX commands
