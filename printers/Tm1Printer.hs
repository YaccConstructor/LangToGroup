{-# LANGUAGE OverloadedStrings #-}

module Tm1Printer where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Inputenc
import qualified Data.Set as Set
import Text.LaTeX.Base.Types
import Data.Matrix
import Data.Maybe
import Debug.Trace


import TMType
import Lib


instance ShowLaTeX Square where
    doLaTeX (Value sq) = raw $ fromString sq
    doLaTeX (BCommand c) = showBCommand c
    doLaTeX (PCommand c) = showPCommand c

instance ShowLaTeX State where
    doLaTeX (State st) = raw $ fromString st


showPCommand :: [SingleTapeCommand] -> LaTeXM ()
showPCommand command = do
    let showTriple (u, q, v) = toLaTeX u <> toLaTeX q <> toLaTeX v
    let showFrom (SingleTapeCommand (q, _)) = showTriple q
    let showTo (SingleTapeCommand (_, q)) = showTriple q
    pmatrix (Just HRight) $ fromLists $ map (\c -> [showFrom c, to, showTo c]) command

showBCommand :: [SingleTapeCommand] -> LaTeXM ()
showBCommand command = do
    let showTriple (u, q, v) = toLaTeX u <> toLaTeX q <> toLaTeX v
    let showFrom (SingleTapeCommand (q, _)) = showTriple q
    let showTo (SingleTapeCommand (_, q)) = showTriple q
    bmatrix (Just HRight) $ fromLists $ map (\c -> [showFrom c, to, showTo c]) command

showStates :: [State] -> LaTeXM ()
showStates = helper where
    helper [state]    = math $ doLaTeX state
    helper (state:ss) = do { math $ doLaTeX state; ", "; showStates ss }

showAlphabet :: [Square] -> LaTeXM ()
showAlphabet = helper where
    helper [Value sq]    = math $ raw $ fromString sq
    helper (Value sq:ss) = do { math $ raw $ fromString sq; ","; showAlphabet ss }
    helper commands = mapM_ (\x -> do { math $ doLaTeX x ; "\n" }) commands


instance ShowLaTeX InputAlphabet where
    doLaTeX (InputAlphabet alphabet) = showAlphabet $ Set.toList alphabet


instance ShowLaTeX TapeAlphabet where
    doLaTeX (TapeAlphabet alphabet) = showAlphabet $ Set.toList alphabet

instance ShowLaTeX MultiTapeStates where
    doLaTeX (MultiTapeStates statesList) = do
        enumerate $ mapM_ (\states -> do { item Nothing; showStates $ Set.toList $ states}) statesList


instance ShowLaTeX StartStates where
    doLaTeX (StartStates states) = showStates states

instance ShowLaTeX AccessStates where
    doLaTeX (AccessStates states) = showStates states


instance ShowLaTeX SingleTapeCommand where
    doLaTeX (SingleTapeCommand ((u, q, v), (u', q', v'))) = do
        let showTriple :: Square -> State -> Square -> LaTeX
            showTriple a s b = (toLaTeX a) <> (toLaTeX s) <> (toLaTeX b) 
        math $ textell $ ((showTriple u q v) <> rightarrow <> (showTriple u' q' v'))


instance ShowLaTeX Commands where
    doLaTeX (Commands commandsSet) = mapM_ (\c -> do { math $ showBCommand c ; "\n" }) $ Set.toList commandsSet


instance ShowLaTeX TM where
    doLaTeX (TM (
        inputAlphabet,
        tapeAlphabets,
        multiTapeStates,
        commands,
        startStates,
        accessStates)) = do
           subsection_ "Input alphabet"
           doLaTeX inputAlphabet
           subsection_ "Tape alphabets"
           enumerate $ mapM_ (\alphabet -> do { item Nothing; doLaTeX $ alphabet}) tapeAlphabets
           subsection_ "States"
           doLaTeX multiTapeStates
           subsection_ "Start states"
           doLaTeX startStates
           subsection_ "Access states"
           doLaTeX accessStates
           subsection_ "Commands"
           doLaTeX commands
