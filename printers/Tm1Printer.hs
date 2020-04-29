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

instance ShowLaTeX StateOmega where
    doLaTeX s = raw $ fromString $ show s

showTriple (u, q, v) = toLaTeX u <> toLaTeX q <> toLaTeX v
showPair (r, l) = toLaTeX r <> toLaTeX l
showFrom (SingleTapeCommand (q, _)) = showTriple q
showFrom (PreSMCommand (q, _)) = showPair q
showTo (SingleTapeCommand (_, q)) = showTriple q
showTo (PreSMCommand (_, q)) = showPair q

showPCommand :: [TapeCommand] -> LaTeXM ()
showPCommand command =
    pmatrix (Just HRight) $ fromLists $ map (\c -> [showFrom c, to, showTo c]) command

showBCommand :: [TapeCommand] -> LaTeXM ()
showBCommand command =
    bmatrix (Just HRight) $ fromLists $ map (\c -> [showFrom c, to, showTo c]) command

showStates :: [State] -> LaTeXM ()
showStates = foldl1 (\x y -> x <> ", " <> y) . map (math . doLaTeX $)

showAlphabet :: [Square] -> LaTeXM ()
showAlphabet alphabet = 
    case map (\x -> math $ case x of Value sq -> raw $ fromString sq ; _ -> doLaTeX x) alphabet of 
        [] -> ""
        lst -> foldl1 (\x y -> x <> ", " <> y) lst


instance ShowLaTeX InputAlphabet where
    doLaTeX (InputAlphabet alphabet) = showAlphabet $ Set.toList alphabet


instance ShowLaTeX TapeAlphabet where
    doLaTeX (TapeAlphabet alphabet) = showAlphabet $ Set.toList alphabet

instance ShowLaTeX MultiTapeStates where
    doLaTeX (MultiTapeStates statesList) =
        enumerate $ mapM_ (\states -> do { item Nothing; showStates $ Set.toList $ states}) statesList


instance ShowLaTeX StartStates where
    doLaTeX (StartStates states) = showStates states

instance ShowLaTeX AccessStates where
    doLaTeX (AccessStates states) = showStates states

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
