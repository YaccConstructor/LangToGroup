{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module TMPrinter where

import Text.LaTeX.Base
import Text.LaTeX.Packages.AMSMath
import qualified Data.Set as Set
import Data.Matrix
import TMType
import Lib


instance ShowLaTeX Square where
    doLaTeX (Value sq iq) = raw $ fromString (sq ++ replicate iq '\'')
    doLaTeX RBS = omega
    doLaTeX LBS = alpha
    doLaTeX ES = raw $ fromString ""
    doLaTeX (E i) = raw $ fromString $ "E_{" ++ (show i) ++ "}"
    doLaTeX (BCommand c) = showBCommand c
    doLaTeX (PCommand c) = showPCommand c

instance ShowLaTeX State where
    doLaTeX (State st) = raw $ fromString st

instance ShowLaTeX StateOmega where
    doLaTeX s = raw $ fromString $ show s

showTriple :: (ShowLaTeX a1, ShowLaTeX a2, ShowLaTeX a3) =>
                    (a1, a2, a3) -> LaTeX
showTriple (u, q, v) = toLaTeX u <> toLaTeX q <> toLaTeX v

showPair :: (ShowLaTeX a1, ShowLaTeX a2) => (a1, a2) -> LaTeX
showPair (r, l) = toLaTeX r <> toLaTeX l

showFrom :: TapeCommand -> LaTeX
showFrom (SingleTapeCommand (q, _)) = showTriple q
showFrom (PreSMCommand (q, _)) = showPair q

showTo :: TapeCommand -> LaTeX
showTo (SingleTapeCommand (_, q)) = showTriple q
showTo (PreSMCommand (_, q)) = showPair q

showPCommand :: [TapeCommand] -> LaTeXM ()
showPCommand command =
    pmatrix (Just HRight) $ fromLists $ map (\c -> [showFrom c, to, showTo c]) command

showBCommand :: [TapeCommand] -> LaTeXM ()
showBCommand command =
    bmatrix (Just HRight) $ fromLists $ map (\c -> [showFrom c, to, showTo c]) command

instance ShowLaTeX [Square] where
    doLaTeX = math . foldl1 (\x y -> x <> " " <> y) . map doLaTeX

instance ShowLaTeX [State] where
    doLaTeX = foldl1 (\x y -> x <> ", " <> y) . map (math . doLaTeX $)

showAlphabet :: [Square] -> LaTeXM ()
showAlphabet alphabet = 
    case alphabet of 
        [] -> ""
        lst -> math . foldl1 (\x y -> x <> ", " <> y) . map doLaTeX $ lst


instance ShowLaTeX InputAlphabet where
    doLaTeX (InputAlphabet alphabet) = showAlphabet $ Set.toList alphabet


instance ShowLaTeX TapeAlphabet where
    doLaTeX (TapeAlphabet alphabet) = showAlphabet $ Set.toList alphabet

instance ShowLaTeX MultiTapeStates where
    doLaTeX (MultiTapeStates statesList) =
        enumerate $ mapM_ (\states -> do { item Nothing; doLaTeX $ Set.toList $ states}) statesList


instance ShowLaTeX StartStates where
    doLaTeX (StartStates states) = doLaTeX states

instance ShowLaTeX AccessStates where
    doLaTeX (AccessStates states) = doLaTeX states

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
