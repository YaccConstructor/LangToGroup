{-# LANGUAGE OverloadedStrings #-}

module ConfigPrinter where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Inputenc
import qualified Data.Set as Set

import TMType
import ConfigType
import Lib
import Tm1Printer

instance ShowLaTeX Configs where
    doLaTeX (Configs configs) = do
        let tapesCount   = length $ head configs
        let tapeSpec     = [CenterColumn, VerticalLine]
        let halfSpec     = concat $ replicate tapesCount tapeSpec
        let columnsSpec  = [DVerticalLine, CenterColumn, DVerticalLine] ++ halfSpec
        let tapesNames   = map (\num -> "Tape " ++ show num) [1..tapesCount]
        let halfHeader   = foldl1 (&) $ map (\cur -> fromString cur) tapesNames
        
        --let showTriple (u, q, v) = (math $ fromString $ concat u) ++ (math $ doLaTeX q) ++ (math $ fromString $ concat v)

        let showTriple (u, q, v) = (math $ doLaTeX q)

        let showLine (lineNumber, config) = do
                foldl1 (&) ([fromString $ show lineNumber] ++ (map showTriple config))
                tabularnewline
                hline

        let tableBody = do
                fromString "№" & halfHeader
                tabularnewline
                hline
                mapM_ showLine $ zip [1..] configs

        tabular Nothing columnsSpec tableBody