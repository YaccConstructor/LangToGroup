{-# LANGUAGE OverloadedStrings #-}

module ConfigPrinter where

import Text.LaTeX.Base
import ConfigType
import Lib
import Helpers
import TMPrinter

instance ShowLaTeX Configs where
    doLaTeX (Configs configs) = do
        let tapesCount   = length $ head configs
        let tapeSpec     = [DVerticalLine, CenterColumn, VerticalLine, CenterColumn, VerticalLine, CenterColumn]
        let halfSpec     = concat $ replicate tapesCount tapeSpec
        let columnsSpec  = [CenterColumn] ++ halfSpec
        let tapesNames   = map (\num -> "Tape " ++ show num) [1..tapesCount]
        let halfHeader   = foldl1 (&) $ map (\cur -> (multicolumn 3 [CenterColumn] $ fromString cur)) tapesNames
        let showTripleConfig (u, q, v) = doLaTeX u & 
                                                (math $ doLaTeX q) & 
                                                doLaTeX v
        let showLine (lineNumber, config) = do
                foldl1 (&) ([fromString $ show lineNumber] ++ (map showTripleConfig config))
                tabularnewline
                hline

        let tableBody = do
                fromString "№" & halfHeader
                tabularnewline
                hline
                mapM_ showLine $ zip [1..] configs

        subsection_ "Configurations"
        tabular Nothing columnsSpec tableBody