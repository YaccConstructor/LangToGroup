-- |This module represents interactive mode of working with program.
--
-- The user can run the project on a specified file with a grammar.

module Main where

import Config

import System.Environment (getArgs)

main :: IO ()
main = do
    args    <- getArgs
    options <- getOptions args
    config  <- getConfig options
    case config of
        PrintHelp -> printHelp
        ApplyApproach appr conf -> appr conf

printHelp :: IO ()
printHelp = putStr $ unlines
    [ "Usage: LangToGroup-cli <options>\n"
    , optionsInfo
    , approachesInfo
    , objectsInfo
    , "For more information see https://github.com/YaccConstructor/LangToGroup/blob/master/README.md"
    ]
