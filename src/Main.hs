
-- |This module represents interactive mode of working with program.
--
-- The user can run the project on a specified file with a grammar.

module Main where

import System.Console.ParseArgs

import GrammarReader
import TmsParser
import System.IO

data Options =
    InputFlagString | OutputFlagString
    deriving (Ord, Eq, Show)

-- |List of options, which might be used for executing algorithm.
argd :: [ Arg Options ]
argd = [ Arg { argIndex = InputFlagString,
               argName = Just "input",
               argAbbr = Just 'i',
               argData = argDataRequired "input-file-full-path" ArgtypeString,
               argDesc = "Full path to file with grammar definition" },
         Arg { argIndex = OutputFlagString,
               argName = Just "output",
               argAbbr = Just 'o',
               argData = argDataRequired "errors-file-full-path" ArgtypeString,
               argDesc = "Full path to file, where errors should be recorded during parsing" }]

{---main :: IO ()
main = do
    args <- parseArgsIO
          (ArgsParseControl ArgsComplete ArgsSoftDash)
          argd
    case getArg args InputFlagString of
          Just input -> case getArg args OutputFlagString of
                          Just output -> convertGrammar2TM input output
                          Nothing     -> error "OutputFlagString parsing error"
          Nothing    -> error "InputFlagString parsing error" --}

mainTms :: String -> String -> IO ()
mainTms filename errorFile = do
    tmsParsingResult <- parseTms filename errorFile
    case tmsParsingResult of
        Left err -> hPrint stderr err
        Right tms -> print tms -- Do whatever you want with tms :: Tms.
