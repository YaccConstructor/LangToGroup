module Main where

import System.Console.ParseArgs

import GrammarReader

data Options =
    InputFlagString | OutputFlagString
    deriving (Ord, Eq, Show)

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

main :: IO ()
main = do
    args <- parseArgsIO
          (ArgsParseControl (ArgsComplete) ArgsSoftDash)
          argd
    case (getArg args InputFlagString) of
          Just input -> case (getArg args OutputFlagString) of
                          Just output -> convertGrammar2TM input output