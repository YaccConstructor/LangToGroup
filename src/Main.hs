
-- |This module represents interactive mode of working with program.
--
-- The user can run the project on a specified file with a grammar.

module Main where

import System.Console.ParseArgs
import Text.Megaparsec.Error (errorBundlePretty)

import GrammarReader as Reader
import TmsParser
import Boolean2TM
import SP2GP
import GPTypes
import TM2Tms (tm2tms)
import TMType (TM)
import TMTypes
import Set (size)

import System.IO
import Data.Map as Map

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

main :: IO ()
main = do
    args <- parseArgsIO
          (ArgsParseControl ArgsComplete ArgsSoftDash)
          argd
    case getArg args InputFlagString of
          Just input -> case getArg args OutputFlagString of
                          Just output -> grammar2Group input output
                          Nothing     -> error "OutputFlagString parsing error"
          Nothing    -> error "InputFlagString parsing error"

grammar2Group :: String -> String -> IO ()
grammar2Group grammarFile errorFile = do
    result <- parseFromFile Reader.parser errorFile grammarFile
    case result of
        Left err -> hPutStrLn stderr $ "Parsing error: " ++ errorBundlePretty err
        Right cs -> do {
                print $ checkGrammarType cs;
                print cs;
                let 
                    tm@(TM quadruples) = boolean2tm cs
                    (GP rels) = groupBeta tm 
                in do {  
                    putStrLn "Number of transitions in Turing Machine";   
                    print $ Map.size quadruples;
                    putStrLn "Number of relations in group";
                    print $ Set.size rels
                }
            }

tm2TMS :: TM -> IO ()
tm2TMS tm = do
    putStrLn "Converting turing machine to visualizer"
    case tm2tms tm of
        Left err -> hPutStrLn stderr $ "Error: " ++ show err
        Right tms -> putStrLn ("\n" ++ show tms)



mainTms :: String -> String -> IO ()
mainTms filename errorFile = do
    tmsParsingResult <- parseTms filename errorFile
    case tmsParsingResult of
        Left err -> hPrint stderr err
        Right tms -> print tms -- Do whatever you want with tms :: Tms.
