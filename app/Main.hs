{-# LANGUAGE MultiWayIf #-}

-- |This module represents interactive mode of working with program.
--
-- The user can run the project on a specified file with a grammar.

module Main where

import System.Console.ParseArgs
import Text.Megaparsec.Error (errorBundlePretty)

import GrammarType (Grammar)
import GRType (GR (GR))
import GrammarReader as Reader
import TmsParser
import Boolean2TM
import TM2Tms (tm2tms)
import TMType (TM)
import TuringMachine (TuringMachine)
import GroupPresentation (GroupPresentation)
import TM2SP
import SP2GP
import CFG2TM
import TM2SymTM
import TM2SM
import SM2GR
import GP2GAP
import GapFuncWriter
import ShowInfo
import Containers (fromList, toList)
import Data.Maybe (fromMaybe, isJust)

import System.IO
import Control.Exception (bracket)

type Approach = Bool -> Grammar -> Handle -> IO ()

data Option =
      InputFile
    | OutputFile
    | ErrorFile
    | FirstApproach
    | SecondApproach
    | SecondApproach_a
    | SecondApproach_b
    | Quiet
    | Help
    deriving (Ord, Eq, Show)

openOutputAnd :: FilePath -> (Handle -> IO a) -> IO a
openOutputAnd "" action = action stdout
openOutputAnd fp action = bracket (openFile fp WriteMode) hClose action

firstApproach :: Approach
firstApproach quiet grammar handle = do
    let (sm, accessWord, _) = tm2sm $ symTM $ cfg2tm grammar
        gr@(GR (a, _)) = sm2gr (sm, accessWord)
        hub = hubRelation accessWord
        genmap = fromList $ zip (toList a) $ map ((++) "f." . show) [1..]
    if quiet
    then
        hPutStr handle $ showInfo gr
    else
        writeGap gr handle genmap hub

secondApproachTemplate ::
    (TuringMachine -> IO a) ->
    (a -> IO GroupPresentation) ->
    Approach
secondApproachTemplate tm2a a2gp quiet grammar handle = do
    tm <- boolean2tm grammar
    a  <- tm2a tm
    gp <- a2gp a
    if quiet
    then
        hPutStr handle $ showInfo gp
    else do
        fmt <- gapFormat gp
        hPutStr handle fmt

secondApproach :: Approach
secondApproach = secondApproachTemplate semigroupGamma groupBeta

secondApproach_a :: Approach
secondApproach_a = secondApproachTemplate semigroupGamma_1 groupBeta_1

secondApproach_b :: Approach
secondApproach_b = secondApproachTemplate semigroupGamma_2 groupBeta_1

-- |List of options, which might be used for executing algorithm.
argd :: [ Arg Option ]
argd = [ Arg { argIndex = InputFile,
               argName = Just "input",
               argAbbr = Just 'i',
               argData = argDataOptional "file_path" ArgtypeString,
               argDesc = "Full path to file with grammar definition" },
         Arg { argIndex = OutputFile,
               argName = Just "output",
               argAbbr = Just 'o',
               argData = argDataOptional "file_path" ArgtypeString,
               argDesc = "Full path to file for printing results" },
         Arg { argIndex = ErrorFile,
               argName = Just "error",
               argAbbr = Just 'e',
               argData = argDataOptional "file_path" ArgtypeString,
               argDesc = "Full path to file, where errors should be recorded during parsing" },
         Arg { argIndex = FirstApproach,
               argName = Just "first-approach",
               argAbbr = Just 'f',
               argData = Nothing,
               argDesc = "Use first approach" },
         Arg { argIndex = SecondApproach,
               argName = Just "second-approach",
               argAbbr = Just 's',
               argData = Nothing,
               argDesc = "Use second approach" },
         Arg { argIndex = SecondApproach_a,
               argName = Just "second-approach-a",
               argAbbr = Just 'a',
               argData = Nothing,
               argDesc = "Use second approach, modification (a)" },
         Arg { argIndex = SecondApproach_b,
               argName = Just "second-approach-b",
               argAbbr = Just 'b',
               argData = Nothing,
               argDesc = "Use second approach, modification (b)" },
         Arg { argIndex = Quiet,
               argName = Just "quiet",
               argAbbr = Just 'q',
               argData = Nothing,
               argDesc = "Print only information about result" },
         Arg { argIndex = Help,
               argName = Just "help",
               argAbbr = Just 'h',
               argData = Nothing,
               argDesc = "Print help information and exit" }
    ]

printHelp :: IO ()
printHelp = do
    putStrLn $ unlines $
        ["Options:"] ++ [
            "    " ++ case ma of { Just a -> "-" ++ [a]; Nothing -> "  " } ++
            ", " ++ case mn of { Just n -> "--" ++ n; Nothing -> "" } ++
            (if isJust md then " <file_path>" else "") ++
            "\n        " ++ d
          | Arg _ ma mn md d <- argd
          ]

main :: IO ()
main = do
    args <- parseArgsIO
          (ArgsParseControl ArgsComplete ArgsSoftDash)
          argd
    if gotArg args Help
    then printHelp
    else do
        let inputFile  = fromMaybe "" $ getArg args InputFile
            outputFile = fromMaybe "" $ getArg args OutputFile
            errorFile  = fromMaybe "" $ getArg args ErrorFile
            approach   = getApproach args
            quiet      = gotArg args Quiet
        grammar <- parseFromFile Reader.parser errorFile inputFile
        openOutputAnd outputFile $
            approach quiet grammar

getApproach :: Args Option -> Approach
getApproach args =
    let fa   = gotArg args FirstApproach
        sa   = gotArg args SecondApproach
        sa_a = gotArg args SecondApproach_a
        sa_b = gotArg args SecondApproach_b
    in  if  | fa   -> firstApproach
            | sa   -> secondApproach
            | sa_a -> secondApproach_a
            | sa_b -> secondApproach_b
            | otherwise -> firstApproach

{-grammar2Group :: String -> String -> IO ()
grammar2Group grammarFile errorFile = do
    result <- parseFromFile Reader.parser errorFile grammarFile
    case result of
        Left err -> hPutStrLn stderr $ "Parsing error: " ++ errorBundlePretty err
        Right cs -> do
            print $ checkGrammarType cs
            print cs
            tm <- boolean2tm cs
            sp <- semigroupGamma tm
            gp <- groupBeta sp
            putStr $ showInfo (tm, gp)-}

{-tm2TMS :: TM -> IO ()
tm2TMS tm = do
    putStrLn "Converting turing machine to visualizer"
    case tm2tms tm of
        Left err -> hPutStrLn stderr $ "Error: " ++ show err
        Right tms -> putStrLn ("\n" ++ show tms)-}

{-mainTms :: String -> String -> IO ()
mainTms filename errorFile = do
    tmsParsingResult <- parseTms filename errorFile
    case tmsParsingResult of
        Left err -> hPrint stderr err
        Right tms -> print tms -- Do whatever you want with tms :: Tms. -}
