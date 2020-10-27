module Executor where

import System.Console.ParseArgs

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
    putStrLn "parse successful"
    case (getArg args InputFlagString) of
        Just s -> putStrLn ("saw input string " ++ s)
        Nothing -> return ()
    case (getArg args OutputFlagString) of
            Just s -> putStrLn ("saw output string " ++ s)
            Nothing -> return ()