module GPKnuthBendixMain where

import GPKnuthBendix
import System.Console.GetOpt
import System.Environment (getArgs)
import Control.Monad ((>=>))

data Options = Options {
    optTMIndices :: [Int],
    optTimePerTest :: Maybe Int,
    optPartOfOrds :: Maybe Double,
    optTotalTime :: Maybe Int
  }

defaultOptions :: Options
defaultOptions = Options {
    optTMIndices = [],
    optTimePerTest = Nothing,
    optPartOfOrds = Nothing,
    optTotalTime = Nothing
  }

addTMIndex :: Int -> Options -> Options
addTMIndex i opts = opts { optTMIndices = optTMIndices opts ++ [i] }

setTimePerTest :: Int -> Options -> Options
setTimePerTest t opts = opts { optTimePerTest = Just t }

setPartOfOrds :: Double -> Options -> Options
setPartOfOrds p opts = opts { optPartOfOrds = Just p }

setTotalTime :: Int -> Options -> Options
setTotalTime t opts = opts { optTotalTime = Just t }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['a'] ["addTM"]
        (ReqArg (addTMIndex . read) "TM_Index")
        "add index of Turing Machine for testing",
    Option [] ["timePerTest"]
        (ReqArg (setTimePerTest . read) "Seconds")
        "set time in seconds for every test",
    Option [] ["partOfOrds"]
        (ReqArg (setPartOfOrds . read) "Part")
        "set part (from 0.0 to 1.0) of all order functions which must be testing",
    Option [] ["totalTime"]
        (ReqArg (setTotalTime . read) "Seconds")
        "set total time for testing"
  ]

argsToOpts :: [String] -> Either String Options
argsToOpts argv =
    case getOpt Permute options argv of
        (o, [], [])  -> Right $ foldl (flip id) defaultOptions o
        (_, _, e)    -> Left  $ concat e ++ usageInfo "" options
        -- (_, n:ns, _) -> Left  $ "Non-option: " ++ n ++ usageInfo "" options

optsToTestInfo :: Options -> Either String TestInfo
optsToTestInfo opts =
    case opts of
        Options tmis Nothing (Just p) (Just tt) ->
            Right $ TestInfo tmis $ PartOfOrdsAndTotalTime p tt
        Options tmis (Just t) Nothing (Just tt) ->
            Right $ TestInfo tmis $ TimePerTestAndTotalTime t tt
        Options tmis (Just t) (Just p) Nothing ->
            Right $ TestInfo tmis $ TimePerTestAndPartOfOrds t p
        _ -> Left "Incorrect options"

argsToTestInfo :: [String] -> Either String TestInfo
argsToTestInfo = argsToOpts >=> optsToTestInfo

main :: IO ()
main = do
    args <- getArgs
    testInfo <- either fail return $
        argsToTestInfo args
    _ <- test testInfo
    return ()
