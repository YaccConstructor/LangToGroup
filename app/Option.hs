module Option (
    Option (..),
    getOptions,
    optionsInfo,
    module Object,
    module Approach,
  ) where

import Object
import Approach

import System.Console.GetOpt

data Option =
      InputFile FilePath
    | OutputFile FilePath
    | ErrorFile FilePath
    | UsedApproach Approach
    | InfoAbout (Set Object)
    | Help

options :: MonadFail m => [OptDescr (m Option)]
options =
    [ Option "i" ["input"]    (ReqArg (return . InputFile)  "file_path")
        "Full path to file with grammar definition"
    , Option "o" ["output"]   (ReqArg (return . OutputFile) "file_path")
        "Full path to file for printing results"
    , Option "e" ["error"]    (ReqArg (return . ErrorFile)  "file_path")
        "Full path to file, where errors should be recorded during parsing"
    , Option "a" ["approach"] (ReqArg (fmap UsedApproach . getApproach) "approach")
        "Used approach (see section `Approaches`)"
    , Option "I" ["info"]     (ReqArg (fmap InfoAbout . getObjects) "objects")
        "Print useful information about objects (see section `Objects`)"
    , Option "h" ["help"]     (NoArg  (return Help))
        "Print help and exit"
    ]

getOptions :: MonadFail m => [String] -> m [Option]
getOptions args =
    case getOpt RequireOrder options args of
        (os, _, []) -> sequence os
        (_, _, err) -> fail $ unlines err

optionsInfo :: String
optionsInfo = usageInfo "Options:" (options :: [OptDescr (IO Option)])
