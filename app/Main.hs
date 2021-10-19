{-# LANGUAGE TemplateHaskell, MultiWayIf #-}

-- |This module represents interactive mode of working with program.
--
-- The user can run the project on a specified file with a grammar.

module Main where

import Control.Exception (bracket)

import GRType (GR (GR))
import GrammarReader as Reader
import Boolean2TM
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
import Lens
import Data.List.Split (splitWhen)
import Control.Monad (when)

import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO

data Approach =
      First
    | Second
    | SecondA
    | SecondB
    deriving (Eq)

data Object =
      ObjectGrammar
    | ObjectTuringMachine
    | ObjectGroupPresentation

data Flag =
      InputFile FilePath
    | OutputFile FilePath
    | ErrorFile FilePath
    | UsedApproach (Either String Approach)
    | GPinLaTeX
    | Info (Either String [Object])
    | Help

options :: [OptDescr Flag]
options =
    [ Option "i" ["input"]    (ReqArg InputFile  "file_path")
        "Full path to file with grammar definition"
    , Option "o" ["output"]   (ReqArg OutputFile "file_path")
        "Full path to file for printing results"
    , Option "e" ["error"]    (ReqArg ErrorFile  "file_path")
        "Full path to file, where errors should be recorded during parsing"
    , Option "a" ["approach"] (ReqArg (UsedApproach . toApproach) "approach")
        "Used approach, can be 'first', 'second', 'second_a' or 'second_b'"
    , Option "L" ["LaTeX"]    (NoArg  GPinLaTeX)
        "Print result in LaTeX format (while doesn't work)"
    , Option "I" ["info"]     (ReqArg (Info . toObjects) "objects")
        "Print useful information about objects. Objects must be separeted by\
        \ comma. Every object must be 'grammar', 'turing_machine' or\
        \ 'group_presentation' string."
    , Option "h" ["help"]     (NoArg Help)
        "Print help and exit"
    ] where
        toApproach :: String -> Either String Approach
        toApproach s
            | s == "first"    = Right First
            | s == "second"   = Right Second
            | s == "second_a" = Right SecondA
            | s == "second_b" = Right SecondB
            | otherwise       = Left s
        toObjects :: String -> Either String [Object]
        toObjects = traverse toObject . splitWhen (== ',')
        toObject :: String -> Either String Object
        toObject s
            | s == "grammar"            = Right ObjectGrammar
            | s == "turing_machine"     = Right ObjectTuringMachine
            | s == "group_presentation" = Right ObjectGroupPresentation
            | otherwise                 = Left s

getFlags :: MonadFail m => [String] -> m [Flag]
getFlags args =
    case getOpt RequireOrder options args of
        (fs, _, []) -> return fs
        (_, _, err) -> fail $ unlines err

data Config =
      PrintingHelp
    | NormalExecution NormalExecutionConfig

data NormalExecutionConfig = NEC
    { _inputFile  :: FilePath
    , _outputFile :: FilePath
    , _errorFile  :: FilePath
    , _approach   :: Approach
    , _infoGr     :: Bool
    , _infoTM     :: Bool
    , _infoGP     :: Bool
    }

makeLenses ''NormalExecutionConfig

getConfig :: MonadFail m => [Flag] -> m Config
getConfig = go defaultNormalExecutionConfig
  where
    defaultNormalExecutionConfig =
        NEC "" "" "" First False False False
    go c [] = return $ NormalExecution c
    go c (f:fs) = case f of
        InputFile  fp -> go (c & inputFile  .~ fp) fs
        OutputFile fp -> go (c & outputFile .~ fp) fs
        ErrorFile  fp -> go (c & errorFile  .~ fp) fs
        UsedApproach (Left na) -> fail $ "Unknown approach: " ++ na
        UsedApproach (Right a) -> go (c & approach .~ a) fs
        GPinLaTeX -> go c fs
        Info (Left no) -> fail $ "Unknown object: " ++ no
        Info (Right []) -> go c fs
        Info (Right (ObjectGrammar:os))           -> go (c & infoGr .~ True) (Info (Right os) : fs)
        Info (Right (ObjectTuringMachine:os))     -> go (c & infoTM .~ True) (Info (Right os) : fs)
        Info (Right (ObjectGroupPresentation:os)) -> go (c & infoGP .~ True) (Info (Right os) : fs)
        Help -> return PrintingHelp

printHelp :: IO ()
printHelp =
    putStr $
        usageInfo "Usage: LangToGroup-cli <options>\nOptions:" options

safeOpen :: FilePath -> (Handle -> IO a) -> IO a
safeOpen "" action = action stdout
safeOpen fp action = bracket (openFile fp WriteMode) hClose action

main :: IO ()
main = do
    args <- getArgs
    flags <- getFlags args
    config <- getConfig flags
    case config of
        PrintingHelp -> printHelp
        NormalExecution nec -> normalExecute nec

normalExecute :: NormalExecutionConfig -> IO ()
normalExecute config = do
    grammar <- parseFromFile Reader.parser (config^.errorFile) (config^.inputFile)
    if config^.approach == First
    then
        let tm = cfg2tm grammar
            (sm, accessWord, _) = tm2sm $ symTM tm
            gr@(GR (a, _)) = sm2gr (sm, accessWord)
            hub = hubRelation accessWord
            genmap = fromList $ zip (toList a) $ map ((++) "f." . show) [1..]
        in  safeOpen (config^.outputFile) $ \handle -> do
                when (config^.infoGr) $
                    hPutStr handle $ showTitleAndInfo grammar
                when (config^.infoTM) $
                    hPutStr handle $ showTitleAndInfo tm
                when (config^.infoGP) $
                    hPutStr handle $ showTitleAndInfo gr
                when (not (config^.infoGr || config^.infoTM || config^.infoGP)) $
                    writeGap gr handle genmap hub
    else do
        tm2sp <- case config^.approach of
                Second  -> return $ semigroupGamma
                SecondA -> return $ semigroupGamma_1
                SecondB -> return $ semigroupGamma_2
                _       -> fail "Unreal case"
        tm <- boolean2tm grammar
        sp <- tm2sp tm
        gp <- groupBeta sp
        safeOpen (config^.outputFile) $ \handle -> do
            when (config^.infoGr) $
                hPutStr handle $ showTitleAndInfo grammar
            when (config^.infoTM) $
                hPutStr handle $ showTitleAndInfo tm
            when (config^.infoGP) $
                hPutStr handle $ showTitleAndInfo gp
            when (not (config^.infoGr || config^.infoTM || config^.infoGP)) $ do
                fmt <- gapFormat gp
                hPutStr handle fmt
