{-# LANGUAGE TemplateHaskell #-}

module Approach (
    ApprConfig,
    inputFile,
    outputFile,
    errorFile,
    infoAbout,
    defaultApprConfig,
    Approach,
    defaultApproach,
    getApproach,
    approachesInfo,
    module Lens,
  ) where

import Object (Object)
import qualified Object
import Lens

import TuringMachine
import SemigroupPresentation
import GRType (GR (GR))
import GrammarReader (parseFromFile, parser)
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

import Control.Exception (bracket)
import Control.Monad (when)
import Data.List (find, intercalate)
import System.IO

data ApprConfig = ApprConfig
    { _inputFile  :: FilePath
    , _outputFile :: FilePath
    , _errorFile  :: FilePath
    , _infoAbout  :: Set Object
    }

makeLenses ''ApprConfig

defaultApprConfig :: ApprConfig
defaultApprConfig = ApprConfig "" "" "" emptyC

type Approach = ApprConfig -> IO ()

defaultApproach :: Approach
defaultApproach = firstApproach

data ApprDescr = Approach
    { names :: [String]
    , appr  :: Approach
    , descr :: String
    }

approaches :: [ApprDescr]
approaches =
    [ Approach ["first"]     firstApproach
        "Implementation of algorithm from\
        \ \"Isoperimetric and Isodiametric Functions of Groups\""
    , Approach ["second"]   (secondApproach semigroupGamma)
        "Implementation of algorithms from\
        \ \"Boolean grammars\" and \"An Introduction to the Theory of Groups\""
    , Approach ["second_a"] (secondApproach semigroupGamma_1)
        "Modifications of `second` approach with modified algorithm from\
        \ \"An Introduction to the Theory of Groups\""
    , Approach ["second_b"] (secondApproach semigroupGamma_2)
        "Modifications of `second` approach with modified algorithm from\
        \ \"An Introduction to the Theory of Groups\""
    ]

firstApproach :: Approach
firstApproach config = do
    grammar <- parseFromFile parser
        (config ^. errorFile) (config ^. inputFile)
    let tm = cfg2tm grammar
        (sm, accessWord, _) = tm2sm $ symTM tm
        gr@(GR (a, _)) = sm2gr (sm, accessWord)
        hub = hubRelation accessWord
        genmap = fromList $ zip (toList a) $ map ((++) "f." . show) [1..]
    safeOpen (config ^. outputFile) $ \handle -> do
        when (config ^. infoAbout & member Object.Grammar) $
            hPutStr handle $ showTitleAndInfo grammar
        when (config ^. infoAbout & member Object.TuringMachine) $
            hPutStr handle $ showTitleAndInfo tm
        when (config ^. infoAbout & member Object.GroupPresentation) $
            hPutStr handle $ showTitleAndInfo gr
        when (config ^. infoAbout & nullC) $
            writeGap gr handle genmap hub

secondApproach :: (TuringMachine -> IO SemigroupPresentation) -> Approach
secondApproach tm2sp config = do
    grammar <- parseFromFile parser
        (config ^. errorFile) (config ^. inputFile)
    tm <- boolean2tm grammar
    sp <- tm2sp tm
    gp <- groupBeta sp
    safeOpen (config ^. outputFile) $ \handle -> do
        when (config ^. infoAbout & member Object.Grammar) $
            hPutStr handle $ showTitleAndInfo grammar
        when (config ^. infoAbout & member Object.TuringMachine) $
            hPutStr handle $ showTitleAndInfo tm
        when (config ^. infoAbout & member Object.GroupPresentation) $
            hPutStr handle $ showTitleAndInfo gp
        when (config ^. infoAbout & nullC) $ do
            fmt <- gapFormat gp
            hPutStr handle fmt

safeOpen :: FilePath -> (Handle -> IO a) -> IO a
safeOpen "" action = action stdout
safeOpen fp action = bracket (openFile fp WriteMode) hClose action

getApproach :: MonadFail m => String -> m Approach
getApproach s =
    maybe (fail $ "Unknown approach: " ++ s) return $
        appr <$> find (\approach -> s `elem` names approach) approaches

approachesInfo :: String
approachesInfo = unlines $
    [ "Approaches:" ] ++
    ( do
        approach <- approaches
        [ "  " ++ intercalate ", " (names approach), "    " ++ descr approach ]
      )
