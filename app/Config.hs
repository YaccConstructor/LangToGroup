{-# LANGUAGE TemplateHaskell #-}

module Config (
    Config (..),
    approach,
    apprConfig,
    defaultConfig,
    getConfig,
    module Option,
  ) where

import Option

data Config =
      PrintHelp
    | ApplyApproach
        { _approach   :: Approach
        , _apprConfig :: ApprConfig
        }

makeLenses ''Config

defaultConfig :: Config
defaultConfig = PrintHelp

getConfig :: MonadFail m => [Option] -> m Config
getConfig = go defaultConfig
  where
    go config [] = return config
    go PrintHelp os = go (ApplyApproach defaultApproach defaultApprConfig) os
    go applyApproach (o:os) = case o of
        InputFile  fp -> go (applyApproach & apprConfig . inputFile  .~ fp) os
        OutputFile fp -> go (applyApproach & apprConfig . outputFile .~ fp) os
        ErrorFile  fp -> go (applyApproach & apprConfig . errorFile  .~ fp) os
        UsedApproach appr' -> go (applyApproach & approach .~ appr') os
        InfoAbout objs -> go (applyApproach & apprConfig . infoAbout \/~ objs) os
        Help -> return PrintHelp
