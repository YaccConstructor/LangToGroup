module StringRewriting.Rewrite (
    rewrite,
    rewrite',
    module StringRewriting,
  ) where

import StringRewriting

import qualified Math.Algebra.Group.StringRewriting as SR
import Control.Monad (forM)

rewrite' :: StringRewriting -> GWord -> GWord
rewrite' = SR.rewrite . map toPair . toList . view rules

rewrite :: MonadFail m => StringRewriting -> String -> m String
rewrite sr strIni = do
    gwIni <- forM (words strIni) $ \strGen -> sr^.generatorsDescr.@strGen
    let gwRes = rewrite' sr gwIni
    strGens <- forM gwRes $ \gen -> sr^.generatorsDescr.@gen
    return $ unwords strGens
