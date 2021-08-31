{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module SP2GP.Generators (
    Gens,
    simple,
    from,
    group,
    gens,
  ) where

import qualified SemigroupPresentation as SP
import qualified GroupPresentation as GP
import Format
import Containers
import Lens

import qualified Control.Monad.Reader as R
import Control.Monad.Trans (lift)

type Gens = [Gen]

data Gen =
      JustGens String
    | FromGroupGens String StrGetter
    | JustGroupGens StrGetter

type StrGetter = SP.SPGetting (Set String)

simple :: String -> Gen
simple = JustGens

from :: String -> StrGetter -> Gen
from = FromGroupGens

group :: StrGetter -> Gen
group = JustGroupGens

type LocalReader = R.ReaderT SP.SemigroupPresentation Maybe

gens :: MonadFail m => Gens -> SP.SemigroupPresentation -> m GP.GeneratorsDescr
gens = (maybe (fail "Can't create generators") return .) . R.runReaderT . gens_

gens_ :: Gens -> LocalReader GP.GeneratorsDescr
gens_ gs = do
    genNames <- concat <$> traverse gen_ gs
    fromList_ $ zip [minBound..] genNames

gen_ :: Gen -> LocalReader [String]
gen_ (JustGens gs) =
    return $ words gs
gen_ (FromGroupGens str strGetter) = do
    stateNames <- toList <$> view strGetter
    f :: SimpleFormat <- lift $ format str
    return $ apply f <$> stateNames
gen_ (JustGroupGens strGetter) = do
    stateNames <- toList <$> view strGetter
    return stateNames
