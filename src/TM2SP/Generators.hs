{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module TM2SP.Generators (
    Gens,
    simple,
    from,
    gens,
  ) where

import qualified TuringMachine as TM
import qualified SemigroupPresentation as SP
import Format
import Containers
import Lens

import qualified Control.Monad.Reader as R
import Control.Monad.Trans (lift)

type Gens = [Gen]

data Gen =
      JustGens String
    | GroupGens String StrGetter

type StrGetter = TM.TMGetter (Set String)

simple :: String -> Gen
simple = JustGens

from :: String -> StrGetter -> Gen
from = GroupGens

type LocalReader = R.ReaderT TM.TuringMachine Maybe

gens :: MonadFail m => Gens -> TM.TuringMachine -> m SP.GeneratorsDescr
gens = (maybe (fail "Can't create generators") return .) . R.runReaderT . gens_

gens_ :: Gens -> LocalReader SP.GeneratorsDescr
gens_ gs = do
    genNames <- concat <$> traverse gen_ gs
    fromList_ $ zip [minBound..] genNames

gen_ :: Gen -> LocalReader [String]
gen_ (JustGens gs) =
    return $ words gs
gen_ (GroupGens str strGetter) = do
    stateNames <- toList <$> view strGetter
    f :: SimpleFormat <- lift $ format str
    return $ apply f <$> stateNames
