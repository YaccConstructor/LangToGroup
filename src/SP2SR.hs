module SP2SR (
    sp2sr,
    sp2sr',
  ) where

import qualified SemigroupPresentation as SP
import SemigroupPresentation (
    SemigroupPresentation,
    relations,
    generatorsDescr
  )
import qualified StringRewriting as SR
import StringRewriting.KnuthBendix (StringRewriting, Order, knuthBendixBy)
import Containers
import Lens

gen :: SP.Generator -> SR.Generator
gen = SR.generator . SP.numGenerator

genOrder :: Order SP.Generator -> Order SR.Generator
genOrder = (`on` (SP.generator . SR.numGenerator))

sp2sr ::
    MonadFail m =>
    Order SP.Generator ->
    SemigroupPresentation ->
    m StringRewriting
sp2sr go sp = return $
    knuthBendixBy
        (genOrder go)
        ((gmap.fmap.map) gen $ sp^.relations)
        (gmap gen $ sp^.generatorsDescr)

sp2sr' :: MonadFail m => SemigroupPresentation -> m StringRewriting
sp2sr' = sp2sr compare
