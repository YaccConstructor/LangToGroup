module GP2SR (
    gp2sr,
    gp2sr',
  ) where

import qualified GroupPresentation as GP
import GroupPresentation (
    GroupPresentation,
    relations,
    generatorsDescr,
    Signed (Positive, Negative)
  )
import qualified StringRewriting as SR
import StringRewriting.KnuthBendix (StringRewriting, Order, knuthBendixBy)
import Containers
import Lens

gen :: GP.Element -> SR.Generator
gen (Positive g) = SR.generator $ GP.numGenerator g * 2
gen (Negative g) = SR.generator $ GP.numGenerator g * 2 + 1

genOrder :: Order GP.Element -> Order SR.Generator
genOrder = (`on`
    (\n ->
        if n `mod` 2 == 0
        then Positive $ GP.generator $ n `div` 2
        else Negative $ GP.generator $ n `div` 2
      ) .
    SR.numGenerator
  )

gd :: GP.GeneratorsDescr -> SR.GeneratorsDescr
gd gdIni = fromList $
    ((_1 %~ (gen.Positive)) <$> toList gdIni) ++
    ((_1 %~ (gen.Negative)) <$> (_2 %~ (++ "^-1")) <$> toList gdIni)

gp2sr ::
    MonadFail m =>
    Order GP.Element ->
    GroupPresentation ->
    m StringRewriting
gp2sr eo gp = return $
    knuthBendixBy
        (genOrder eo)
        ((gmap.fmap.map) gen $ gp^.relations)
        (gd (gp^.generatorsDescr))

gp2sr' :: MonadFail m => GroupPresentation -> m StringRewriting
gp2sr' = gp2sr compare
