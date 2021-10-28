-- |Module `SP2GP` include functions `groupBeta` and `groupBeta_1` for
--  converting semigroup presentation to group presentation.
module SP2GP (
    groupBeta,
  ) where

import SP2GP.Generators
import SP2GP.Relations
import SemigroupPresentation
import qualified GroupPresentation as GP

sp2gp ::
    MonadFail m =>
    Gens ->
    Rels ->
    SemigroupPresentation ->
    m GP.GroupPresentation
sp2gp gs rs' sp = do
    gd <- gens gs sp
    rs <- rels rs' gd sp
    return $ GP.groupPresentation rs gd

groupBeta :: MonadFail m => SemigroupPresentation -> m GP.GroupPresentation
groupBeta =
    sp2gp [
            group (strGenerators.filterByFormat("q{}")),
            simple "s",
            group (strGenerators.filterByFormat("s{}")),
            "r_{}" `from` strNumRelations,
            simple "x t k"
        ] [[[
            "x s_B" === "s_B x^2",
            "r_i s_B" === "s_B x r_i x",
            "r_i^-1 F_i^# q_i_1 G_i r_i" === "H_i^# q_i_2 K_i",
            "t r_i" === "r_i t",
            "t x" === "x t",
            "k r_i" === "r_i k",
            "k x" === "x k",
            "k q^-1 t q" === "q^-1 t q k"
        ] & for' (
            "{s_B}" `in'`
            (strGenerators.filterByFormat("s_{}").insertGen("s")))
        ] & for' (
            ("r_{i}", "F_i q{_i_1} G_i" === "H_i q{_i_2} K_i") `in'`
            (strNumRelations, strRelations.forAll(replaceGenerator("h", "s")))
          )
        ]
