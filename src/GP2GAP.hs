module GP2GAP (
    gapFormat,
  ) where

import GroupPresentation

import Data.List (intercalate)

type GAPformat = String

gapFormat :: MonadFail m => GroupPresentation -> m GAPformat
gapFormat gp = do
    gens <- gens2str gp
    rels <- rels2str gp
    return $
        "local f, g;\n\
        \f := FreeGroup( " ++ gens ++ " );\n\
        \g := f / [ " ++ rels ++ " ];\n\
        \return g;\n"

gens2str :: MonadFail m => GroupPresentation -> m String
gens2str gp = return $ intercalate ", " $ toList $ gp^.strGenerators

rels2str :: MonadFail m => GroupPresentation -> m String
rels2str gp = do
    let gd = gp^.generatorsDescr
    rels <- traverse (rel2str gd) (toList $ gp^.relations)
    return $ intercalate ", " rels

rel2str :: MonadFail m => GeneratorsDescr -> Relation -> m String
rel2str gd (Pair (ew1, ew2)) = do
    let ew = ew1 ++ reverse ((^~) <$> ew2)
    es <- traverse (elem2str gd) ew
    return $ intercalate " * " es

elem2str :: MonadFail m => GeneratorsDescr -> Element -> m String
elem2str gd (Positive g) = gd !? g
elem2str gd (Negative g) = (++ "^(-1)") <$> (gd !? g)
