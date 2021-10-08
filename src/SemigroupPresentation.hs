{-# LANGUAGE TemplateHaskell, TupleSections, RankNTypes #-}

-- |Module `SemigroupPresentation` include type of semigroup presentation and
--  useful objects for working with it. This module also export other useful
--  modules.
module SemigroupPresentation (
    GeneratorsDescr,
    SemigroupPresentation,
    semigroupPresentation,
    relations,
    generatorsDescr,
    SPGetter,
    SPGetting,
    SPTraversal',
    generators,
    allGenerators,
    strRelations,
    strGenerators,
    strNumRelations,
    asStrSet,
    module SemigroupPresentation.Relation,
    module Containers,
    module Lens,
  ) where

import SemigroupPresentation.Relation
import Containers
import Lens

import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe, mapMaybe)

type GeneratorsDescr = IsoMap Generator String

data SemigroupPresentation = SP
  { _relations :: Relations
  , _generatorsDescr :: GeneratorsDescr
  }

semigroupPresentation :: Relations -> GeneratorsDescr -> SemigroupPresentation
semigroupPresentation = SP

makeLenses ''SemigroupPresentation

instance Show SemigroupPresentation where
    show sp =
        flip concatMap (sp^.relations.to toList) $
            \(Pair (gw1, gw2)) ->
                let showGen g = fromMaybe "???" (sp^.generatorsDescr.@g)
                in
                    unwords (map showGen gw1) ++
                    " = " ++
                    unwords (map showGen gw2) ++
                    "\n"

type SPGetter a = Getter SemigroupPresentation a

type SPGetting a = Getting a SemigroupPresentation a

type SPTraversal' a = Traversal' SemigroupPresentation a

generators :: SPTraversal' Generator
generators f sp =
    liftA2 semigroupPresentation
        (sp^.relations       & fmap fromList . traverse updR  . toList)
        (sp^.generatorsDescr & fmap fromList . traverse updGD . toList)
          where
            updR = (traverse.traverse) f
            updGD (gen, str) = (,str) <$> f gen

allGenerators :: SPGetter (Set Generator)
allGenerators = to $ do
    rs <- view relations
    gd <- view generatorsDescr
    let generatorsFromGD = valuesSet gd
        generatorsFromR = fromList . view both . unPair
    return $ foldr (\/) generatorsFromGD $ generatorsFromR <$> toList rs

strRelations :: SPGetting (Set StrRelation)
strRelations = to $ do
    gd <- view generatorsDescr
    rs <- view relations
    return $ (gmap.fmap.mapMaybe) (gd !?) rs

strGenerators :: SPGetting (Set String)
strGenerators = generatorsDescr . to valuesSet

strNumRelations :: SPGetting (Set String)
strNumRelations = relations . to (\rs -> fromList $ show <$> [1 .. size rs])

asStrSet :: Show a => Getter a (Set String)
asStrSet = to (singleton . show)
