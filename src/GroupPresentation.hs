{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module GroupPresentation (
    GeneratorsDescr,
    GroupPresentation,
    groupPresentation,
    relations,
    generatorsDescr,
    GPGetter,
    generators,
    allGenerators,
    strGenerators,
    module GroupPresentation.Relation,
    module Containers,
    module Lens,
  ) where

import GroupPresentation.Relation
import Containers
import Lens

type GeneratorsDescr = IsoMap Generator String

data GroupPresentation = GP
  { _relations :: Relations
  , _generatorsDescr :: GeneratorsDescr
  }

groupPresentation :: Relations -> GeneratorsDescr -> GroupPresentation
groupPresentation = GP

makeLenses ''GroupPresentation

instance Show GroupPresentation where
    show gp =
        flip concatMap (gp^.relations.to toList) $
            \(Pair (ew1, ew2)) ->
                let showElem e =
                        let g = getGenerator e
                            s = if isPositive e then "" else "^-1"
                        in  maybe "???" (++ s) (gp^.generatorsDescr.@g)
                in
                    unwords (map showElem ew1) ++
                    " = " ++
                    unwords (map showElem ew2) ++
                    "\n"

type GPGetter a = Getter GroupPresentation a

generators :: GPGetter (Set Generator)
generators = generatorsDescr . to valuesSet

allGenerators :: GPGetter (Set Generator)
allGenerators = to $ do
    rs <- view relations
    gd <- view generatorsDescr
    let generatorsFromGD = valuesSet gd
        generatorsFromR = fromList . map unSigned . view both . unPair
    return $ foldr (\/) generatorsFromGD $ generatorsFromR <$> toList rs

strGenerators :: GPGetter (Set String)
strGenerators = generatorsDescr . to valuesSet
