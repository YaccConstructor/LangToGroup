{-# LANGUAGE TemplateHaskell #-}

module StringRewriting (
    GeneratorsDescr,
    StringRewriting,
    stringRewriting,
    rules,
    generatorsDescr,
    module StringRewriting.Rule,
    module Containers,
    module Lens,
  ) where

import StringRewriting.Rule
import Containers
import Lens

import Data.Maybe (fromMaybe)

type GeneratorsDescr = IsoMap Generator String

data StringRewriting = SR
  { _rules :: Rules
  , _generatorsDescr :: GeneratorsDescr
  }

stringRewriting :: Rules -> GeneratorsDescr -> StringRewriting
stringRewriting = SR

makeLenses ''StringRewriting

instance Show StringRewriting where
    show sr =
        flip concatMap (sr^.rules) $ \r ->
            let showGen g = fromMaybe "???" (sr^.generatorsDescr.@g)
                showGWord = unwords . map showGen
            in  showGWord (r^.left) ++ " -> " ++ showGWord (r^.right) ++ "\n"
