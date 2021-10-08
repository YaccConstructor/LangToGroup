{-# LANGUAGE TemplateHaskell #-}

module StringRewriting.Rule (
    Rule,
    rule,
    left,
    right,
    toPair,
    Rules,
    module StringRewriting.Generator,
  ) where

import StringRewriting.Generator

import Containers
import Lens

data Rule = R
  { _left :: GWord
  , _right :: GWord
  } deriving (Eq, Ord)

rule :: GWord -> GWord -> Rule
rule = R

makeLenses ''Rule

toPair :: Rule -> (GWord, GWord)
toPair (R l r) = (l, r)

type Rules = Set Rule
