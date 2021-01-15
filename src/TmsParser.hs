-- |This module provides types for parsing 'Tms' Turing machines.
module TmsParser where

import Text.Megaparsec

import Data.Text (Text)
import Data.String

import TMTypes
import ParsingHelpers

-- |Parser part.

-- pTms :: Parser Tms
-- pTms = undefined

-- parser = makeEofParser pTms
