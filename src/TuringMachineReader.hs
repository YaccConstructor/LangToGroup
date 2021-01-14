module TuringMachineReader (parser) where

import Text.Megaparsec

import Data.Text (Text)
import Data.String

import TMType
import ParsingHelpers

-- |Parser part.

pTuringMachine :: Parser TM
pTuringMachine = undefined

parser = makeEofParser pTuringMachine
