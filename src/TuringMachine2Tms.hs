-- |This module represents functionality for converting the 'TMTypes.TuringMachine' to 'TmsType.Tms'.
module TuringMachine2Tms (turingMachine2tms, turingMachineSt2tmsSt) where

import Data.Map (toList)
import Data.Char (chr, ord)
import Data.Functor ((<&>))
import Data.Set (fromList, toList)

import TmsType
import TMTypes

turingMachine2tms :: TuringMachine -> Tms
turingMachine2tms (TM quads) = 
    Tms (
            "TMTypes.TuringMachine",
            [turingMachineSt2tmsSt startState],
            [[turingMachineSt2tmsSt finalState]],
            quadsList <&> turingMCmd2tms <&> TmsCommand . pure,
            [extractAlphabet quadsList]
        )
    where
        quadsList = Data.Map.toList quads

-- Section of helper functions.

turingMachineSt2tmsSt :: State -> TmsState
turingMachineSt2tmsSt (Q ind) = TmsState $ "Q_" ++ show ind

extractAlphabet :: [Quadruple] -> [Char]
extractAlphabet = Data.Set.toList . Data.Set.fromList . (concatMap extChars)
    where
        extChars :: Quadruple -> [Char]
        extChars ((_, f), (C t, _)) = symb2char <$> [f, t]
        extChars ((_, f), _)        = symb2char <$> [f]

turingMCmd2tms :: Quadruple -> TmsSingleTapeCommand
turingMCmd2tms ((iniSt, from), (act, folSt)) =
    TmsSingleTapeCommand (
        action, turingMachineSt2tmsSt iniSt, turingMachineSt2tmsSt folSt, move
        )
    where
        (move, action) = case act of
            C to -> (Stay,      ChangeFromTo (symb2char from) (symb2char to))
            L    -> (MoveLeft,  ChangeFromTo (symb2char from) (symb2char from))
            R    -> (MoveRight, ChangeFromTo (symb2char from) (symb2char from))

symb2char :: Symbol -> Char
symb2char (S 0) = '_'
symb2char (S c)       = chr $ c + (ord 'a' - 1)
