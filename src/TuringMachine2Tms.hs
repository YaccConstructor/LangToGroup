-- |This module represents functionality for converting the 'TMTypes.TuringMachine' to 'TmsType.Tms'.
module TuringMachine2Tms (turingMachine2tms, turingMachineSt2tmsSt) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.List (sort)

import TmsType
import TuringMachine

turingMachine2tms :: TuringMachine -> Tms
turingMachine2tms tm =
    Tms (
            "TMTypes_TuringMachine",
            turingMachineSt2tmsSt startState,
            [turingMachineSt2tmsSt finalState],
            sort $ toList (tm^.quadruples) <&> turingMCmd2tms (tm^.alphabet) <&> toTmsCommand,
            [head . show <$> (blank \> (values (tm^.alphabet) :: ShowedSymbols))]
        )

-- Section of helper functions.

turingMachineSt2tmsSt :: State -> TmsState
turingMachineSt2tmsSt q = TmsState $ "Q_" ++ show (numState q)

turingMCmd2tms :: Alphabet -> TuringMachine.Quadruple -> OneTapeTMCommand
turingMCmd2tms alph ((iniSt, from), (act, folSt)) = (
        turingMachineSt2tmsSt iniSt,
        TmsSingleTapeCommand (action, move),
        turingMachineSt2tmsSt folSt)
    where
        (move, action) = case act of
            S to -> (Stay,      ChangeFromTo (symb2char from) (symb2char to))
            M m | m == toLeft -> (MoveLeft,  ChangeFromTo (symb2char from) (symb2char from))
                | otherwise   -> (MoveRight, ChangeFromTo (symb2char from) (symb2char from))
        symb2char :: Symbol -> Char
        symb2char s = fromMaybe '?' $ do
            s' <- alph !? s
            return $
                if s' == blank
                then '_'
                else head $ show s'
