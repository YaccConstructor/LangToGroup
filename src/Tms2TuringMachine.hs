-- |This module provides functionality for converting the Turing machine 'Tms' to 'TMType.TM'.
module Tms2TuringMachine (tms2turingMachine, hash) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map(..), lookup, insert, fromList)
import Data.Bits (xor)

import TMTypes
import TmsType

tms2turingMachine :: Tms -> Either String TuringMachine
tms2turingMachine
    ( Tms
        ( _,
          ini,
          (acc : []),
          commands,
          [alphabet@(x : xs)]
          )
      ) = do
        oneTapeCmds <- traverse toOneTapeCommand commands -- [(TmsState, TmsSingleTapeCommand, TmsState)]
        let otherStates = filter (\s -> s /= ini && s /= acc) (concatMap (\(s1, _, s2) -> [s1, s2]) oneTapeCmds)
        let stateToInd = Map.fromList $ zip (acc : ini : otherStates) [0 ..]
        return $ TMTypes.fromList $ concatMap (tmsCmd2tmCmd (x :| xs) stateToInd) (zip [0, Prelude.length alphabet ..] oneTapeCmds)
    where
        toOneTapeCommand :: TmsCommand -> Either String OneTapeTMCommand
        toOneTapeCommand (TmsCommand (ini, [cmd], fol)) = return (ini, cmd, fol)
        toOneTapeCommand _                              = fail "Multi tape command found."

tms2turingMachine _ = fail "Must have only one tape, one accept state and nonempty alphabet."

-- | Hash function used to proivde algorithm with extra unique names.
hash :: String -> Int
hash = foldl (\h c -> 29 * h `xor` ord c) 0

type Transition = (Symbol, SymbolMove)

-- | Convert TmsSingleTapeCommand to [Quadriple]
tmsCmd2tmCmd :: NonEmpty.NonEmpty Char -> Map TmsState Int -> (Int, OneTapeTMCommand) -> [Quadruple]
tmsCmd2tmCmd alph stateToInd (transStart, (iniSt, (TmsSingleTapeCommand (action, move)), folSt)) = concatMap NonEmpty.toList $ do
    oneSequence <- translate alph (action, move)
    case oneSequence of
        (step :| [])             -> return . return $ makeQuad step iniSt folSt
        (step :| extras@(_ : _)) -> return $
            (makeQuad' step iniSt (addIndex transStart iniSt)) :|
            (
                (makeTransition iniSt <$> zip [transStart ..] (Prelude.init extras)) ++
                [makeQuad'' (Prelude.last extras) (addIndex (transStart + (Prelude.length extras) - 1) iniSt) folSt]
            )
    where
        makeQuad (s, m) ini fol = ((tmsState2state ini, s), (m, tmsState2state fol))
        makeQuad' (s, m) ini fol = ((tmsState2state ini, s), (m, fol))
        makeQuad'' (s, m) ini fol = ((ini, s), (m, tmsState2state fol))

        makeTransition :: TmsState -> (Int, Transition) -> Quadruple
        makeTransition ini (ind, (s, mv)) = ((addIndex ind ini, s), (mv, addIndex (succ ind) ini))

        addIndex :: Int -> TmsState -> State
        addIndex x (TmsState name) = tmsState2state $ TmsState (name ++ "_" ++ show x)

        tmsState2state :: TmsState -> State
        tmsState2state state = Q $ safeLookup state stateToInd

        safeLookup :: TmsState -> Map TmsState Int -> Int
        safeLookup key@(TmsState name) map = case Map.lookup key map of
            Nothing  -> hash name
            Just val -> val

        -- | List of equivalent sequences of Transitions.
        translate :: NonEmpty Char -> (TmsTapeSquare, TmsTapeHeadMovement) -> NonEmpty (NonEmpty Transition)
        translate _ ((ChangeFromTo f t), MoveLeft)  | f == t    = return $ (smb f, L)     :| []
                                                    | otherwise = return $ (smb f, chg t) :| [(smb t, L)]
        translate _ ((ChangeFromTo f t), MoveRight) | f == t    = return $ (smb f, R)     :| []
                                                    | otherwise = return $ (smb f, chg t) :| [(smb t, R)]
        translate _ ((ChangeFromTo f t), Stay)                  = return $ (smb f, chg t) :| []

        translate abc (Leave, Stay) = do
            ch <- NonEmpty.cons '_' abc
            return $ pure (smb ch, chg ch)
        translate abc (Leave, MoveLeft) = do
            ch <- NonEmpty.cons '_' abc
            return $ pure (smb ch, L)
        translate abc (Leave, MoveRight) = do
            ch <- NonEmpty.cons '_' abc
            return $ pure (smb ch, R)

        smb :: Char -> Symbol
        smb '_' = emptySymbol
        smb c   = S $ ord c

        chg :: Char -> SymbolMove
        chg = C . smb
