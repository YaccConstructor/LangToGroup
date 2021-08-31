-- |This module provides functionality for converting the Turing machine 'Tms' to 'TMType.TM'.
module Tms2TuringMachine (tms2turingMachine, hash) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map (Map, lookup, fromList)
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bits (xor)
import Data.Maybe (fromMaybe)

import TuringMachine
import TmsType

tms2turingMachine :: Tms -> Either String TuringMachine
tms2turingMachine
    ( Tms
        ( _,
          startSt,
          [acc],
          commands,
          [alph@(x : xs)]
          )
      ) = do
        oneTapeCmds <- traverse toOneTapeCommand commands -- [(TmsState, TmsSingleTapeCommand, TmsState)]
        let otherStates = filter (\s -> s /= startSt && s /= acc) (concatMap (\(s1, _, s2) -> [s1, s2]) oneTapeCmds)
        let stateToInd = Map.fromList $ zip (acc : startSt : otherStates) [0 ..]
        let alphabet' = fromList $ zip ((Nothing :) $ map Just $ '_' \> alph) [minBound..] :: Alphabet
        return $ turingMachine (fromList $ concatMap (tmsCmd2tmCmd (x :| xs) stateToInd) (zip [0, Prelude.length alph ..] oneTapeCmds)) emptyC alphabet'
    where
        toOneTapeCommand :: TmsCommand -> Either String OneTapeTMCommand
        toOneTapeCommand (TmsCommand (tmsStartSt, [cmd], fol)) = return (tmsStartSt, cmd, fol)
        toOneTapeCommand _                                     = Left "Multi tape command found."

tms2turingMachine _ = Left "Must have only one tape, one accept state and nonempty alphabet."

-- | Hash function used to proivde algorithm with extra unique names.
hash :: String -> Int
hash = foldl (\h c -> 29 * h `xor` ord c) 0

type Transition = (Symbol, SymbolOrMove)

-- | Convert TmsSingleTapeCommand to [Quadriple]
tmsCmd2tmCmd :: NonEmpty.NonEmpty Char -> Map.Map TmsState Int -> (Int, OneTapeTMCommand) -> [TuringMachine.Quadruple]
tmsCmd2tmCmd alph stateToInd (transStart, (iniSt, TmsSingleTapeCommand (action, move), folSt)) = concatMap NonEmpty.toList $ do
    oneSequence <- translate alph (action, move)
    case oneSequence of
        (step :| [])             -> return . return $ makeQuad step iniSt folSt
        (step :| extras@(_ : _)) -> return $
            makeQuad' step iniSt (addIndex transStart iniSt) :|
            (
                (makeTransition iniSt <$> zip [transStart ..] (Prelude.init extras)) ++
                [makeQuad'' (Prelude.last extras) (addIndex (transStart + Prelude.length extras - 1) iniSt) folSt]
            )
    where
        makeQuad (s, m) startSt fol   = ((tmsState2state startSt, s), (m, tmsState2state fol))
        makeQuad' (s, m) startSt fol  = ((tmsState2state startSt, s), (m, fol))
        makeQuad'' (s, m) startSt fol = ((startSt, s), (m, tmsState2state fol))

        makeTransition :: TmsState -> (Int, Transition) -> TuringMachine.Quadruple
        makeTransition startSt (ind, (s, sm)) = ((addIndex ind startSt, s), (sm, addIndex (succ ind) startSt))

        addIndex :: Int -> TmsState -> State
        addIndex x (TmsState name) = tmsState2state $ TmsState (name ++ "_" ++ show x)

        tmsState2state :: TmsState -> State
        tmsState2state st = state $ safeLookup st stateToInd

        safeLookup :: TmsState -> Map.Map TmsState Int -> Int
        safeLookup key@(TmsState name) m = fromMaybe (hash name) (Map.lookup key m)

        -- | List of equivalent sequences of Transitions.
        translate :: NonEmpty Char -> (TmsTapeSquare, TmsTapeHeadMovement) -> NonEmpty (NonEmpty Transition)
        translate _ (ChangeFromTo f t, MoveLeft)  | f == t    = return $ (smb f, M toLeft)  :| []
                                                  | otherwise = return $ (smb f, chg t)     :| [(smb t, M toLeft)]
        translate _ (ChangeFromTo f t, MoveRight) | f == t    = return $ (smb f, M toRight) :| []
                                                  | otherwise = return $ (smb f, chg t)     :| [(smb t, M toRight)]
        translate _ (ChangeFromTo f t, Stay)                  = return $ (smb f, chg t)     :| []

        translate abc (Leave, Stay) = do
            ch <- NonEmpty.cons '_' abc
            return $ pure (smb ch, chg ch)
        translate abc (Leave, MoveLeft) = do
            ch <- NonEmpty.cons '_' abc
            return $ pure (smb ch, M toLeft)
        translate abc (Leave, MoveRight) = do
            ch <- NonEmpty.cons '_' abc
            return $ pure (smb ch, M toRight)

        smb :: Char -> Symbol
        smb '_' = blankSymbol
        smb c   = symbol $ ord c

        chg :: Char -> SymbolOrMove
        chg = S . smb
