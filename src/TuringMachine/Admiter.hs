{-# LANGUAGE MultiWayIf #-}

-- |Module `TuringMachine.Admiter` include `admit` function for detecting if
--  input Turing machine admit input string.
module TuringMachine.Admiter (
    Depth,
    Action,
    admit,
    admit',
    module TuringMachine,
  ) where

import TuringMachine

import TuringMachine.Interpreter

type Depth = Maybe Int

type Action = SymbolOrMove

admit :: MonadFail m => TuringMachine -> Depth -> (String, Int) -> m [Action]
admit tm depth (strIni, posIni) =
    admit' tm depth (initWS strIni posIni)

admit' :: MonadFail m => TuringMachine -> Depth -> WorkingState -> m [Action]
admit' = go []
  where
    go _ _ (Just 0) _ =
        fail "Analysis depth limit reached"
    go res tm depth ws  = do
        (((q, s), (sm, q')), ws') <- step tm ws
        if  | q' == finalState ->
                return $ reverse res
            | q == q' && S s == sm ->
                fail "Loop was found"
            | otherwise ->
                go (sm : res) tm (subtract 1 <$> depth) ws'
