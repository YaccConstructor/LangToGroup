{-# LANGUAGE TemplateHaskell #-}

module TuringMachine.Interpreter (
    WorkingState,
    currentState,
    tape,
    step,
    initWS,
    run,
    smartRun,
    module TuringMachine.Interpreter.Tape,
    module TuringMachine,
  ) where

import TuringMachine.Interpreter.Tape
import TuringMachine

data WorkingState = WS
  { _currentState  :: State
  , _tape          :: Tape
  } deriving (Eq)

instance Show WorkingState where
    show (WS q t) = show t ++ "   (" ++ show (numState q) ++ ")"

makeLenses ''WorkingState

step ::
    MonadFail m =>
    TuringMachine ->
    WorkingState ->
    m (TuringMachine.Quadruple, WorkingState)
step tm (WS q t) = do
    s <- tm^.alphabet.@(t^.top)
    (sm, q') <- tm^.quadruples.@(q, s)
    t' <- case sm of
            S s' -> do
                c <- tm^.alphabet.@s'
                return (t & top .~ c)
            M m ->
                return (t^.move(m))
    return (((q, s), (sm, q')), WS q' t')

initWS :: String -> Int -> WorkingState
initWS s i = WS startState (fromString s i)

run :: TuringMachine -> WorkingState -> [WorkingState]
run tm ws =
    case step tm ws of
        Nothing -> [ws]
        Just (_, ws') -> ws : run tm ws'

smartRun :: TuringMachine -> WorkingState -> [WorkingState]
smartRun tm ws =
    case step tm ws of
        Nothing -> [ws]
        Just (((qf, s), (sm, qt)), ws') ->
            if qf == qt && S s == sm
            then [ws, ws']
            else ws : smartRun tm ws'
