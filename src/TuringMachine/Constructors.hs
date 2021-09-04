{-# LANGUAGE MultiWayIf, TupleSections, ScopedTypeVariables #-}

module TuringMachine.Constructors (
    makeStandartTM,
    crash,
    check,
    die,
    move,
    moveInf,
    rewrite,
    rewriteAndMove,
    (+^>),
    (>+^),
    (-^),
    (++>),
    (||>),
    (@@>),
    loop,
    forSs,
    module TuringMachine,
  ) where

import TuringMachine

import Containers.Map (intersectionWith)
import Containers.PrismMap (toMap)

import Control.Monad (forM_, (>=>))
import Data.Maybe (mapMaybe)
import qualified Control.Monad.State as ST

infixl 8 +^>, >+^
infix  7 @@>
infixl 6 ++>
infixl 5 ||>

type LocalState = ST.State (TuringMachine, TuringMachine)

type LocalQuadruple = (State, ShowedSymbol, ShowedSymbolOrMove, State)

makeStandartTM :: [LocalQuadruple] -> TuringMachine
makeStandartTM lqs =
    let cs = fromList (concatMap getSs lqs) <\ blank :: Set ShowedSymbol
        ls = fromList [] :: PrismMap String State
        a  = fromList $ zip (blank : toList cs) [minBound..]
    in  turingMachine (fromList $ mapMaybe (toQ a) lqs) ls a
      where
        getSs :: LocalQuadruple -> [ShowedSymbol]
        getSs (_, s, M _, _) = [s]
        getSs (_, s, S s', _) = [s, s']
        toQ :: Alphabet -> LocalQuadruple -> Maybe TuringMachine.Quadruple
        toQ a (q1, s, M m, q2) = do
            s_ <- a !? s
            return ((q1, s_), (M m, q2))
        toQ a (q1, s, S s', q2) = do
            s_  <- a !? s
            s'_ <- a !? s'
            return ((q1, s_), (S s'_, q2))

forSs ::
    ShowedSymbolClass s =>
    (ShowedSymbol -> TuringMachine) ->
    [s] ->
    TuringMachine
forSs sstm = foldr (||>) crash . map sstm . showedSymbols

crash :: TuringMachine
crash = makeStandartTM []

check :: ShowedSymbolClass s => [s] -> TuringMachine
check = forSs $ \s -> makeStandartTM [(startState, s, S s, finalState)]

die :: ShowedSymbolClass s => [s] -> TuringMachine
die = forSs $ \s -> makeStandartTM [(startState, s, S s, startState)]

move :: ShowedSymbolClass s => Move -> [s] -> TuringMachine
move m = forSs $ \s -> makeStandartTM [(startState, s, M m, finalState)]

moveInf :: ShowedSymbolClass s => Move -> [s] -> TuringMachine
moveInf m = forSs $ \s -> makeStandartTM [(startState, s, M m, startState)]

rewrite :: ShowedSymbolClass s => [s] -> ShowedSymbol -> TuringMachine
rewrite ss s' =
    forSs (\s -> makeStandartTM [(startState, s, S s', finalState)]) ss

(+^>) :: String -> TuringMachine -> TuringMachine
lbl +^> tm = tm & labeledStates <+~ (lbl, startState)

(>+^) :: TuringMachine -> String -> TuringMachine
tm >+^ lbl = tm & labeledStates <+~ (lbl, finalState)

(-^) :: TuringMachine -> String -> TuringMachine
tm -^ lbl = tm & labeledStates %~ (<\ lbl)

makeTMop ::
    LocalState () ->
    TuringMachine ->
    TuringMachine ->
    TuringMachine
makeTMop mainAction = curry $ ST.evalState $ do
    mainAction
    newLS <- updateLabeledStates
    newA  <- updateAlphabet
    unionTMs newLS newA
      where
        updateLabeledStates = do
            (ls1, ls2) <- ST.gets $ both %~ view labeledStates
            let sharedLabels = toList $
                    intersectionWith (,) (toMap ls1) (toMap ls2)
            forM_ sharedLabels $ \(_, (s1, s2)) ->
                _2.states %= \s -> Just $
                    if s == s2
                    then s1
                    else s
            let ls1' = toList ls1
                ls2' = 
                    mapMaybe
                        (\lbl -> (,lbl) <$> (ls2 !? lbl))
                        (toList $ valuesSet ls2 \\ valuesSet ls1)
            return $ fromList $ ls1' ++ ls2'
        updateAlphabet = do
            (a1, a2) <- ST.gets $ both %~ view alphabet
            let allChars :: [ShowedSymbol] = toList $ valuesSet a1 \/ valuesSet a2
                newA = fromList $ zip allChars [minBound..]
            forM_ [(_1, a1), (_2, a2)] $ \(tm, a) ->
                tm.symbols %= ((a !?) >=> (newA !?))
            return newA
        --here you must use either FlexibleContexts extension or type definition
        unionTMs :: LabeledStates -> Alphabet -> LocalState TuringMachine
        unionTMs newLS newA = do
            (qs1, qs2) <- ST.gets $ both %~ view quadruples
            let newQs = qs1 \/ qs2
            return $ turingMachine newQs newLS newA

(++>) :: TuringMachine -> TuringMachine -> TuringMachine
(++>) = makeTMop $ do
    newStartState2 <- uses (_1.maxNumState) (state . (+1))
    _1.states %= \s -> Just $
        if s == finalState
        then newStartState2
        else s
    _2.states %= \s -> Just $
        if s == finalState
        then finalState
        else s + newStartState2 - startState

(||>) :: TuringMachine -> TuringMachine -> TuringMachine
(||>) = makeTMop $ do
    maxState1 <- uses (_1.maxNumState) state
    _2.states %= \s -> Just $
        if  | s == startState -> startState
            | s == finalState -> finalState
            | otherwise -> s - startState + maxState1

(@@>) :: TuringMachine -> TuringMachine -> TuringMachine
(@@>) = makeTMop $ do
    maxState1 <- uses (_1.maxNumState) state
    _2.states %= \s -> Just $
        if  | s == startState -> finalState
            | s == finalState -> startState
            | otherwise -> s - startState + maxState1

loop :: TuringMachine -> TuringMachine
loop =
    states %~ \s -> Just $
        if s == finalState
        then startState
        else s

rewriteAndMove :: ShowedSymbolClass s => [s] -> ShowedSymbol -> Move -> TuringMachine
rewriteAndMove ss s m = rewrite ss s ++> move m [s]
