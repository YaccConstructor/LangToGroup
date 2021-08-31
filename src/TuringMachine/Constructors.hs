{-# LANGUAGE MultiWayIf, TupleSections, ScopedTypeVariables #-}

module TuringMachine.Constructors (
    CharOrMove (C, M'),
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
    forCs,
    module TuringMachine,
  ) where

import TuringMachine

import Containers.Map (intersectionWith)
import Containers.PrismMap (toMap)

import Control.Monad (forM_, (>=>))
import Data.Maybe (mapMaybe)
import Data.List.Extra (nubOrd)
import qualified Control.Monad.State as ST

infixl 8 +^>, >+^
infix  7 @@>
infixl 6 ++>
infixl 5 ||>

type LocalState = ST.State (TuringMachine, TuringMachine)

data CharOrMove =
      C Char
    | M' Move

type LocalQuadruple = (State, Char, CharOrMove, State)

makeStandartTM :: [LocalQuadruple] -> TuringMachine
makeStandartTM lqs =
    let cs = blankChar \> (nubOrd $ concatMap getCs lqs)
        sd = fromList ([] :: [(String, State)])
        a  = fromList $ zip (Nothing : map Just cs) [minBound..]
    in  turingMachine (fromList $ mapMaybe (toQ a) lqs) sd a
      where
        getCs :: LocalQuadruple -> [Char]
        getCs (_, c, M' _, _) = [c]
        getCs (_, c, C c', _) = [c, c']
        toQ :: Alphabet -> LocalQuadruple -> Maybe TuringMachine.Quadruple
        toQ a (q1, c, M' m, q2) =
            let mc = if c == blankChar then Nothing else Just c
            in  (\c' -> ((q1, c'), (M m, q2))) <$> (a !? mc)
        toQ a (q1, c, C c', q2) =
            let mc  = if c  == blankChar then Nothing else Just c
                mc' = if c' == blankChar then Nothing else Just c'
            in  (\c1 c2 -> ((q1, c1), (S c2, q2)))
                    <$> (a !? mc)
                    <*> (a !? mc')

forCs :: (Char -> TuringMachine) -> String -> TuringMachine
forCs ctm cs = foldr (||>) crash $ map ctm cs -- `cs' may be changed to `nubOrd cs'

crash :: TuringMachine
crash = makeStandartTM []

check :: String -> TuringMachine
check = forCs $ \c -> makeStandartTM [(startState, c, C c, finalState)]

die :: String -> TuringMachine
die = forCs $ \c -> makeStandartTM [(startState, c, C c, startState)]

move :: Move -> String -> TuringMachine
move m = forCs $ \c -> makeStandartTM [(startState, c, M' m, finalState)]

moveInf :: Move -> String -> TuringMachine
moveInf m = forCs $ \c -> makeStandartTM [(startState, c, M' m, startState)]

rewrite :: String -> Char -> TuringMachine
rewrite cs c' = forCs (\c -> makeStandartTM [(startState, c, C c', finalState)]) cs

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
            let allChars :: [Maybe Char] = toList $ valuesSet a1 \/ valuesSet a2
                newA = fromList $ zip allChars [minBound..]
            forM_ (zip [_1, _2] [a1, a2]) $ \(tm, a) ->
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

rewriteAndMove :: String -> Char -> Move -> TuringMachine
rewriteAndMove s c m = rewrite s c ++> move m [c]
