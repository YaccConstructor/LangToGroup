{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module TuringMachine.Quadruple (
    QuadrupleFromPart,
    QuadrupleToPart,
    TuringMachine.Quadruple.Quadruple,
    Quadruples,
    StrQuadruple,
    StrPair,
    toStrQ,
    Predicate,
    (&.),
    (|.),
    fromState,
    toState,
    fromSymbol,
    toSymbol,
    withMove,
    withoutMove,
    takeOnly,
    takeFromPart,
    takeToPart,
    withoutLoops,
    copy,
    module TuringMachine.State,
    module TuringMachine.SymbolOrMove,
  ) where

import TuringMachine.State
import TuringMachine.SymbolOrMove

import Containers
import Lens

infixr 3 &.
infixr 2 |.

type QuadrupleFromPart = (State, Symbol)

type QuadrupleToPart = (SymbolOrMove, State)

type Quadruple = (QuadrupleFromPart, QuadrupleToPart)

type Quadruples = Map QuadrupleFromPart QuadrupleToPart

type StrQuadruple = Containers.Quadruple String

type StrPair = Pair String

toStrQ :: TuringMachine.Quadruple.Quadruple -> StrQuadruple
toStrQ ((q1, s), (ms, q2)) =
    Quadruple (
        show $ numState q1,
        show $ numSymbol s,
        showSymbolOrMove ms,
        show $ numState q2
      )
  where
    showSymbolOrMove (M m') = show m'
    showSymbolOrMove (S s') = show $ numSymbol s'

type Predicate a = a -> Bool

type StrQuadruplePredicate = Predicate StrQuadruple

(&.) :: Predicate a -> Predicate a -> Predicate a
p1 &. p2 = \a -> p1 a && p2 a

(|.) :: Predicate a -> Predicate a -> Predicate a
p1 |. p2 = \a -> p1 a || p2 a

fromState :: State -> StrQuadruplePredicate
fromState s (Quadruple (s', _, _, _)) = show (numState s) == s'

toState :: State -> StrQuadruplePredicate
toState s (Quadruple (_, _, _, s')) = show (numState s) == s'

fromSymbol :: Symbol -> StrQuadruplePredicate
fromSymbol s (Quadruple (_, s', _, _)) = show (numSymbol s) == s'

toSymbol :: Symbol -> StrQuadruplePredicate
toSymbol s (Quadruple (_, _, s', _)) = show (numSymbol s) == s'

withMove :: Move -> StrQuadruplePredicate
withMove m (Quadruple (_, _, m', _)) = show m == m'

withoutMove :: StrQuadruplePredicate
withoutMove (Quadruple (_, _, m', _)) = m' `notElem` map show [toLeft,toRight]

takeOnly :: Predicate a -> Getter (Set a) (Set a)
takeOnly p = to $ (p <?>)

takeFromPart :: Getter (Set StrQuadruple) (Set StrPair)
takeFromPart = to $ gmap $ \(Containers.Quadruple (qf, s, _, _)) -> Pair (qf,s)

takeToPart :: Getter (Set StrQuadruple) (Set StrPair)
takeToPart = to $ gmap $ \(Containers.Quadruple (_, _, sm, qt)) -> Pair (sm,qt)

withoutLoops :: Getter (Set StrQuadruple) (Set StrQuadruple)
withoutLoops = to $ filterC isn'tLoop where
    isn'tLoop :: Predicate StrQuadruple
    isn'tLoop (Containers.Quadruple (qf, s, sm, qt)) = qf /= qt || s /= sm

copy ::
    (Listable c String, Indexable c Index String) =>
    Int -> Getter (Set c) (Set [String])
copy i = to $ gmap $ \c ->
    let a = c !? index i
        (l1, l2) = splitAt i $ toList c
    in  l1 ++ a ++ l2
