{-# LANGUAGE TemplateHaskell, TupleSections, RankNTypes, TypeApplications #-}

module TuringMachine (
    LabeledStates,
    Alphabet,
    TuringMachine,
    turingMachine,
    quadruples,
    labeledStates,
    alphabet,
    TMGetter,
    TMSetter,
    states,
    symbols,
    allStates,
    allSymbols,
    strQuadruples,
    strLabeledStates,
    strSymbols,
    strNumStates,
    strNumSymbols,
    maxNumState,
    maxNumSymbol,
    module TuringMachine.Quadruple,
    module Containers,
    module Lens,
  ) where

import TuringMachine.Quadruple
import Containers hiding (Quadruple)
import Lens

import Control.Applicative (liftA2, liftA3)
import Data.Maybe (catMaybes)
import GHC.Generics ((:.:)(Comp1, unComp1))

type LabeledStates = PrismMap String State

type Alphabet = IsoMap (Maybe Char) Symbol

data TuringMachine = TM
  { _quadruples :: Quadruples
  , _labeledStates :: LabeledStates
  , _alphabet :: Alphabet
  } deriving (Eq)

turingMachine :: Quadruples -> LabeledStates -> Alphabet -> TuringMachine
turingMachine = TM

makeLenses ''TuringMachine

maybe' :: (a -> Maybe b) -> (a -> b) -> a -> b
maybe' f g x =
    case f x of
        Nothing -> g x
        Just y  -> y

instance Show TuringMachine where
    show tm =
        flip concatMap (tm^.quadruples.to toList) $
            \((q1, s), (sm, q2)) ->
                let q1' =
                        maybe'
                            ((tm^.labeledStates) !?)
                            (show . numState)
                            q1
                    s'  = pure $
                        maybe'
                            (
                                maybe'
                                    ((tm^.alphabet) !?)
                                    (const $ Just '?')
                              )
                            (const blankChar)
                            s
                    sm' = case sm of
                        S s_ -> pure $
                            maybe'
                                (
                                    maybe'
                                        ((tm^.alphabet) !?)
                                        (const $ Just '?')
                                  )
                                (const blankChar)
                                s_
                        M m  -> show m
                    q2' =
                        maybe'
                            ((tm^.labeledStates) !?)
                            (show . numState)
                            q2
                in  replicate (4 - length q1') ' ' ++ q1' ++ " " ++
                    s'  ++ replicate (2 - length s')  ' ' ++
                    sm' ++ replicate (3 - length sm') ' ' ++
                    q2' ++ "\n"

type TMGetter a = Getter TuringMachine a

type TMSetter a b = Setter TuringMachine TuringMachine a b

traverseMaybe :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
traverseMaybe f = fmap catMaybes . traverse f

states :: TMSetter State (Maybe State)
states f tm =
    liftA3 turingMachine
        (tm^.quadruples    & fmap fromList . traverseMaybe updQ  . toList)
        (tm^.labeledStates & fmap fromList . traverseMaybe updLS . toList)
        (tm^.alphabet      & pure)
          where
            f' = Comp1 <$> f
            updQ ((qf, s), (sm, qt)) = 
                unComp1 $ liftA2 (,) ((,s) <$> f' qf) ((sm,) <$> f' qt)
            updLS = unComp1 . traverse f'

symbols :: TMSetter Symbol (Maybe Symbol)
symbols f tm =
    liftA3 turingMachine
        (tm^.quadruples    & fmap fromList . traverseMaybe updQ . toList)
        (tm^.labeledStates & pure)
        (tm^.alphabet      & fmap fromList . traverseMaybe updA . toList)
      where
        f' = Comp1 <$> f
        updQ ((qf, s), (m@(M _), qt)) =
            unComp1 $ (,(m, qt)) <$> (qf,) <$> f' s
        updQ ((qf, s), (S s', qt)) =
            unComp1 $ liftA2 (,) ((qf,) <$> f' s) ((,qt) <$> S <$> f' s')
        updA = unComp1 . traverse f'

allStates :: TMGetter (Set State)
allStates = to $ do
    qs <- view quadruples
    ls <- view labeledStates
    let statesFromLS = valuesSet ls
        statesFromQ ((qf, _), (_, qt)) = fromList [qf, qt]
    return $ foldr (\/) statesFromLS $ statesFromQ <$> toList qs

allSymbols :: TMGetter (Set Symbol)
allSymbols = to $ do
    qs <- view quadruples
    a  <- view alphabet
    let symbolsFromA = valuesSet a
        symbolsFromQ ((_, s), (sm, _)) =
            fromList @(Set Symbol) $
                case sm of
                    M _  -> [s]
                    S s' -> [s, s']
    return $ foldr (\/) symbolsFromA $ symbolsFromQ <$> toList qs

strQuadruples :: TMGetter (Set StrQuadruple)
strQuadruples = quadruples . to (fromList . map toStrQ . toList)

strLabeledStates :: TMGetter (Set String)
strLabeledStates = labeledStates . to keysSet

strSymbols :: TMGetter (Set String)
strSymbols = alphabet . to (gmap (maybe [blankChar] pure) . valuesSet)

strNumStates :: TMGetter (Set String)
strNumStates = allStates . to (gmap (show . numState))

strNumSymbols :: TMGetter (Set String)
strNumSymbols = allSymbols . to (gmap (show . numSymbol))

maxNumState :: TMGetter Int
maxNumState = quadruples . to (maximum . map maxN . toList) where
    maxN ((qs, _), (_, qf)) = (max `on` numState) qs qf

maxNumSymbol :: TMGetter Int
maxNumSymbol = quadruples . to (maximum . map maxM . toList) where
    maxM ((_, s), (M _, _)) = numSymbol s
    maxM ((_, s), (S s', _)) = (max `on` numSymbol) s s'
