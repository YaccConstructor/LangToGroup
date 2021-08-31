{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, ExistentialQuantification, FlexibleContexts, UndecidableInstances, DefaultSignatures #-}

module TM2SP.Relations (
    Rels,
    (===),
    for',
    in',
    rels,
  ) where

import qualified TuringMachine as TM
import qualified SemigroupPresentation as SP
import Containers
import Lens
import Format

import qualified Control.Monad.Reader as R
import qualified Control.Monad.List as LT
import Control.Monad.Trans (lift)
import Control.Monad (when)
import Control.Lens (each)
import Data.Maybe (catMaybes)
import Data.List (find)

type Rels = [Rel]

data Rel =
    JustRel String String
  | forall e. RangeElement e => ForBlock (Range e) Rels

data Range e = Range String (SetGetter e)

type SetGetter a = TM.TMGetter (Set a)

(===) :: String -> String -> Rel
(===) = JustRel

for' :: RangeElement e => Range e -> Rels -> Rel
for' = ForBlock

in' :: RangeElement e => String -> SetGetter e -> Range e
in' = Range

type LocalReader a =
    forall m. MonadFail m =>
    R.ReaderT ReplaceRules (
        R.ReaderT SP.GeneratorsDescr (
            R.ReaderT TM.TuringMachine
                m
          )
      )
      a

liftTM :: MonadFail m => R.ReaderT TM.TuringMachine m a -> R.ReaderT ReplaceRules (R.ReaderT SP.GeneratorsDescr (R.ReaderT TM.TuringMachine m)) a
liftTM = lift . lift

type ReplaceRules = [ReplaceRule]

type ReplaceRule = ([String], [[SP.Generator]])

consistRR :: String -> ReplaceRule -> Bool
consistRR g rr = g `elem` fst rr

type ReplaceAtom = (String, SP.Generator)

consistRA :: String -> ReplaceAtom -> Bool
consistRA g ra = fst ra == g

rels ::
    MonadFail m =>
    Rels ->
    SP.GeneratorsDescr ->
    TM.TuringMachine ->
    m SP.Relations
rels =
    (R.runReaderT .) .
    R.runReaderT .
    flip R.runReaderT [] .
    rels_

rels_ :: Rels -> LocalReader SP.Relations
rels_ rs =
    fromList . concat <$> traverse rel_ rs

rel_ :: Rel -> LocalReader [SP.Relation]
rel_ (JustRel gw1 gw2) = LT.runListT $ do
    let gs1 = words gw1
        gs2 = words gw2
    (gws1, srrs) <- LT.ListT $ gens_ gs1 []
    (gws2, _)    <- LT.ListT $ gens_ gs2 srrs
    return $ SP.relation gws1 gws2
rel_ (ForBlock rg rs) = do
    newRule <- range_ rg
    R.local (newRule:) $ toList <$> rels_ rs

class RangeElement e where
    dimension :: Set e -> Int
    range_ :: Range e -> LocalReader ReplaceRule
    default range_ ::
        (Sizable e, Listable e String, Ord e) =>
        Range e -> LocalReader ReplaceRule
    range_ (Range str cStrGetter) = do
        cStrSet <- liftTM $ view cStrGetter
        let fs' = words str
        when (length fs' > dimension cStrSet) $
            fail
                "Amount of formats does not equal to dimension of received data"
        let mfs :: [Maybe TaggedFormat] = format <$> fs'
        let strsList = toList <$> toList cStrSet
            gs' = catMaybes <$> zipWith (traverse apply) mfs <$> strsList
        gs <- (traverse.traverse) gen_ gs'
        return (format2str <$> catMaybes mfs, gs)

instance RangeElement String where
    dimension = const 1
    range_ (Range str strGetter) = do
        let fs' = words str
        when (length fs' /= 1) $
            fail
                "Amount of formats does not equal to dimension of received data"
        let f' = head fs'
        f :: TaggedFormat <- format f'
        strSet <- liftTM $ view strGetter
        let strList = toList strSet
        gs <- traverse gen_ $ apply f <$> strList
        return ([format2str f], pure <$> gs)

instance RangeElement (Quadruple String) where
    dimension = const 4

instance RangeElement [String] where
    dimension = foldr (min . length) 42

gens_ ::
    [String] ->
    [ReplaceAtom] ->
    LocalReader [(SP.GWord, [ReplaceAtom])]
gens_ [] replaceAtoms = return [([], replaceAtoms)]
gens_ allGs@(g:gs) replaceAtoms =
    case find (consistRA g) replaceAtoms of
        Just (_, gen) ->
            add gen <$> gens_ gs replaceAtoms
        Nothing -> do
            replaceRules <- R.ask
            case find (consistRR g) replaceRules of
                Nothing -> do
                    gen <- gen_ g
                    add gen <$> gens_ gs replaceAtoms
                Just (gs', genss) ->
                    fmap concat $
                        traverse (gens_ allGs) $
                            [ zip gs' gens ++ replaceAtoms
                            | gens <- genss
                            ]
      where
        add gen = each._1 %~ (gen :)

gen_ :: String -> LocalReader SP.Generator
gen_ g = do
    gsd <- lift R.ask
    gsd !? g
