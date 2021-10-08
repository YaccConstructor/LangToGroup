{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module SP2GP.Relations (
    Rels,
    (===),
    for',
    in',
    rels,
  ) where

import qualified SemigroupPresentation as SP
import qualified GroupPresentation as GP
import Format
import Containers
import Lens

import qualified Control.Monad.Reader as R
import qualified Control.Monad.List as LT
import Control.Monad (guard, forM, forM_)
import Control.Monad.Trans (lift)
import Control.Lens (each)
import Data.List (find)
import Data.Foldable (fold)
import Data.Semigroup (stimes)

type Rels = [Rel]

data Rel =
      JustRel String String
    | forall v c. RangeClass v c => ForBlock (Range v c) Rels

data Range v c = Range v c

type SetGetter a = SP.SPGetting (Set a)

(===) :: String -> String -> Rel
(===) = JustRel

for' :: RangeClass v c => Range v c -> Rels -> Rel
for' = ForBlock

in' :: RangeClass v c => v -> c -> Range v c
in' = Range

type LocalReader =
    R.ReaderT ReplaceRules (
        R.ReaderT GP.GeneratorsDescr (
            R.ReaderT SP.SemigroupPresentation
                Maybe
          )
      )

liftSP :: R.ReaderT SP.SemigroupPresentation Maybe a -> LocalReader a
liftSP = lift . lift

type ReplaceRules = [ReplaceRule]

type ReplaceRule = ([String], [[GP.GWord]])

consistRR :: String -> ReplaceRule -> Bool
consistRR g rr = g `elem` fst rr

type ReplaceAtom = (String, GP.GWord)

consistRA :: String -> ReplaceAtom -> Bool
consistRA g ra = fst ra == g

rels ::
    MonadFail m =>
    Rels ->
    GP.GeneratorsDescr ->
    SP.SemigroupPresentation ->
    m GP.Relations
rels =
    ((maybe (fail "Can't create relations") return .) .) .
    (R.runReaderT .) .
    R.runReaderT .
    flip R.runReaderT [] .
    rels_

rels_ :: Rels -> LocalReader GP.Relations
rels_ rs =
    fromList . concat <$> traverse rel_ rs

rel_ :: Rel -> LocalReader [GP.Relation]
rel_ (JustRel ew1 ew2) = LT.runListT $ do
    let es1 = words ew1
        es2 = words ew2
    (ews1, srrs) <- LT.ListT $ elems_ es1 []
    (ews2, _)    <- LT.ListT $ elems_ es2 srrs
    return $ GP.relation ews1 ews2
rel_ (ForBlock rg rs) = do
    newRule <- range_ rg
    R.local (newRule:) $ toList <$> rels_ rs

class RangeClass v c where
    range_ :: Range v c -> LocalReader ReplaceRule

instance RangeClass String (SetGetter String) where
    range_ (Range str strGetter) = do
        let fs' = words str
        guard $ length fs' == 1
        let f' = head fs'
        f :: TaggedFormat <- format f'
        strSet <- liftSP $ view strGetter
        let strList = toList strSet
        gs <- traverse gen_ $ apply f <$> strList
        return ([format2str f], (pure.pure) <$> gs)

instance RangeClass Rel (SetGetter SP.StrRelation) where
    range_ (Range (ForBlock _ _) _) =
        fail "ForBlock can't be used into Range definition"
    range_ (Range (JustRel gw1 gw2) relGetter) = do
        let pgw = Pair (gw1, gw2)
        pfs :: Pair [MaybeTaggedFormat] <-
            traverse (traverse format . words) pgw
        forM_ pfs $ \fs ->
            guard $
                all (\(f1, f2) -> isFormat f1 || isFormat f2) $
                    zip fs (tail fs)
        relSet <- liftSP $ view relGetter
        let relList = toList relSet
        gws <- forM relList $ \rel ->
            (\f -> fmap fold $ sequence $ f <$> pfs <*> rel) $ \fs' gs' ->
                go [] fs' gs'
        let fs = fold $ (fmap.map) format2str pfs
        return (fs, gws)
      where
        go ::
            [String] ->
            [MaybeTaggedFormat] ->
            [String] ->
            LocalReader [GP.GWord]
        go _ [] [] = return []
        go _ [JustString _] gs = do
            gw <- traverse gen_ gs
            return [gw]
        go acc fs@((JustString _):(JustFormat f):fs_) (g:gs)
            | match f g = do
                gw1' <- traverse gen_ $ reverse acc
                gw2' <- pure <$> gen_ g
                (gw1':) <$> (gw2':) <$> go [] fs_ gs
            | otherwise =
                go (g:acc) fs gs
        go _ ((JustFormat f):fs) (g:gs) = do
            guard $ match f g
            gw <- pure <$> gen_ g
            (gw:) <$> go [] fs gs
        go _ _ _ = fail "Something goes wrong"

instance (RangeClass v1 c1, RangeClass v2 c2) => RangeClass (v1, v2) (c1, c2) where
    range_ (Range (v1, v2) (c1, c2)) = do
        (gs1, gwss1) <- range_ (Range v1 c1)
        (gs2, gwss2) <- range_ (Range v2 c2)
        return $ (gs1 ++ gs2, zipWith (++) gwss1 gwss2)

elems_ ::
    [String] ->
    [ReplaceAtom] ->
    LocalReader [(GP.EWord, [ReplaceAtom])]
elems_ [] replaceAtoms = return [([], replaceAtoms)]
elems_ allEs@(e:es) replaceAtoms = do
    let (g, p) = elem2gen e
    case find (consistRA g) replaceAtoms of
        Just (_, gw) -> do
            let ew = powGWord gw p
            add ew <$> elems_ es replaceAtoms
        Nothing -> do
            replaceRules <- R.ask
            case find (consistRR g) replaceRules of
                Nothing -> do
                    ew <- elem_ (g, p)
                    add ew <$> elems_ es replaceAtoms
                Just (gs, gwss) ->
                    fmap concat $
                        traverse (elems_ allEs) $
                            [ zip gs gws ++ replaceAtoms
                            | gws <- gwss
                            ]
      where
        add ew = each._1 %~ (ew ++)

data GenPow =
      JustPow Int
    | Sharp
    deriving Show

elem2gen :: String -> (String, GenPow)
elem2gen s =
    case span (/= '^') s of
        (g, [])   -> (g, JustPow 1)
        (g, "^#") -> (g, Sharp)
        (g, _:p)  -> (g, JustPow $ read p)

powGWord :: GP.GWord -> GenPow -> GP.EWord
powGWord gw (JustPow p)
    | p > 0 = stimes p $ GP.Positive <$> gw
    | p == 0 = []
    | otherwise = stimes (-p) $ GP.Negative <$> reverse gw
powGWord gw Sharp = GP.Negative <$> gw

elem_ :: (String, GenPow) -> LocalReader GP.EWord
elem_ (g, p) = do
    g' <- gen_ g
    return $ powGWord [g'] p

gen_ :: String -> LocalReader GP.Generator
gen_ g = do
    gsd <- lift R.ask
    gsd !? g
