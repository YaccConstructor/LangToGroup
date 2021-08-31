{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module SemigroupPresentation.Solver (
    Depth,
    Rule,
    solve,
    module SemigroupPresentation,
  ) where

import SemigroupPresentation

import Containers.Pair (swap)

import Data.List (find, sortBy)

type Depth = Maybe Int

data RuleType =
      Direct Int
    | Inverse Int
    deriving (Eq, Ord, Show)

type Rule = (GWord, GWord)

solve ::
    MonadFail m =>
    String ->
    SemigroupPresentation ->
    Depth ->
    String ->
    m [Rule]
solve strRes sp depth strIni = do
    let gd = sp^.generatorsDescr
    gwRes <- traverse (gd !?) $ words strRes
    gwIni <- traverse (gd !?) $ words strIni
    let rels = sp^.relations & toList
        rules = fromDistinctAscList $
            zip (Direct  <$> [0..]) (unPair <$>          rels) ++
            zip (Inverse <$> [0..]) (unPair <$> swap <$> rels)
        fs = firstStep rules gwIni
        gwsNext = map snd fs
    solve' gwRes (fromList $ gwIni +> gwsNext) rules depth fs

solve' ::
    MonadFail m =>
    GWord ->
    Set GWord ->
    Map RuleType Rule ->
    Depth ->
    [([RuleType], GWord)] ->
    m [Rule]
solve' _ _ _ _ [] =
    fail "Analysis depth limit reached"
solve' res rgws rs d gws =
    let (gwsOth, gwsNew)  = minStep rs d gws
    in  case find ((== res) . snd) gwsNew of
            Just (rts, _) ->
                traverse (rs !?) $ reverse rts
            Nothing -> do
                let gws' = gwsOth ++ filter ((`notMember` rgws) . snd) gwsNew
                    rgws' = rgws \/ fromList (snd <$> gwsNew)
                solve' res rgws' rs d gws'

firstStep :: Map RuleType Rule -> GWord -> [([RuleType], GWord)]
firstStep rs gw = do
    (rt, gw') <- step rs gw
    return ([rt], gw')

minStep ::
    Map RuleType Rule ->
    Depth ->
    [([RuleType], GWord)] ->
    ([([RuleType], GWord)], [([RuleType], GWord)])
minStep _ _ [] = ([], [])
minStep rs d gws =
    let ((rts, gw) : gws') = sortBy (compare `on` (length . snd)) gws
    in  (gws',
            case d of
                Just d' | length rts >= d' -> []
                _ -> do
                    (rt, gw') <- step rs gw
                    return (rt:rts, gw')
          )

step :: Map RuleType Rule -> GWord -> [(RuleType, GWord)]
step rs gw = do
    (rt, rule) <- toList rs
    gw' <- allRewrites rule gw
    return (rt, gw')

allRewrites :: Eq a => ([a], [a]) -> [a] -> [[a]]
allRewrites (from, to) ini = do
    i <- [0..length ini]
    let (s, f) = splitAt i ini
    f' <- from `prefixOf` f
    return $ s ++ to ++ f'

prefixOf :: (MonadFail m, Eq a) => [a] -> [a] -> m [a]
prefixOf [] s = return s
prefixOf _ [] = fail "Substring longer than string"
prefixOf (c1:t) (c2:s)
    | c1 == c2  = prefixOf t s
    | otherwise = fail "Substring is not prefix of string"
