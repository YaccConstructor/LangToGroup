{-# LANGUAGE TupleSections #-}

module SPSolver where

import SPTypes
import Data.List (inits, tails, isPrefixOf)

type Depth = Maybe Int

data RuleType =
      Direct Int
    | Inverse Int
    deriving (Eq, Show)

type PrevRule = Maybe RuleType

revRuleType :: RuleType -> RuleType
revRuleType (Direct x) = Inverse x
revRuleType (Inverse x) = Direct x

solve :: Depth -> [Relation] -> GWord -> [GWord]
solve d rs gw =
    let rules = map fmap [Direct, Inverse] <*> zip rs [0..]
    in  fmap fst $
        concat $
        maybe id take d $
        iterate (solveStep rules)
        [(gw, Nothing)]

solveStep ::
    [(Relation, RuleType)] ->
    [(GWord, PrevRule)] ->
    [(GWord, PrevRule)]
solveStep rules gws = do
    (gw, pr) <- gws
    rule <- filter ((/= pr) . Just . revRuleType . snd) rules
    let (gwFrom, gwTo) =
            case rule of
                (from `Equals` to, Direct _)  -> (from, to)
                (from `Equals` to, Inverse _) -> (to ,from)
    (, Just $ snd rule) <$> allRewrites gwFrom gwTo gw

allRewrites :: Eq a => [a] -> [a] -> [a] -> [[a]]
allRewrites from to ini =
    map (\(s, f) -> s ++ to ++ drop (length from) f) $
    filter (\(_, f) -> from `isPrefixOf` f) $
    zip (inits ini) (tails ini)
