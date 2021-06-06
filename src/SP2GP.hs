module SP2GP where

import TMTypes
import SPReader
import GPTypes
import GPGens
import SPTypes (SemigroupPresentation(..))
import qualified SPTypes as SP
import SPGens (isQ)
import qualified Set
import Control.Applicative (liftA2)

(^*) :: SP.GWord -> SPReader EWord
(^*) w = do
    gNotIsQ <- (not .) <$> fromTMReader isQ
    let (leftSWord, q : rightSWord) = span gNotIsQ w
    leftSWord' <- (map neg) <$> convertW leftSWord
    q' : rightSWord' <- convertW (q : rightSWord)
    return $ leftSWord' ++ (q' : rightSWord')

(===) :: [SPReader Element] -> [SPReader Element] -> SPReader Relation
w1 === w2 = Equals <$> sequence w1 <*> sequence w2

relations :: SPReader Relations
relations = do
    SP relationsSet <- getS
    m <- getM
    l <- getL
    let relationsList = Set.toList relationsSet
        ss = [s_ i | i <- [0..m]] ++ [h]
        rs = [r_ i | i <- [1..l]]
    fmap Set.fromList $ sequence $
        [
            [x, s] === [s, x, x]
            | s <- ss
        ] ++
        [
            [r, s] === [s, x, r, x]
            | r <- rs, s <- ss
        ] ++
        ( do
            (w1 `SP.Equals` w2, i) <- relationsList `zip` [0..]
            let asEWord = fmap (: [])
            let (<++>) = liftA2 (++)
            let w1' = asEWord (r_ i ^~) <++> (w1 ^*) <++> asEWord (r_ i)
            let w2' = (w2 ^*)
            return $
                Equals <$> w1' <*> w2'
        ) ++
        [
            [t, r] === [r, t]
            | r <- rs
        ] ++
        [
            [t, x] === [x, t]
        ] ++
        [
            [k, r] === [r, k]
            | r <- rs
        ] ++
        [
            [k, x] === [x, k]
        ] ++
        [
            [k, (h ^~), ((q_ 0) ^~), h, t, (h ^~), q_ 0, h] === [(h ^~), ((q_ 0) ^~), h, t, (h ^~), q_ 0, h, k]
        ]

groupBeta :: TuringMachine -> GroupPresentation
groupBeta = GP . runSPReader relations
