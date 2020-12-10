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
    fmap Set.fromList $ sequence $
        [
            [x, s_ beta] === [s_ beta, x, x]
            | beta <- [0..m]
        ] ++
        [
            [r_ i, s_ beta] === [s_ beta, x, r_ i, x]
            | i <- [0..l], beta <- [0..m]
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
            [t, r_ i] === [r_ i, t]
            | i <- [0..l]
        ] ++
        [
            [t, x] === [x, t]
        ] ++
        [
            [k, r_ i] === [r_ i, k]
            | i <- [0..l]
        ] ++
        [
            [k, x] === [x, k]
        ] ++
        [
            [k, (q ^~), t, q] === [(q ^~), t, q, k]
        ]

groupBeta :: TuringMachine -> GroupPresentation
groupBeta = GP . runSPReader relations
