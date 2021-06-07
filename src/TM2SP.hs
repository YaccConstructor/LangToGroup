module TM2SP where

import TMTypes
import TMReader
import SPTypes
import SPGens
import qualified Set
import qualified Data.Set as RealSet
import qualified Data.Map.Lazy as Map
import Control.Monad (guard)

setsOfSymbols :: TMReader [(State, [Symbol])]
setsOfSymbols = do
    TM qs' <- getT
    m <- getM
    let qs = Map.toList qs'
        qsL = filter ((== L) . fst . snd) qs
        allSymbols = RealSet.fromList $ S <$> [0..m]
        q1s1s = Map.fromList $ map (\(q, s) -> (q, RealSet.singleton s)) $ map fst qsL
        q2s = Map.fromSet (const allSymbols) $ RealSet.fromList $ map (snd . snd) qsL
        qss = Map.unionWith RealSet.union q1s1s q2s
    return $ map (\(q, ss) -> (q, RealSet.toList ss)) $ Map.toList $ qss

(===) :: [TMReader Generator] -> [TMReader Generator] -> TMReader Relation
w1 === w2 = Equals <$> sequence w1 <*> sequence w2

relations :: TMReader Relations
relations = do
    n <- getN
    m <- getM
    TM quadruplesMap <- getT
    let quadruples = filter (\((q1, s1), (sm, q2)) -> q1 /= q2 || sm /= C s1) $ Map.toList quadruplesMap
    sos <- setsOfSymbols
    fmap Set.fromList $ sequence $
        ( do
            quadruple <- quadruples
            case quadruple of
                ((Q i, S j), (C (S k), Q l)) ->
                    [[q_ i, s_ j] === [q_ l, s_ k]] ++ (
                        if j == 0
                        then [
                            [q_ i, h] === [q_ l, s_ k, h]
                          ]
                        else []
                    )

                ((Q i, S j), (R,       Q l)) ->
                    [[q_ i, s_ j] === [s_ j, q_ l]] ++ (
                        if j == 0
                        then [
                            [q_ i, h] === [s_ 0, q_ l, h]
                          ]
                        else []
                    )

                ((Q i, S j), (L,       Q l)) ->
                    [[s_ j, p_ i] === [p_ l, s_ j]] ++ (
                        if j == 0
                        then [
                            [h, p_ i] === [h, p_ l, s_ 0]
                          ]
                        else []
                    )
        ) ++
        [
            [q_ alpha, s_ beta] === [s_ beta, p_ alpha]
          | (q, ss) <- sos,
            s <- ss,
            let Q alpha = q,
            let S beta = s
        ] ++
        ( do
            (q, ss) <- sos
            s <- ss
            guard $ s == S 0
            let Q alpha = q
            [
                [q_ alpha, h] === [s_ 0, p_ alpha, h],
                [h, p_ alpha] === [h, q_ alpha, s_ 0]
              ]
        ) ++
        [[q_ 0, s_ beta]    === [q_ 0]    | beta <- [0..m]] ++
        [[s_ beta, q_ 0, h] === [q_ 0, h] | beta <- [0..m]]

semigroupGamma :: TuringMachine -> SemigroupPresentation
semigroupGamma = SP . runTMReader relations
