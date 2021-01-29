module TM2SP where

import TMTypes
import TMReader
import SPTypes
import SPGens
import qualified Set
import qualified Data.Map.Lazy as Map

(===) :: [TMReader Generator] -> [TMReader Generator] -> TMReader Relation
w1 === w2 = Equals <$> sequence w1 <*> sequence w2

relations :: TMReader Relations
relations = do
    m <- getM
    TM quadruplesMap <- getT
    let quadruples = Map.toList quadruplesMap
    fmap Set.fromList $ sequence $
        ( do
            quadruple <- quadruples
            case quadruple of
                ((Q i, S j), (C (S k), Q l)) ->
                    [[q_ i, s_ j] === [q_ l, s_ k]]

                ((Q i, S j), (R,       Q l)) ->
                    [
                        [q_ i, s_ j, s_ beta] === [s_ j, q_ l, s_ beta]
                        | beta <- [0..m]
                    ] ++
                    [[q_ i, s_ j, h] === [s_ j, q_ l, s_ 0, h]]

                ((Q i, S j), (L,       Q l)) ->
                    [
                        [s_ beta, q_ i, s_ j] === [q_ l, s_ beta, s_ j]
                        | beta <- [0..m]
                    ] ++
                    [[h, q_ i, s_ j] === [h, q_ l, s_ 0, s_ j]]
        ) ++
        [[q_ 0, s_ beta]    === [q_ 0]    | beta <- [0..m]] ++
        [[s_ beta, q_ 0, h] === [q_ 0, h] | beta <- [0..m]] ++
        [[h, q_ 0, h]       === [q]]

semigroupGamma :: TuringMachine -> SemigroupPresentation
semigroupGamma = SP . runTMReader relations
