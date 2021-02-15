module GPGenOrds where

import TMTypes
import SPReader
import GPGens
import GPTypes
import KnuthBendix (Order)
import qualified Data.Map.Lazy as Map
import Data.List (permutations)
import Control.Applicative ((<**>))

idOrRev :: [a] -> [[a]]
idOrRev xs = fmap ($ xs) [id, reverse]

orders :: TuringMachine -> [Order Element]
orders tm = do
    let n = runSPReader getN tm
        m = runSPReader getM tm
        l = runSPReader getL tm
    qs <- idOrRev [ q_ i | i <- [0..n] ]
    ss <- idOrRev [ s_ i | i <- [0..m] ]
    rs <- idOrRev [ r_ i | i <- [0..l] ]
    posElems <- flip runSPReader tm . sequence . concat <$> permutations
        [
            [q],
            qs,
            ss,
            rs,
            [x],
            [t],
            [k]
        ]
    fs <- idOrRev [id, neg]
    allElems <- [fs <*> posElems, posElems <**> fs]
    return $ toOrder $ Map.fromList $ zip allElems [0..]

toOrder :: Ord a => Map.Map a Int -> Order a
toOrder m el y = compare (Map.lookup el m) (Map.lookup y m)
