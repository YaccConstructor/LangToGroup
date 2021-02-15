module GPKnuthBendix where

import TMTypes
import TMTesting
import SPReader
import GPTypes
import GPGens
import GPGenOrds
import SP2GP
import qualified Set
import KnuthBendix (knuthBendixBy, Order)
import System.Timeout (timeout)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering) )
import Control.Exception (evaluate)
import Control.Monad (guard)
import System.Random.Shuffle.FisherYates (shuffle)
import Control.Monad.List (ListT(..))
import Control.Monad.IO.Class (liftIO)

data TimeParam =
      TimePerTestAndPartOfOrds Int Double
    | TimePerTestAndTotalTime  Int Int
    | PartOfOrdsAndTotalTime   Double Int

data TestInfo = TestInfo {
    tmIndices :: [Int],
    timeParam :: TimeParam
  }

asList :: Monad m => [a] -> ListT m a
asList = ListT . return

test :: TestInfo -> IO [(Int, Int)]
test (TestInfo tmis' tp) =
    runListT $ do
        let tmis = if null tmis' then [1 .. length testingSet] else tmis'
        tmi <- asList tmis
        let tm = testingTM tmi
        allOrdersForTM <- liftIO $ shuffle $ zip [0..] $ orders tm
        let calcCountOfOrdersForTM =
                \partOfOrds ->
                    length allOrdersForTM `min` floor (
                        partOfOrds * fromIntegral (length allOrdersForTM)
                      )
            timePerTest =
                case tp of
                    TimePerTestAndPartOfOrds time _ -> time * 1000000
                    TimePerTestAndTotalTime  time _ -> time * 1000000
                    PartOfOrdsAndTotalTime   p time ->
                        (time * 1000000 `div` length tmis) `div` calcCountOfOrdersForTM p
            countOfOrdersForTM =
                case tp of
                    TimePerTestAndPartOfOrds _ p ->
                        calcCountOfOrdersForTM p
                    TimePerTestAndTotalTime t' time ->
                        length allOrdersForTM `min` ((time `div` length tmis) `div` t')
                    PartOfOrdsAndTotalTime   p _ ->
                        calcCountOfOrdersForTM p
            ordersForTM = take countOfOrdersForTM allOrdersForTM
        (ordi, ord) <- asList ordersForTM
        res <- liftIO $ timeout timePerTest $ testTM tm ord
        guard $ res == Just ()
        liftIO $ putStrLn $ show tmi ++ " " ++ show ordi
        return (tmi, ordi)

testTM :: TuringMachine -> Order Element -> IO ()
testTM tm ord =
    let maxGi =
            flip runSPReader tm $ ((+ 6) . sum) <$> sequence [getN, getM, getL]
        GP rs = groupBeta tm
        rules =
            map (\(ew1 `Equals` ew2) -> (ew1, ew2)) (Set.unSet rs) ++
            [
                ([a, neg a], [])
            |
                i <- [0..maxGi],
                a <- [Positive, Negative] <*> [G i]
            ]
    in  do
            hSetBuffering stdout NoBuffering
            _ <- evaluate (knuthBendixBy ord rules)
            return ()
