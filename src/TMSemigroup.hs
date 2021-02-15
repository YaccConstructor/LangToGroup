{-# LANGUAGE LambdaCase #-}

module TMSemigroup where

import TMTypes
import TMReader
import BiTMReader
import qualified Data.Map.Lazy as Map

newtype TMConcat = TMC { unTMC :: TuringMachine }

runBiTMReaderTMC :: BiTMReader TuringMachine -> TMConcat -> TMConcat -> TMConcat
runBiTMReaderTMC btmr (TMC tm1) (TMC tm2) = TMC $ runBiTMReader btmr tm1 tm2

instance Semigroup TMConcat where
    (<>) = runBiTMReaderTMC $ do
        TM rs1 <- forTM1 getT
        TM rs2 <- forTM2 getT
        n1 <- forTM1 getN
        return $ TM $ Map.fromList $
            ( do
                ((q1, s), (sm, q2)) <- Map.toList rs1
                let newQ = \case
                     Q 0 -> Q (n1 + 1)
                     Q x -> Q x
                let q1' = newQ q1
                let q2' = newQ q2
                return ((q1', s), (sm, q2'))
            ) ++
            ( do
                ((q1, s), (sm, q2)) <- Map.toList rs2
                let newQ = \case
                     Q 0 -> Q 0
                     Q (-1) -> Q (-1)
                     Q x -> Q (x + n1)
                let q1' = newQ q1
                let q2' = newQ q2
                return ((q1', s), (sm, q2'))
            )

newtype TMUnion = TMU { unTMU :: TuringMachine }

runBiTMReaderTMU :: BiTMReader TuringMachine -> TMUnion -> TMUnion -> TMUnion
runBiTMReaderTMU btmr (TMU tm1) (TMU tm2) = TMU $ runBiTMReader btmr tm1 tm2

instance Semigroup TMUnion where
    (<>) = runBiTMReaderTMU $ do
        TM rs1 <- forTM1 getT
        TM rs2 <- forTM2 getT
        n1 <- forTM1 getN
        return $ TM $ Map.fromList $
            ( do
                ((q1, s), (sm, q2)) <- Map.toList rs2
                let newQ = \case
                     Q 1 -> Q 1
                     Q 0 -> Q 0
                     Q (-1) -> Q (-1)
                     Q x -> Q (x + n1 - 1)
                let q1' = newQ q1
                let q2' = newQ q2
                return ((q1', s), (sm, q2'))
            ) ++
            Map.toList rs1

(<@>) :: Maybe TuringMachine -> Maybe TuringMachine -> Maybe TuringMachine
Nothing  <@> _        = Nothing
Just tm1 <@> Nothing  = Just $ flip runTMReader tm1 $ do
    TM rs <- getT
    return $ TM $ Map.fromList
        ( do
            ((q1, s), (sm, q2)) <- Map.toList rs
            let newQ = \case
                 Q 0 -> Q 1
                 Q x -> Q x
            let q1' = newQ q1
            let q2' = newQ q2
            return ((q1', s), (sm, q2'))
        )
Just tm1 <@> Just tm2 = Just $ flip (uncurry . runBiTMReader) (tm1, tm2) $ do
    TM rs1 <- forTM1 getT
    TM rs2 <- forTM2 getT
    n1 <- forTM1 getN
    return $ TM $ Map.fromList $
        ( do
            ((q1, s), (sm, q2)) <- Map.toList rs2
            let newQ = \case
                 Q 1 -> Q 0
                 Q 0 -> Q 1
                 Q (-1) -> Q (-1)
                 Q x -> Q (x + n1 - 1)
            let q1' = newQ q1
            let q2' = newQ q2
            return ((q1', s), (sm, q2'))
        ) ++
        Map.toList rs1

newtype TMError = TME { unTME :: TuringMachine }

runBiTMReaderTME :: BiTMReader TuringMachine -> TMError -> TMError -> TMError
runBiTMReaderTME btmr (TME tm1) (TME tm2) = TME $ runBiTMReader btmr tm1 tm2

instance Semigroup TMError where
    (<>) = runBiTMReaderTME $ do
        TM rs1 <- forTM1 getT
        TM rs2 <- forTM2 getT
        n1 <- forTM1 getN
        return $ TM $ Map.fromList $
            ( do
                ((q1, s), (sm, q2)) <- Map.toList rs1
                let newQ = \case
                     Q (-1) -> Q (n1 + 1)
                     Q x -> Q x
                let q1' = newQ q1
                let q2' = newQ q2
                return ((q1', s), (sm, q2'))
            ) ++
            ( do
                ((q1, s), (sm, q2)) <- Map.toList rs2
                let newQ = \case
                     Q 0 -> Q 0
                     Q (-1) -> Q (-1)
                     Q x -> Q (x + n1)
                let q1' = newQ q1
                let q2' = newQ q2
                return ((q1', s), (sm, q2'))
            )
