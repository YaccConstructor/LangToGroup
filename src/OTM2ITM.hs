module OTM2ITM where

import OTMReader
import XTMSymbol
import Move
import qualified TMType as OTM
import qualified TMTypes as ITM
import qualified TMSemigroup as ITMS
import qualified OTMTSet
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (join)

infixl 9 >+>, >|>, >@>

type TMModule = OTMReader (Maybe ITM.TuringMachine)

fromList :: [ITM.Quadruple] -> TMModule
fromList = return . Just . ITM.fromList

(>+>) :: TMModule -> TMModule -> TMModule
tmm1 >+> tmm2 = do
    mtm1 <- tmm1
    mtm2 <- tmm2
    return $ ITMS.unTMC <$>
        (ITMS.TMC <$> mtm1) <> (ITMS.TMC <$> mtm2)

(>|>) :: TMModule -> TMModule -> TMModule
tmm1 >|> tmm2 = do
    mtm1 <- tmm1
    mtm2 <- tmm2
    return $ ITMS.unTMU <$>
        (ITMS.TMU <$> mtm1) <> (ITMS.TMU <$> mtm2)

(>@>) :: TMModule -> TMModule -> TMModule
tmm1 >@> tmm2 = do
    mtm1 <- tmm1
    mtm2 <- tmm2
    return $ mtm1 ITMS.<@> mtm2

(>#>) :: TMModule -> TMModule -> TMModule
tmm1 >#> tmm2 = do
    mtm1 <- tmm1
    mtm2 <- tmm2
    return $ ITMS.unTME <$>
        (ITMS.TME <$> mtm1) <> (ITMS.TME <$> mtm2)

emptyTM :: TMModule
emptyTM = return Nothing

brokenTM :: TMModule
brokenTM = fromList []

liteDeadTM :: ITMSymbol -> TMModule
liteDeadTM itms =
    fromList [((ITM.startState, itms), (ITM.C itms, ITM.startState))]

liteMoveTM :: ITMSymbol -> Move -> TMModule
liteMoveTM itms m =
    fromList [((ITM.startState, itms), (toSymbolMove m, ITM.finalState))]

liteMoveInfTM :: ITMSymbol -> Move -> TMModule
liteMoveInfTM itms m =
    fromList [((ITM.startState, itms), (toSymbolMove m, ITM.startState))]

liteRewriteTM :: ITMSymbol -> ITMSymbol -> TMModule
liteRewriteTM itms itms' =
    fromList [((ITM.startState, itms), (ITM.C itms', ITM.finalState))]

liteErrorTM :: ITMSymbol -> TMModule
liteErrorTM itms =
    fromList [((ITM.startState, itms), (ITM.C itms, ITM.errorState))]

deadTM :: TMModule
deadTM =
    fromList =<< forAllPossibleITMS (\itms ->
        return [((ITM.startState, itms), (ITM.C itms, ITM.startState))]
      )

moveTM :: Move -> TMModule
moveTM m =
    fromList =<< forAllPossibleITMS (\itms ->
        return [((ITM.startState, itms), (toSymbolMove m, ITM.finalState))]
      )

moveInfTM :: Move -> TMModule
moveInfTM m =
    fromList =<< forAllPossibleITMS (\itms ->
        return [((ITM.startState, itms), (toSymbolMove m, ITM.startState))]
      )

rewriteTM :: (ITMSymbol -> OTMReader ITMSymbol) -> TMModule
rewriteTM f =
    fromList =<< forAllPossibleITMS (\itms -> do
        itms' <- f itms
        return [((ITM.startState, itms), (ITM.C itms', ITM.finalState))]
      )

moveUntilTM :: (ITMSymbol -> OTMReader Bool) -> Move -> TMModule
moveUntilTM p m =
    fromList =<< forAllPossibleITMS (\itms -> do
        res <- p itms
        return $
            if res
            then [((ITM.startState, itms), (toSymbolMove (versa m), ITM.finalState))]
            else [((ITM.startState, itms), (toSymbolMove m, ITM.startState))]
      )

shiftTM :: Move -> TMModule
shiftTM m =
    fromList =<< forAllPossibleITMS (\itmsV ->
        forAllPossibleITMS (\itmsS -> do
            mtmsV <- i2mTMS itmsV
            mtmsS <- i2mTMS itmsS
            let qF = ITM.Q (mtmsS * 2 + 1)
            let qT1 = ITM.Q (mtmsV * 2)
            let qT2 = ITM.Q (mtmsV * 2 + 1)
            return $
                ((qF, itmsV), (ITM.C itmsS, qT1)) : (
                    if qT1 /= ITM.finalState
                    then [((qT1, itmsS), (toSymbolMove m, qT2))]
                    else []
                  )
          )
      )

liteAllowTM :: ITMSymbol -> TMModule
liteAllowTM itms = liteRewriteTM itms itms

fillInfTM :: ITMSymbol -> Move -> TMModule
fillInfTM itmsF m =
    (rewriteTM (const $ return itmsF) >@> liteMoveTM itmsF m) >+> brokenTM

caseTM :: (ITMSymbol -> TMModule) -> TMModule
caseTM f =
    join $
        foldr (>|>) emptyTM <$>
            forAllPossibleITMS (\itms -> do
                let tmm = f itms
                mtm <- tmm
                return $ case mtm of
                     Nothing -> []
                     Just _  -> [tmm]
              )

liteSomeShiftTM :: ITMSymbol -> Move -> TMModule
liteSomeShiftTM itms m = do
    blank <- t2iTMS Nothing
    (
        (liteMoveTM blank m >|> moveInfTM (versa m)) >+>
        shiftTM m >+>
        liteMoveTM itms m
      ) >@> liteMoveTM blank (versa m)

tripleCaseTM :: OTMTSet.OTMTSet -> (OTMTriple -> TMModule) -> TMModule
tripleCaseTM otmts f =
    caseTM (\itms1 -> do
        ttms1 <- i2tTMS itms1
        case ttms1 of
             Nothing       -> emptyTM
             Just (Left _) -> emptyTM
             Just (Right square1)
                | all (`OTMTSet.notMember` otmts) [square1, OTM.ES] ->
                    liteErrorTM itms1
                | otherwise ->
                    liteMoveTM itms1 ToRight >+>
                    caseTM (\itms2 -> do
                        ttms2 <- i2tTMS itms2
                        case ttms2 of
                             Nothing        -> emptyTM
                             Just (Right _) -> emptyTM
                             Just (Left state)
                                | all (`OTMTSet.notMember` otmts) ((,) <$> [square1, OTM.ES] <*> [state]) ->
                                    liteErrorTM itms2
                                | otherwise ->
                                    liteMoveTM itms2 ToRight >+>
                                    caseTM (\itms3 -> do
                                        ttms3 <- i2tTMS itms3
                                        case ttms3 of
                                             Nothing       -> emptyTM
                                             Just (Left _) -> emptyTM
                                             Just (Right square2)
                                                | all (`OTMTSet.notMember` otmts) ((,,) <$> [square1, OTM.ES] <*> [state] <*> [square2, OTM.ES]) ->
                                                    liteErrorTM itms3
                                                | otherwise ->
                                                    f (square1, state, square2)
                                      )
                      )
      )

highlightForCopyTM :: TMModule -> TMModule -> TMModule
highlightForCopyTM tmRB tmLB = do
    blank <- t2iTMS Nothing
    (
        tmRB >+>
        shiftTM ToRight >+>
        (liteMoveTM blank ToLeft >|> moveInfTM ToLeft) >+>
        tmLB >+>
        shiftTM ToLeft >+>
        (liteMoveTM blank ToLeft >|> moveInfTM ToRight) >+>
        shiftTM ToLeft >+>
        (liteMoveTM blank ToRight >|> moveInfTM ToRight) >+>
        liteMoveTM blank ToRight >+>
        (liteMoveTM blank ToLeft >|> moveInfTM ToRight)
      )

copyTM :: Int -> TMModule
copyTM n | n > 0 = do
    blank <- t2iTMS Nothing
    (
        (
            (
                caseTM (\itms ->
                    if itms == blank then emptyTM else (
                        liteRewriteTM itms blank >+>
                        liteMoveTM blank ToRight >+>
                        liteRewriteTM blank itms >+>
                        liteMoveTM itms ToLeft >+>
                        liteMoveTM blank ToLeft >+>
                        (liteMoveTM blank ToLeft >|> moveInfTM ToLeft) >+>
                        (liteRewriteTM blank itms >|> moveInfTM ToLeft) >+>
                        liteMoveTM itms ToLeft
                      )
                  ) >@>
                (
                    shiftTM ToLeft >+>
                    (liteMoveTM blank ToRight >|> moveInfTM ToRight) >+>
                    (liteMoveTM blank ToRight >|> moveInfTM ToRight) >+>
                    (liteMoveTM blank ToLeft >|> moveInfTM ToRight)
                  )
              ) >+> brokenTM
          ) >|>
        (
            if n > 1
            then (
                liteMoveTM blank ToLeft >+>
                (liteMoveTM blank ToRight >|> moveInfTM ToLeft) >+>
                shiftTM ToRight >+>
                copyTM (n - 1)
              )
            else (
                liteMoveTM blank ToRight >+>
                liteMoveTM blank ToRight >+>
                (liteMoveTM blank ToLeft >|> moveInfTM ToRight) >+>
                shiftTM ToLeft >+>
                (liteMoveTM blank ToLeft >|> moveInfTM ToRight) >+>
                shiftTM ToLeft >+>
                (liteMoveTM blank ToLeft >|> moveInfTM ToLeft) >+>
                (liteMoveTM blank ToRight >|> moveInfTM ToLeft) >+>
                shiftTM ToRight >+>
                moveTM ToRight
              )
          )
      )
copyTM _ = emptyTM

updateOTMSquareTM :: Move -> OTM.Square -> OTM.Square -> TMModule
updateOTMSquareTM m oldotms newotms = do
    blank <- t2iTMS Nothing
    if oldotms == newotms
    then emptyTM
    else
        case (oldotms, newotms) of
             (OTM.ES, _) -> do
                newitms <- t2iTMS $ Just $ Right newotms
                (
                    shiftTM m >+>
                    (liteRewriteTM blank newitms >|> moveInfTM (versa m))
                  )
             (_, OTM.ES) -> do
                olditms <- t2iTMS $ Just $ Right oldotms
                (
                    liteRewriteTM olditms blank >+>
                    liteMoveTM blank m >+>
                    (liteMoveTM blank (versa m) >|> moveInfTM m) >+>
                    shiftTM (versa m)
                  )
             _ -> do
                newitms <- t2iTMS $ Just $ Right newotms
                olditms <- t2iTMS $ Just $ Right oldotms
                liteRewriteTM olditms newitms

updateOTMStateTM :: OTM.State -> OTM.State -> TMModule
updateOTMStateTM oldotms newotms = do
    olditms <- t2iTMS $ Just $ Left oldotms
    newitms <- t2iTMS $ Just $ Left newotms
    liteRewriteTM olditms newitms

updateOTMTripleTM :: Move -> OTMTriple -> OTMTriple -> TMModule
updateOTMTripleTM m (sq1, st, sq2) (sq1', st', sq2') = do
    ist' <- t2iTMS $ Just $ Left st'
    case m of
         ToRight ->
            updateOTMSquareTM ToLeft sq1 sq1' >+>
            moveTM ToRight >+>
            updateOTMStateTM st st' >+>
            liteMoveTM ist' ToRight >+>
            updateOTMSquareTM ToRight sq2 sq2' >+>
            moveTM ToRight
         ToLeft ->
            updateOTMSquareTM ToRight sq2 sq2' >+>
            moveTM ToLeft >+>
            updateOTMStateTM st st' >+>
            liteMoveTM ist' ToLeft >+>
            updateOTMSquareTM ToLeft sq1 sq1' >+>
            moveTM ToLeft

(~=) :: OTMTriple -> OTMTriple -> Bool
(OTM.ES, st1, OTM.ES) ~= (_   , st2, _   ) = st1 == st2
(OTM.ES, st1, sq21  ) ~= (_   , st2, sq22) = st1 == st2 && sq21 == sq22
(sq11  , st1, OTM.ES) ~= (sq12, st2, _   ) = st1 == st2 && sq11 == sq12
otmt1 ~= otmt2 = otmt1 == otmt2

eosovTM :: TMModule
eosovTM = do
    alpha <- t2iTMS $ Just (Right OTM.LBS)
    omega <- t2iTMS $ Just (Right OTM.RBS)
    blank <- t2iTMS Nothing
    (finalSt1, finalSt2) <- getFinalState
    commands <- getCommands
    let fstTriples = OTMTSet.fromList $ (fst <$>) $ Map.keys commands
    let sndTriples = \fstTriple ->
         OTMTSet.fromList $ (snd <$>) $ filter ((~= fstTriple) . fst) $ Map.keys commands
    (
        moveUntilTM itmsIsState ToRight >+>
        tripleCaseTM fstTriples (\fstTriple@(sq11, st1, sq21) ->
            (liteMoveTM omega ToRight >|> moveInfTM ToRight) >+>
            moveUntilTM itmsIsState ToRight >+>
            tripleCaseTM (sndTriples fstTriple) (\sndTriple@(sq12, st2, sq22) ->
                let possibleCommands = concat [
                     (\x -> (((sq11', st1, sq21'), (sq12', st2, sq22')), x)) <$> fromMaybe [] (Map.lookup ((sq11', st1, sq21'), (sq12', st2, sq22')) commands)
                     | sq11' <- [sq11, OTM.ES],
                       sq21' <- [sq21, OTM.ES],
                       sq12' <- [sq12, OTM.ES],
                       sq22' <- [sq22, OTM.ES]]
                in  case length possibleCommands of
                         0 -> error "possibleCommands can't be empty here"
                         1 ->
                            let [((oldFstT, oldSndT), (newFstT, newSndT))] = possibleCommands
                                (_, newSt1, _) = newFstT
                                (_, newSt2, _) = newSndT
                            in
                                updateOTMTripleTM ToLeft oldSndT newSndT >+>
                                moveUntilTM itmsIsState ToLeft >+>
                                updateOTMTripleTM ToLeft oldFstT newFstT >+>
                                (liteMoveTM omega ToRight >|> moveInfTM ToRight) >+>
                                (liteAllowTM omega >|> moveInfTM ToRight) >+>
                                (
                                    if newSt1 == finalSt1 && newSt2 == finalSt2
                                    then liteRewriteTM omega alpha
                                    else emptyTM
                                  )
                         n ->
                            (
                                highlightForCopyTM
                                    (liteMoveTM omega ToRight >|> moveInfTM ToRight)
                                    (
                                        (liteMoveTM alpha ToLeft >|> moveInfTM ToLeft) >+>
                                        (liteMoveTM alpha ToLeft >|> moveInfTM ToLeft)
                                      )
                              ) >+>
                            copyTM (n - 1) >+>
                            (
                                let helperTM [] = liteMoveTM alpha ToLeft >|> liteMoveTM blank ToLeft
                                    helperTM (((oldFstT, oldSndT), (newFstT, newSndT)) : otherCommands) =
                                        let (_, newSt1, _) = newFstT
                                            (_, newSt2, _) = newSndT
                                        in
                                            moveUntilTM itmsIsState ToRight >+>
                                            updateOTMTripleTM ToRight oldFstT newFstT >+>
                                            moveUntilTM itmsIsState ToRight >+>
                                            updateOTMTripleTM ToRight oldSndT newSndT >+>
                                            (liteAllowTM alpha >|> liteAllowTM blank >|> moveInfTM ToRight) >+>
                                            (
                                                if newSt1 == finalSt1 && newSt2 == finalSt2
                                                then (liteAllowTM alpha >|> liteRewriteTM blank alpha)
                                                else helperTM otherCommands
                                              )
                                in  helperTM possibleCommands
                              )
              ) >#> ((liteMoveTM alpha ToLeft >|> moveInfTM ToLeft) >+> liteErrorTM omega)
          ) >#> (
            (liteMoveTM alpha ToLeft >|> moveInfTM ToLeft) >+>
            (
                (
                    liteMoveTM blank ToRight >+>
                    (
                        liteRewriteTM omega blank >|>
                        fillInfTM blank ToRight
                      ) >+>
                    liteMoveTM blank ToRight >+>
                    (
                        liteRewriteTM omega blank >|>
                        fillInfTM blank ToRight
                      ) >+>
                    liteMoveTM blank ToRight >+>
                    (
                        liteMoveTM alpha ToLeft >|>
                        liteDeadTM blank
                      )
                  ) >|> (
                    liteMoveTM omega ToRight >+>
                    (
                        liteRewriteTM omega blank >|>
                        fillInfTM blank ToRight
                      ) >+>
                    liteMoveTM blank ToRight >+>
                    (
                        liteRewriteTM omega blank >|>
                        fillInfTM blank ToRight
                      ) >+>
                    liteMoveTM blank ToRight >+>
                    (
                        (
                            liteSomeShiftTM alpha ToLeft >+>
                            liteAllowTM omega
                          ) >|> (
                            liteMoveInfTM blank ToLeft >|>
                            liteAllowTM omega
                          )
                      )
                  )
              )
          )
      )

eosmvTM :: TMModule
eosmvTM = do
    alpha <- t2iTMS $ Just (Right OTM.LBS)
    omega <- t2iTMS $ Just (Right OTM.RBS)
    blank <- t2iTMS Nothing
    (
        eosovTM >+>
        (liteMoveTM omega ToRight >|> liteMoveTM blank ToRight >|> liteErrorTM alpha)
      ) >@> liteAllowTM alpha

emsTM :: TMModule
emsTM = do
    alpha <- t2iTMS $ Just (Right OTM.LBS)
    blank <- t2iTMS Nothing
    (
        (
            eosmvTM >@> (
                liteMoveTM blank ToLeft >+> (
                    liteMoveTM blank ToRight >|>
                    moveInfTM ToLeft
                  )
              )
          ) >+> brokenTM
      ) >#> liteAllowTM alpha

otm2itm :: OTM.TM -> ITM.TuringMachine
otm2itm = fromJust . runOTMReader emsTM
