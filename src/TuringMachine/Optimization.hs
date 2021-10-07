{-# LANGUAGE ScopedTypeVariables, TypeApplications, TupleSections #-}

-- |Module `TuringMachine.Optimization` include functions for reducing size of
--  Turing machine with saving of functionality.
module TuringMachine.Optimization (
    optimize,
    optimal,
    ifCanOptimize,
    safeOptimize,
    safeOptimal,
    ifCanSafeOptimize,
    DiffTM,
    diffTM,
    module TuringMachine.Optimization.Level,
    module TuringMachine.Optimization.Safe,
    module TuringMachine,
  ) where

import TuringMachine.Optimization.Level
import TuringMachine.Optimization.Safe
import TuringMachine

import TuringMachine.Constructors
import Containers.Set (findMin, deleteMin)
import Containers.Map (fromListWith, mapWithKey)

import qualified Control.Monad.State.Lazy as ST
import Control.Monad.State.Lazy (runState, execStateT, get, put)
import Control.Monad.Trans (lift)
import Control.Monad (when)
import Data.Tuple (swap)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Containers.ListUtils (nubOrd)

type TMConverting = ST.StateT TuringMachine (ST.State IsChanged) ()

type IsChanged = Bool

isChanged :: TMConverting
isChanged = lift $ put True

isn'tChanged :: TMConverting
isn'tChanged = lift $ put False

ifIsChanged :: TMConverting -> TMConverting
ifIsChanged tmConv = do
    changed <- lift get
    when changed
        tmConv

infinityConverting :: TMConverting -> TMConverting
infinityConverting tmConv = do
    isn'tChanged
    tmConv
    ifIsChanged $ do
        infinityConverting tmConv
        isChanged

removeUselessQuadruples :: TMConverting
removeUselessQuadruples = do
    qs <- use quadruples
    allQs <- use allStates
    let qsWithM =
            withAnyMove <?> qs
        statesToM =
            valuesSet $ snd <$> qsWithM
        statesToS =
            allQs \\ statesToM \\ fromList [startState, finalState]
        loops =
            isLoop <?> qs
        qsToStatesToS =
            (stateToFrom statesToS <?> qs) \\ loops
        statesAndSymbolsToS :: [(State, Symbol)] =
            (\(S s, q) -> (q, s)) <$> values qsToStatesToS
        statesWithSymbolsToS :: Map State (Set Symbol) =
            fromListWith (\/) $ map (_2 %~ singleton) statesAndSymbolsToS
        predicate :: (State, Symbol) -> Bool =
            \(q, s) -> fromMaybe True $ do
                symbolsToS <- statesWithSymbolsToS !? q
                return $ s `member` symbolsToS
    quadruples <?>= predicate
    qs' <- use quadruples
    when (size qs > size qs')
        isChanged
      where
        isLoop :: Predicate Quadruple
        isLoop ((q1, s), (sm, q2)) = q1 == q2 && sm == S s
        withAnyMove :: Predicate QuadrupleToPart
        withAnyMove (M _, _) = True
        withAnyMove _ = False
        stateToFrom :: Set State -> Predicate QuadrupleToPart
        stateToFrom qs (_, q) = q `member` qs

unconsSet :: Set a -> (a, Set a)
unconsSet s = (findMin s, deleteMin s)

mergeSimilarStates :: TMConverting
mergeSimilarStates = do
    qs <- use quadruples
    allQs <- use allStates
    let statesWithSymbols :: Map Symbol (Set State) =
            fromListWith (\/) $ (swap . (_1 %~ singleton)) <$> keys qs
        statesWithoutSymbols =
            (allQs <\ finalState \\) <$> statesWithSymbols
        removeUselessGroups :: [Set State] -> [Set State] =
            filter ((> 1) . size)
        cloneLoops =
            concatMap $ \quad@((qf, s), (sm, qt)) ->
                if qf == qt
                then [quad, ((qf, s), (sm, state (-1)))]
                else [quad :: TuringMachine.Quadruple]
        qsGroupedBySymbol :: Map Symbol [((SymbolOrMove, State), Set State)] =
            fromListWith (++) $
                map (\((qf, s), sm_qt) -> (s, [(sm_qt, singleton qf)])) $
                    cloneLoops $
                        toList qs
        qsGroupedBySymbolAndQuadrupleSndPart =
            fromListWith (\/) <$> qsGroupedBySymbol
        groupsOfStates :: [[Set State]] =
            values $
                mapWithKey (\s ->
                    let qsWithoutS =
                            fromMaybe emptyC $ statesWithoutSymbols !? s
                    in  removeUselessGroups .
                        (qsWithoutS :) .
                        map (\/ qsWithoutS) .
                        values
                  ) $
                    qsGroupedBySymbolAndQuadrupleSndPart
        merge2ListsOfGroups log1 log2 = removeUselessGroups $ nubOrd $ do
            g1 <- log1
            g2 <- log2
            return $ g1 /\ g2
        totalListOfGroups = foldl1 merge2ListsOfGroups groupsOfStates
    when (not $ null totalListOfGroups) $ do
        let statesWithSimilarState :: Map State State =
                fromList $
                    (\(q, qs') -> (,q) <$> values qs') $
                        unconsSet $
                            maximumBy (compare `on` size) $
                                totalListOfGroups
        states %= \s -> Just $ fromMaybe s $ statesWithSimilarState !? s
        isChanged

removeUnknownSymbols :: TMConverting
removeUnknownSymbols = do
    symbolsFromAlphabet  :: Set Symbol <- uses alphabet valuesSet
    symbolsFromAllPlaces :: Set Symbol <- use  allSymbols
    when (not $ nullC $ symbolsFromAllPlaces \\ symbolsFromAlphabet) $ do
        symbols %= \s ->
            if s `member` symbolsFromAlphabet
            then Just s
            else Nothing
        isChanged

removeUnusedStates :: TMConverting
removeUnusedStates = do
    qs <- use quadruples
    let takeStates ((q1, _), (_, q2)) = [q1,q2]
        statesFromQs :: Set State =
            (fromList $ concat $ takeStates <$> toList qs) <+
                startState <+ finalState
        renumberedStates :: Map State State =
            fromList $ zip (toList statesFromQs) [minBound..]
    states %= (renumberedStates !?)

removeUnusedSymbols :: TMConverting
removeUnusedSymbols = do
    qs <- use quadruples
    let takeSymbols ((_, s), (ms, _)) =
            case ms of
                M _  -> [s]
                S s' -> [s, s']
        symbolsFromQs :: Set Symbol =
            fromList $ concat $ takeSymbols <$> toList qs
        renumberedSymbols :: Map Symbol Symbol =
            fromList $ zip (toList symbolsFromQs) [minBound..]
    symbols %= (renumberedSymbols !?)

fixIfQuadrupleFromFinalState :: TMConverting
fixIfQuadrupleFromFinalState = do
    qs <- use quadruples
    let fromFinalState = views (_1._1) (== finalState)
    when (or $ fromFinalState <$> toList qs) $ do
        allSs :: [ShowedSymbol] <- uses alphabet values
        put $ die allSs

fixIfWithoutFinalState :: TMConverting
fixIfWithoutFinalState = do
    qs <- use quadruples
    let hasFinalState = views (_2._2) (== finalState)
    when (and $ not.hasFinalState <$> toList qs) $ do
        allSs :: [ShowedSymbol] <- uses alphabet values
        put $ die allSs

optimization :: Level -> TMConverting
optimization l = do
    when (l >= O1) $
        infinityConverting removeUselessQuadruples
    when (l >= O2) $ do
        infinityConverting mergeSimilarStates
    removeUnknownSymbols
    removeUnusedStates
    removeUnusedSymbols
    fixIfWithoutFinalState
    fixIfQuadrupleFromFinalState

optimize' :: Level -> TuringMachine -> (TuringMachine, Bool)
optimize' l tm = flip runState False $ flip execStateT tm $ optimization l

optimize :: Level -> TuringMachine -> TuringMachine
optimize = (fmap.fmap) fst optimize'

optimal :: Level -> TuringMachine -> Bool
optimal = (fmap.fmap) snd optimize'

ifCanOptimize :: MonadFail m => Level -> TuringMachine -> m TuringMachine
ifCanOptimize l tm =
    let (newTM, changed) = optimize' l tm
    in  if changed
        then return newTM
        else fail "Can't optimize"

safeOptimize :: Level -> TuringMachine -> TuringMachine
safeOptimize l = optimize l . secure

safeOptimal :: Level -> TuringMachine -> Bool
safeOptimal l = optimal l . secure

ifCanSafeOptimize :: MonadFail m => Level -> TuringMachine -> m TuringMachine
ifCanSafeOptimize l = ifCanOptimize l . secure

newtype DiffTM = DiffTM (TuringMachine, TuringMachine)

instance Show DiffTM where
    show (DiffTM (tm1, tm2)) = "+:\n" ++ show tm1 ++ "-:\n" ++ show tm2

diffTM :: Level -> TuringMachine -> DiffTM
diffTM l oldTM =
    let newTM = optimize l oldTM
        oldQs :: Set TuringMachine.Quadruple =
            fromList $ toList $ oldTM^.quadruples
        newQs :: Set TuringMachine.Quadruple =
            fromList $ toList $ newTM^.quadruples
    in  DiffTM (
            newTM & quadruples .~ fromList (toList (newQs \\ oldQs)),
            oldTM & quadruples .~ fromList (toList (oldQs \\ newQs))
          )
