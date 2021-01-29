module TMTesting where

import TMTypes
import TMSemigroup
import TMReader
import Move
import SP2GP
import GPTypes
import Interpreter
import qualified Set as MySet
import qualified Data.Set as Set
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Tuple.Utils (fst3)

type MTM = Maybe TuringMachine

(+++) :: MTM -> MTM -> MTM
(+++) = ((unTMC <$>) .) . ((<>) `on` (TMC <$>))

(|||) :: MTM -> MTM -> MTM
(|||) = ((unTMU <$>) .) . ((<>) `on` (TMU <$>))

(@@@) :: MTM -> MTM -> MTM
(@@@) = (<@>)

(###) :: MTM -> MTM -> MTM
(###) = ((unTME <$>) .) . ((<>) `on` (TME <$>))

fromList' :: [Quadruple] -> MTM
fromList' = Just . fromList

empty :: MTM
empty = Nothing

break :: MTM
break = fromList' []

die :: Symbol -> MTM
die s =
    fromList' [((startState, s), (C s, startState))]

accept :: Symbol -> MTM
accept s =
    fromList' [((startState, s), (C s, finalState))]

move :: Symbol -> Move -> MTM
move s m =
    fromList' [((startState, s), (toSymbolMove m, finalState))]

moveInf :: Symbol -> Move -> MTM
moveInf s m =
    fromList' [((startState, s), (toSymbolMove m, startState))]

rewrite :: Symbol -> Symbol -> MTM
rewrite s s' =
    fromList' [((startState, s), (C s', finalState))]

fail :: Symbol -> MTM
fail s =
    fromList' [((startState, s), (C s, errorState))]

moveOnly :: Symbol -> Move -> Int -> MTM
moveOnly s m a = foldl (|||) (move s m) [ die (S i) | i <- [0..a] ]

acceptOnly :: Symbol -> Int -> MTM
acceptOnly s a = foldl (|||) (accept s) [ die (S i) | i <- [0..a] ]

rewriteOnly :: Symbol -> Symbol -> Int -> MTM
rewriteOnly s s' a = foldl (|||) (rewrite s s') [ die (S i) | i <- [0..a] ]

printGPfromTM :: String -> MTM -> IO ()
printGPfromTM msg (Just tm) = do
    putStrLn msg
    let qs = TMTypes.toList tm
        n  = runTMReader getN tm
        m  = runTMReader getM tm
        gp = groupBeta tm
        rs = case gp of GP x -> x
        gs = Set.fromList $ do
            r <- MySet.toList rs
            let ew1 `Equals` ew2 = r
            e <- ew1 ++ ew2
            return $ case e of
                    Positive x -> x
                    Negative x -> x
    putStr "Quadruples: "
    print $ length qs
    putStr "States:     "
    print n
    putStr "Symbols:    "
    print m
    putStr "Relations:  "
    print $ MySet.size rs
    putStr "Generators: "
    print $ Set.size gs
    putStrLn ""

testing :: IO ()
testing = mapM_ (\(msg, _, tm) -> printGPfromTM msg tm) testingSet

testingSet :: [(String, String, MTM)]
testingSet = [
    ("1. a*", "a",
        accept (S 0) |||
        moveInf (S 1) ToRight
    ),
    ("2. a+", "a",
        moveOnly (S 1) ToRight 1 +++
        (
            accept (S 0) |||
            moveInf (S 1) ToRight
        )
    ),
    ("3. a?", "a",
        accept (S 0) ||| (
            move (S 1) ToRight +++
            acceptOnly (S 0) 1
        )
    ),
    ("4. a|b", "ab",
        (
            (
                move (S 1) ToRight |||
                move (S 2) ToRight
            ) +++
            acceptOnly (S 0) 2
        ) ||| die (S 0)
    ),
    ("5. abc", "abc",
        foldr (+++) empty [ moveOnly (S i) ToRight 3 | i <- [1,2,3] ] +++
        acceptOnly (S 0) 3
    ),
    ("6. ababa", "ab",
        foldr (+++) empty [ moveOnly (S i) ToRight 2 | i <- [1,2,1,2,1] ] +++
        acceptOnly (S 0) 3
    ),
    ("7. a(bc)*ba", "abc",
        moveOnly (S 1) ToRight 3 +++
        (
            moveOnly (S 2) ToRight 3 @@@ move (S 3) ToRight
        ) +++ (
            move (S 1) ToRight ||| die (S 0) ||| die (S 2)
        ) +++
        acceptOnly (S 0) 3
    ),
    ("8. Dyck v.1", "abc",
        accept (S 0) ||| (
            (
                (
                    (
                        rewrite (S 1) (S 0) +++
                        move (S 0) ToRight +++
                        foldr (|||) (rewrite (S 0) (S 3)) [ moveInf (S i) ToRight | i <- [1,2,3] ]
                    ) ||| (
                        rewrite (S 2) (S 0) +++
                        move (S 0) ToRight +++
                        foldr (|||) (move (S 0) ToLeft) [ moveInf (S i) ToRight | i <- [1,2,3] ] +++
                        rewriteOnly (S 3) (S 0) 3 +++
                        move (S 0) ToLeft
                    )
                ) +++ foldr (|||) (move (S 0) ToRight) [ moveInf (S i) ToLeft  | i <- [1,2,3] ]
            ) @@@ empty
        ) |||
        die (S 3)
    ),
    ("9. Dyck v.2", "abcd",
        accept (S 0) ||| (
            (
                rewrite (S 1) (S 0) +++
                move (S 0) ToRight +++ (
                    die (S 0) |||
                    rewrite (S 1) (S 3) |||
                    rewrite (S 2) (S 4)
                )
            ) @@@ empty
        ) |||
        die (S 2) ||| (
            (
                rewrite (S 3) (S 0) +++
                move (S 0) ToRight +++ (
                    die (S 0) |||
                    (rewrite (S 1) (S 3) +++ move (S 3) ToRight) |||
                    (rewrite (S 2) (S 4) +++ move (S 4) ToRight) |||
                    moveInf (S 3) ToRight |||
                    moveInf (S 4) ToRight
                ) +++ (
                    die (S 0) |||
                    rewrite (S 1) (S 3) |||
                    rewrite (S 2) (S 4)
                ) +++ (
                    move (S 0) ToRight |||
                    moveInf (S 3) ToLeft |||
                    moveInf (S 4) ToLeft
                )
            ) @@@ empty
        ) ||| (
            (
                rewrite (S 4) (S 0) +++
                move (S 0) ToRight
            ) @@@ empty
        )
    ),
    ("10. Dyck v.3", "abc",
        accept (S 0) ||| (
            (
                (
                    (
                        rewrite (S 1) (S 0) +++
                        move (S 0) ToRight
                    ) @@@ (
                        rewrite (S 2) (S 0) +++
                        move (S 0) ToRight
                    )
                ) +++ (
                    die (S 0) ||| (
                        rewrite (S 1) (S 0) +++
                        move (S 0) ToRight +++ (
                            die (S 0) |||
                            moveInf (S 1) ToRight |||
                            rewrite (S 2) (S 3) |||
                            moveInf (S 3) ToRight
                        ) +++ (
                            die (S 0) |||
                            moveInf (S 1) ToRight |||
                            rewrite (S 2) (S 3) |||
                            moveInf (S 3) ToRight
                        ) +++ (
                            move (S 0) ToRight |||
                            moveInf (S 1) ToLeft |||
                            moveInf (S 3) ToLeft
                        )
                    ) |||
                    rewrite (S 3) (S 1)
                )
            ) @@@ empty
        ) |||
        die (S 2) ||| (
            (
                rewrite (S 3) (S 0) +++
                move (S 0) ToRight
            ) @@@ empty
        )
    ),
    ("11. Dyck v.4", "abcd",
        accept (S 0) ||| (
            (
                (
                    (
                        (
                            (
                                rewrite (S 1) (S 0) +++
                                move (S 0) ToRight
                            ) @@@ (
                                rewrite (S 2) (S 0) +++
                                move (S 0) ToRight
                            )
                        ) +++ (
                            die (S 0) |||
                            rewrite (S 1) (S 0)
                        )
                    ) |||
                    rewrite (S 3) (S 0)
                ) +++
                move (S 0) ToRight +++ (
                    die (S 0) |||
                    (rewrite (S 1) (S 3) +++ move (S 3) ToRight) |||
                    (rewrite (S 2) (S 4) +++ move (S 4) ToRight) |||
                    moveInf (S 3) ToRight |||
                    moveInf (S 4) ToRight
                ) +++ (
                    die (S 0) |||
                    rewrite (S 1) (S 3) |||
                    rewrite (S 2) (S 4)
                ) +++ (
                    move (S 0) ToRight |||
                    moveInf (S 3) ToLeft |||
                    moveInf (S 4) ToLeft
                )
            ) @@@ empty
        ) |||
        die (S 2) ||| (
            (
                rewrite (S 4) (S 0) +++
                move (S 0) ToRight
            ) @@@ empty
        )
    ),
    ("12. Dyck v.5", "abc",
        let mainModule n = mainModule' n n @@@ empty
            mainModule' 1 n =
                (
                    (
                        rewrite (S 3) (S 0) +++
                        move (S 0) ToRight
                    ) @@@ empty
                ) ||| (
                    rewrite (S 1) (S 0) +++
                    move (S 0) ToRight +++
                    foldr1 (+++) (
                        replicate n (
                            die (S 0) |||
                            moveInf (S 1) ToRight |||
                            rewrite (S 2) (S 3) |||
                            moveInf (S 3) ToRight
                        )
                    ) +++ (
                        move (S 0) ToRight |||
                        moveInf (S 1) ToLeft |||
                        moveInf (S 3) ToLeft
                    )
                ) |||
                die (S 0)
            mainModule' m n =
                (
                    (
                        (
                            rewrite (S 1) (S 0) +++
                            move (S 0) ToRight
                        ) @@@ (
                            rewrite (S 2) (S 0) +++
                            move (S 0) ToRight
                        )
                    ) +++
                    mainModule' (m-1) n
                ) ||| (
                    (
                        rewrite (S 3) (S 0) +++
                        move (S 0) ToRight
                    ) @@@ empty
                ) |||
                die (S 0)
        in  (
                accept (S 0) |||
                (mainModule 10 @@@ empty) |||
                die (S 2)
            )
    ),
    ("13. wwR 2s v.1", "ab",
        accept (S 0) ||| (
            (
                (
                    rewrite (S 1) (S 0) +++
                    move (S 0) ToRight +++ (
                        move (S 0) ToLeft |||
                        moveInf (S 1) ToRight |||
                        moveInf (S 2) ToRight
                    ) +++
                    rewriteOnly (S 1) (S 0) 2 +++
                    move (S 0) ToLeft +++ (
                        move (S 0) ToRight |||
                        moveInf (S 1) ToLeft |||
                        moveInf (S 2) ToLeft
                    )
                ) ||| (
                    rewrite (S 2) (S 0) +++
                    move (S 0) ToRight +++ (
                        move (S 0) ToLeft |||
                        moveInf (S 1) ToRight |||
                        moveInf (S 2) ToRight
                    ) +++
                    rewriteOnly (S 2) (S 0) 2 +++
                    move (S 0) ToLeft +++ (
                        move (S 0) ToRight |||
                        moveInf (S 1) ToLeft |||
                        moveInf (S 2) ToLeft
                    )
                )
            ) @@@ empty
        )
    ),
    ("14. wwR 2s v.2", "ab",
        accept (S 0) ||| (
            (
                (
                    foldr1 (|||)
                    [
                        rewrite (S i) (S 0) +++
                        move (S 0) ToRight +++ (
                            move (S 0) ToLeft |||
                            moveInf (S 1) ToRight |||
                            moveInf (S 2) ToRight
                        ) +++
                        rewriteOnly (S i) (S 0) 2
                    |   i <- [1,2]
                    ]
                ) +++
                move (S 0) ToLeft +++ (
                    move (S 0) ToRight |||
                    moveInf (S 1) ToLeft |||
                    moveInf (S 2) ToLeft
                )
            ) @@@ empty
        )
    ),
    ("15. wwR 4s", "abcd",
        accept (S 0) ||| (
            (
                foldr1 (|||)
                [
                    rewrite (S i) (S 0) +++
                    move (S 0) ToRight +++
                    foldr (|||) (move (S 0) ToLeft) [ moveInf (S j) ToRight | j <- [1..4] ] +++
                    rewriteOnly (S i) (S 0) 4
                |   i <- [1..4]
                ] +++
                move (S 0) ToLeft +++
                foldr (|||) (move (S 0) ToRight) [ moveInf (S j) ToLeft | j <- [1..4] ]
            ) @@@ empty
        )
    ),
    ("16. wwR 8s", "abcdefgh",
        accept (S 0) ||| (
            (
                foldr1 (|||)
                [
                    rewrite (S i) (S 0) +++
                    move (S 0) ToRight +++
                    foldr (|||) (move (S 0) ToLeft) [ moveInf (S j) ToRight | j <- [1..8] ] +++
                    rewriteOnly (S i) (S 0) 8
                |   i <- [1..8]
                ] +++
                move (S 0) ToLeft +++
                foldr (|||) (move (S 0) ToRight) [ moveInf (S j) ToLeft | j <- [1..8] ]
            ) @@@ empty
        )
    ),
    ("17. wwR 16s", "abcdefghijklmnop",
        accept (S 0) ||| (
            (
                foldr1 (|||)
                [
                    rewrite (S i) (S 0) +++
                    move (S 0) ToRight +++
                    foldr (|||) (move (S 0) ToLeft) [ moveInf (S j) ToRight | j <- [1..16] ] +++
                    rewriteOnly (S i) (S 0) 16
                |   i <- [1..16]
                ] +++
                move (S 0) ToLeft +++
                foldr (|||) (move (S 0) ToRight) [ moveInf (S j) ToLeft | j <- [1..16] ]
            ) @@@ empty
        )
    )
  ]

aboutTestingSet :: IO ()
aboutTestingSet = mapM_ putStrLn $ fst3 <$> testingSet

runTest' :: Int -> String -> Int -> IO ()
runTest' i w d =
    mapM_ (\(n, tp, (Q st)) -> putStrLn $ show n ++ ":  (" ++ show st ++ ")  " ++ show tp) $
    fmap (\(n, ws) -> (n, Interpreter.tape ws, currentState ws)) $
    zip [0..] $ take d $ states $
    (\(m, a, Just t) -> startWithAlphabet t (MC <$> w) 0 (Set.fromList $ MC <$> a)) $
    testingSet !! (i - 1)

runTest :: Int -> String -> IO ()
runTest i w = runTest' i w 242
