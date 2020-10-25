{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.LaTeX.Base.Render
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Inputenc
import qualified Data.Set as Set

import GrammarPrinter
import TMPrinter
import Lib
import GrammarType
import CFG2TM 
import TMType
import TMInterpreter
import ConfigPrinter
import TM2SymTM
import TM2SM
import SMPrinter
import SM2GR
import qualified SMType
import GRType
import GapFuncWriter
import System.IO
import qualified Data.Map.Strict as Map
import SMInterpreter
import Helpers
import DotGraphWriter
import MapleFuncWriter
import Data.Tuple.Utils
import Console.Options
import Data.List (isSuffixOf)

preambula :: LaTeXM ()
preambula = 
    documentclass [] article
    <> usepackage [Text.LaTeX.Packages.Inputenc.utf8] inputenc
    <> usepackage [] "unicode-math"
    <> usepackage [] "amsmath"
    <> usepackage [] "mathtools"
    <> usepackage ["left=1cm","right=5cm", "top=2cm", "bottom=2cm", "bindingoffset=0cm"] "geometry"
    <> title "Examples"

example :: LaTeX
example = execLaTeXM $ 
    do
        preambula 
        document $ do
            doLaTeX testGrammar
            doLaTeX $ cfg2tm testGrammar
            --doLaTeX $ interpretTM ["a"] $ cfg2tm testGrammar
            -- newpage
            doLaTeX $ symTM $ cfg2tm testGrammar
            doLaTeX $ symDetTM $ cfg2tm testGrammar
            -- newpage
            -- doLaTeX $ fst3 $ tm2sm $ symDetTM $ cfg2tm testGrammar
            -- doLaTeX epsTestGrammar
            -- doLaTeX $ cfg2tm epsTestGrammar
            -- -- doLaTeX $ interpretTM ["a"] $ cfg2tm epsTestGrammar
            -- -- newpage
            -- doLaTeX epsTestLeftGrammar
            -- doLaTeX $ cfg2tm epsTestLeftGrammar
            -- doLaTeX $ interpretTM ["a"] $ cfg2tm epsTestLeftGrammar
            -- doLaTeX $ interpretTM [] $ cfg2tm epsTestLeftGrammar
            -- newpage
            -- doLaTeX abTestGrammar
            -- doLaTeX $ cfg2tm abTestGrammar
            -- -- doLaTeX $ interpretTM ["b", "b", "a", "a"] $ cfg2tm abTestGrammar
            --newpage
            --doLaTeX ab2TestGrammar
            --doLaTeX $ cfg2tm ab2TestGrammar
            -- -- doLaTeX $ interpretTM ["b", "a", "b", "a"] $ cfg2tm ab2TestGrammar
            -- -- newpage
            -- -- doLaTeX $ tm2sm $ symTM $ cfg2tm ab2TestGrammar
            -- doLaTeX abNoEpsTestGrammar
            -- doLaTeX $ cfg2tm abNoEpsTestGrammar
            -- newpage
            -- doLaTeX ab3TestGrammar
            -- doLaTeX $ cfg2tm ab3TestGrammar
            -- -- doLaTeX $ interpretTM ["b", "a", "b", "a"] $ cfg2tm ab3TestGrammar
            -- -- doLaTeX $ fst $ tm2sm tmForTestSm
            --doLaTeX $ threePhaseProcessing simpleTM
            --doLaTeX $ fst $ oneruleTM
            --doLaTeX $ threePhaseProcessing $ fst $ oneruleTM
            --doLaTeX $ symTM $ fst $ oneruleTM
            --doLaTeX $ symDetTM $ fst $ oneruleTM
            --doLaTeX symSmallMachine
            -- newpage
            --doLaTeX $ fst3 $ tm2sm symSmallMachine

-- main :: IO()
-- main = do
--     renderFile "out.tex" example 

-- main :: IO()
-- main = do
--         let tm = cfg2tm ab2TestGrammar
--         let inp = ["b", "a", "b", "b", "a", "b", "a", "a"]
--         putStrLn $ show $ interpretTM inp tm

-- main :: IO()
-- main = do
--         let s@(sm, w, as) = tm2sm $ symDetTM $ cfg2tm testGrammar
--         let inputSmb = map (\a -> SMType.SmbY $ SMType.Y a) $ mapValue ["a"]
--         let startWord = sigmaFunc as $ inputSmb : (replicate (length as - 1) [])
--         putStrLn $ show $ length $ interpretSM startWord sm w

-- main :: IO()
-- main = do
--         let s@(sm, w, as) = tm2sm symSmallMachine
--         let inputSmb = map (\a -> SMType.SmbY $ SMType.Y a) $ mapValue ["a"]
--         let startWord = sigmaFunc as $ inputSmb : (replicate (length as - 1) [])
--         putStrLn $ show $ length $ interpretSM startWord sm w

-- main :: IO()
-- main = do
--         putStrLn $ show $ (foldl (+) 0 $ map snd $ Map.toList m) - length m
--         writeGraph "test.dot" g
--         where 
--             (sm, _, as) = tm2sm $ symDetTM $ cfg2tm testGrammar
--             inputSmb = map (\a -> SMType.SmbY $ SMType.Y a) $ mapValue ["a"]
--             startWord = sigmaFunc as $ inputSmb : (replicate (length as - 1) [])
--             (g, m) = getRestrictedGraph startWord sm 1
    
-- main :: IO()
-- main = do
--     let smw@(sm, w0, as) = tm2sm $ symSmallMachine
--     --let (sm, w0, word) = oneRuleSm
--     let gr@(GR (a, r)) = sm2gr (sm, w0)
--     --let (gr@(GR (a, r)), word) = oneRuleGr
--     --putStrLn $ (show $ length $ SMType.srs sm)
--     putStrLn $ (show $ length a) ++ " " ++ (show $ length r)
--     let genmap = Map.fromList $ zip (Set.toList a) $ map ((++) "f." . show) [1..]
--     let inputSmb = map (\a -> SMType.SmbY $ SMType.Y a) $ mapValue ["a"]
--     let startSMWord = sigmaFunc as $ inputSmb : (replicate (length as - 1) [])
--     let word = easyHubRelation startSMWord
--     let fword = foldl1 (\x y -> x ++ "*" ++ y) $ map (printSmb genmap) word 
--     putStrLn fword
--     do fhandle <- openFile "symSmallMachineGroup.txt" WriteMode
--        writeGap gr fhandle genmap
--        hFlush fhandle
--        hClose fhandle

-- main :: IO()
-- main = do
--     let (sm, accessWord, startWord, word) = aStarSMachine
--     let gr@(GR (a, r)) = sm2gr (sm, accessWord)
--     putStrLn $ (show $ length a) ++ " " ++ (show $ length r)
--     let genmap = Map.fromList $ zip (Set.toList a) $ map ((++) "f." . show) [1..]
--     --let word = foldl (\acc x -> [(SmbA' $ A_R x)] ++ acc ++ [(SmbA $ A_R x)]) (easyHubRelation startWord) $ SMType.srs sm
--     --putStrLn $ show word
--     let fword = foldl1 (\x y -> x ++ "*" ++ y) $ map (printSmb genmap) word 
--     putStrLn fword
--     do fhandle <- openFile "aStarSMachineGroup.txt" WriteMode
--        writeGap gr fhandle genmap
--        hFlush fhandle
--        hClose fhandle

oneRuleGr :: (GR, [SmbR])
oneRuleGr = do
    let y = SMType.Y $ defValue "a"
    let q0 = SMType.State SMType.Q "0" (Set.fromList []) Nothing
    let q1 = SMType.State SMType.Q "1" (Set.fromList []) Nothing
    let q0' = SMType.State SMType.Q "2" (Set.fromList []) Nothing
    let q1' = SMType.State SMType.Q "3" (Set.fromList []) Nothing
    let from1 = SMType.Word [SMType.SmbQ q0]
    let to1 = SMType.Word [SMType.SmbQ q0', SMType.SmbY' y]
    let from2 = SMType.Word [SMType.SmbQ q1]
    let to2 = SMType.Word [SMType.SmbQ q1']
    let r = SMType.SRule $ [(from1, to1), (from2, to2)]
    let as = [A_Y y, A_Q q0, A_Q q1, A_Q q0', A_Q q1', A_R r, A_K 1, A_K 2]
    let powr x = (GRType.SmbA' $ A_R r) : x ++ [GRType.SmbA $ A_R r]
    let u = hubRelation $ SMType.Word $ [SMType.SmbQ q0', SMType.SmbQ q1']
    let relations = [   GRType.Relation (powr [smb2As $ SMType.SmbQ q0], map smb2As [SMType.SmbQ q0', SMType.SmbY' y]),
                        GRType.Relation (powr [smb2As $ SMType.SmbQ q1], [smb2As $ SMType.SmbQ q1'])] ++
                        [GRType.Relation ([GRType.SmbA $ A_R r, GRType.SmbA $ A_Y y], [GRType.SmbA $ A_Y y, GRType.SmbA $ A_R r])] ++
                        [GRType.Relator u]
    let startWord = hubRelation $ SMType.Word [SMType.SmbQ q0, SMType.SmbY y, SMType.SmbQ q1]
    (GR (Set.fromList as, Set.fromList relations), startWord)

oneRuleSm :: (SMType.SM, SMType.Word, [SmbR])
oneRuleSm = do
    let y = SMType.Y $ defValue "a"
    let q0 = SMType.State SMType.Q "0" (Set.fromList []) Nothing
    let q1 = SMType.State SMType.Q "1" (Set.fromList []) Nothing
    let q0' = SMType.State SMType.Q "2" (Set.fromList []) Nothing
    let q1' = SMType.State SMType.Q "3" (Set.fromList []) Nothing
    let from1 = SMType.Word [SMType.SmbQ q0]
    let to1 = SMType.Word [SMType.SmbQ q0', SMType.SmbY' y]
    let from2 = SMType.Word [SMType.SmbQ q1]
    let to2 = SMType.Word [SMType.SmbQ q1']
    let r = SMType.SRule $ [(from1, to1), (from2, to2)]
    let startWord = hubRelation $ SMType.Word [SMType.SmbQ q0, SMType.SmbY y, SMType.SmbQ q1]
    (SMType.SM [[y]] [Set.fromList [q0, q0'], Set.fromList [q1, q1']] [r], SMType.Word [SMType.SmbQ q0', SMType.SmbQ q1'], startWord)

printCount :: Grammar -> Bool -> IO ()
printCount grammar@(Grammar (n, t, r, _)) is_det = do
    putStrLn $ "T: " ++ (show $ length t) ++ " N: " ++ (show $ length n) ++ " R: " ++ (show $ length r) 
    
    let tm@(TM (InputAlphabet tmX, tapeAlphabet, MultiTapeStates multiTapeStates, Commands tmCmds, _, _)) = cfg2tm grammar
    let tmG = foldl (\acc (TapeAlphabet a) -> acc + length a) 0 tapeAlphabet
    let tmQ = foldl (\acc a -> acc + length a) 0 multiTapeStates
    putStrLn $ "tmX: " ++ (show $ length tmX) ++ " tmG: " ++ (show tmG) ++ " tmQ: " ++ (show tmQ) ++ " tmCmds: " ++ (show $ length tmCmds)

    let tm'@(TM (InputAlphabet tmX', tapeAlphabet', MultiTapeStates multiTapeStates', Commands tmCmds', _, _)) = toSymm is_det $ tm
    let tmG' = foldl (\acc (TapeAlphabet a) -> acc + length a) 0 tapeAlphabet'
    let tmQ' = foldl (\acc a -> acc + length a) 0 multiTapeStates'
    putStrLn $ "tm'X: " ++ (show $ length tmX') ++ " tm'G: " ++ (show tmG') ++ " tm'Q: " ++ (show tmQ') ++ " tm'Cmds: " ++ (show $ length tmCmds')

    let (sm, w, _) = tm2sm $ tm'
    let smY = concat $ SMType.yn sm
    let smQ = foldl (\acc a -> acc + length a) 0 (SMType.qn sm)
    putStrLn $ "smY: " ++ (show $ length smY) ++ " smQ: " ++ (show smQ) ++ " smR: " ++ (show $ length $ SMType.srs sm)

    let (GR (a, rels)) = sm2gr (sm, w)
    putStrLn $ "A: " ++ (show $ length a) ++ " R: " ++ (show $ length rels)
    putStrLn ""

printExperiments :: Bool -> IO()
printExperiments is_det = do
        putStrLn $ "One rule"
        printCount testGrammar is_det
        
        putStrLn $ "A star"
        printCount epsTestGrammar is_det
    
        putStrLn $ "Dyck"
        printCount ab2TestGrammar is_det

printPresentationGap :: Bool -> Grammar -> String -> IO()
printPresentationGap is_det grammar outFileName = do
    let symm = toSymm is_det
    let (sm, accessWord, _) = tm2sm $ symm $ cfg2tm grammar
    let gr@(GR (a, _)) = sm2gr (sm, accessWord)
    let hub = hubRelation accessWord
    let genmap = Map.fromList $ zip (Set.toList a) $ map ((++) "f." . show) [1..]
    do fhandle <- openFile outFileName WriteMode
       writeGap gr fhandle genmap hub
       hFlush fhandle
       hClose fhandle
    

printExample :: Bool -> Grammar -> String -> IO()
printExample is_det grammar outFileName = renderFile outFileName exampleLatex
    where
        symm = toSymm is_det
        exampleLatex = execLaTeXM $ 
            do
                preambula 
                document $ do
                    doLaTeX grammar
                    newpage
                    doLaTeX $ cfg2tm grammar
                    newpage
                    doLaTeX $ symm $ cfg2tm grammar
                    newpage
                    doLaTeX $ fst3 $ tm2sm $ symm $ cfg2tm grammar



flagParamBoolParser :: String -> Either String Bool
flagParamBoolParser s
    | s == "true"  || s == "True"   = Right True
    | s == "false" || s == "False"  = Right False
    | otherwise                     = Left "Bool expected"

toSymm :: Bool -> TM -> TM
toSymm True = symDetTM
toSymm False = symTM

flagParamExampleParser :: String -> Either String Grammar
flagParamExampleParser s
    | s == "one"    = Right testGrammar
    | s == "a*"     = Right epsTestGrammar
    | s == "dyck"   = Right ab2TestGrammar
    | otherwise     = Left "Example name expected"

flagParamOutParser :: String -> Either String String
flagParamOutParser s
    | isSuffixOf ".tex" s   = Right s
    | otherwise             = Right $ s ++ ".tex"

main :: IO()
main = defaultMain $ do 
    programName "print-results"
    programDescription "Printing experiments result."
    is_det_flag_param <- flagParam (FlagShort 'd' 
                                    <> FlagLong "is_det" 
                                    <> FlagDescription "The flag corresponds to which symmetrization of the Turing machine should be used, deterministic or non-deterministic.\ 
                                                        \ For deterministic use argument \"true\", for non use \"false\".") 
                                        (FlagRequired flagParamBoolParser)
    print_example_flag_param <- flagParam (FlagShort 'p' 
                                            <> FlagLong "print_example" 
                                            <> FlagDescription "With this flag you can specify which grammar should be printed in LaTeX on every transformation step. \
                                            \\"one\" --- one rule grammar, \"a*\" --- grammar for language A with Kleene star, \"dyck\" --- Dyck language grammar. \
                                            \If this flag is not used, numerical results of experiments will be displayed.") 
                                                (FlagRequired flagParamExampleParser)
    out_flag_param <- flagParam (FlagShort 'o' 
                                <> FlagDescription "With this flag, you can specify an output filename.") 
                                    (FlagRequired flagParamOutParser)
    print_gap_flag <- flag (FlagShort 'G' 
                            <> FlagDescription "The flag corresponds for printing a group presentation to the Gap-format file.")
    action $ \toParam -> do
        case (toParam is_det_flag_param, toParam print_example_flag_param, toParam out_flag_param, toParam print_gap_flag) of
            (Just is_det, Just grammar, Just outFileName, True) -> printPresentationGap is_det grammar outFileName
            (Just is_det, Just grammar, Nothing, True) -> printPresentationGap is_det grammar "out.txt"
            (Just is_det, Just grammar, Just outFileName, False) -> printExample is_det grammar outFileName
            (Just is_det, Just grammar, Nothing, False) -> printExample is_det grammar "out.tex"
            (Just is_det, Nothing, Nothing, False) -> printExperiments is_det
            (Nothing, _, _, _) -> putStrLn "Expected a \"is_det\" paramater"
            (Just _, Nothing, Nothing, True) -> putStrLn "Expected a \"p\" paramater"
            (Just _, Nothing, Just _, _) -> putStrLn "Expected a \"p\" paramater"


symSmallMachine :: TM
symSmallMachine = tm where
    a = defValue "a"
    q0 = State "q_0^1"
    q1 = State "q_1^1"
    inp = InputAlphabet (Set.fromList [a])
    tapes = [TapeAlphabet (Set.fromList [a])]
    states = MultiTapeStates [Set.fromList [q0, q1]]
    cmd1 = [SingleTapeCommand ((a, q0, RBS), (ES, q1, RBS))]
    cmd2 = [SingleTapeCommand ((ES, q1, RBS), (a, q0, RBS))]
    cmds = Commands $ Set.fromList [cmd1, cmd2]
    start = StartStates [q0]
    access = AccessStates [q1]
    tm = TM (inp, tapes, states, cmds, start, access)

aStarSMachine :: (SMType.SM, SMType.Word, SMType.Word, [SmbR])
aStarSMachine = (SMType.SM alphabet states relations, accessWord, startWord, word) 
    where
    a = SMType.Y $ defValue "a"
    a1 = SMType.Y $ defValue "a_1"
    a2 = SMType.Y $ defValue "a_2"
    alphabet = [[a, a1], [a2]]
    q0 = SMType.State SMType.Q "0" (Set.fromList []) Nothing
    q1 = SMType.State SMType.Q "1" (Set.fromList []) Nothing
    q2 = SMType.State SMType.Q "2" (Set.fromList []) Nothing
    alp = SMType.State SMType.E "" (Set.fromList []) Nothing
    alp1 = SMType.State SMType.E "1" (Set.fromList []) Nothing
    alp2 = SMType.State SMType.E "2" (Set.fromList []) Nothing
    alp3 = SMType.State SMType.E "3" (Set.fromList []) Nothing
    p = SMType.State SMType.P "" (Set.fromList []) Nothing
    p1 = SMType.State SMType.P "1" (Set.fromList []) Nothing
    p2 = SMType.State SMType.P "2" (Set.fromList []) Nothing
    p3 = SMType.State SMType.P "3" (Set.fromList []) Nothing
    states = [Set.fromList [alp, alp1, alp2, alp3], Set.fromList [p, p1, p2, p3], Set.fromList [q0, q1, q2]]
    r1 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp], SMType.Word [SMType.SmbQ alp1]), 
                            (SMType.Word [SMType.SmbQ p, SMType.SmbQ q1], SMType.Word [SMType.SmbQ p, SMType.SmbQ q1])]
    r2 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp1], SMType.Word [SMType.SmbQ alp]), 
                            (SMType.Word [SMType.SmbQ p, SMType.SmbQ q1], SMType.Word [SMType.SmbY' a, SMType.SmbQ p, SMType.SmbQ q2])]
    r3 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp], SMType.Word [SMType.SmbQ alp2]), 
                            (SMType.Word [SMType.SmbQ p, SMType.SmbQ q2], SMType.Word [SMType.SmbQ p, SMType.SmbQ q2])]
    r4 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp2], SMType.Word [SMType.SmbQ alp2]), 
                            (SMType.Word [SMType.SmbQ p, SMType.SmbQ q2], SMType.Word [SMType.SmbY' a, SMType.SmbQ p1, SMType.SmbQ q2])]
    r5 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp2], SMType.Word [SMType.SmbQ alp2]), 
                            (SMType.Word [SMType.SmbQ p1], SMType.Word [SMType.SmbY' a1, SMType.SmbQ p1, SMType.SmbY a2]),
                            (SMType.Word [SMType.SmbQ q2], SMType.Word [SMType.SmbQ q2])]
    r6 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp2], SMType.Word [SMType.SmbQ alp2]), 
                            (SMType.Word [SMType.SmbQ p1], SMType.Word [SMType.SmbY' a, SMType.SmbY a1, SMType.SmbQ p2]),
                            (SMType.Word [SMType.SmbQ q2], SMType.Word [SMType.SmbQ q2])]
    r7 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp2], SMType.Word [SMType.SmbQ alp2]), 
                            (SMType.Word [SMType.SmbQ p2], SMType.Word [SMType.SmbY a, SMType.SmbQ p2, SMType.SmbY' a2]),
                            (SMType.Word [SMType.SmbQ q2], SMType.Word [SMType.SmbQ q2])]
    r8 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp2], SMType.Word [SMType.SmbQ alp2]), 
                            (SMType.Word [SMType.SmbQ p2, SMType.SmbQ q2], SMType.Word [SMType.SmbQ p1, SMType.SmbQ q2])]
    r9 = SMType.SRule $ [   (SMType.Word [SMType.SmbQ alp2, SMType.SmbQ p1], SMType.Word [SMType.SmbQ alp2, SMType.SmbQ p3]), 
                            (SMType.Word [SMType.SmbQ q2], SMType.Word [SMType.SmbQ q2])]
    r10 = SMType.SRule $ [  (SMType.Word [SMType.SmbQ alp2], SMType.Word [SMType.SmbQ alp2]), 
                            (SMType.Word [SMType.SmbQ p3], SMType.Word [SMType.SmbY a, SMType.SmbQ p3, SMType.SmbY' a2]),
                            (SMType.Word [SMType.SmbQ q2], SMType.Word [SMType.SmbQ q2])]
    r11 = SMType.SRule $ [  (SMType.Word [SMType.SmbQ alp2], SMType.Word [SMType.SmbQ alp]), 
                            (SMType.Word [SMType.SmbQ p3, SMType.SmbQ q2], SMType.Word [SMType.SmbQ p, SMType.SmbQ q2])]
    r12 = SMType.SRule $ [  (SMType.Word [SMType.SmbQ alp], SMType.Word [SMType.SmbQ alp3]), 
                            (SMType.Word [SMType.SmbQ p, SMType.SmbQ q2], SMType.Word [SMType.SmbQ p, SMType.SmbQ q2])]
    r13 = SMType.SRule $ [  (SMType.Word [SMType.SmbQ alp3, SMType.SmbQ p, SMType.SmbQ q2], SMType.Word [SMType.SmbQ alp, SMType.SmbQ p, SMType.SmbQ q0])]                                                                                                              
    relations = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13]
    startWord = SMType.Word [SMType.SmbQ alp, SMType.SmbY a, SMType.SmbQ p, SMType.SmbQ q1]
    word = foldl (\acc x -> [(SmbA' $ A_R x)] ++ acc ++ [(SmbA $ A_R x)]) (easyHubRelation startWord) $ [r1, r2, r12, r13]
    accessWord = SMType.Word [SMType.SmbQ alp, SMType.SmbQ p, SMType.SmbQ q0]

oneruleTM :: (TM, [[SMType.Smb]])
oneruleTM = (tm, w) where
    a = defValue "a"
    q0 = State "q_0^1"
    q1 = State "q_1^1"
    inp = InputAlphabet (Set.fromList [a])
    tapes = [TapeAlphabet (Set.fromList [a])]
    states = MultiTapeStates [Set.fromList [q0, q1]]
    cmd = [SingleTapeCommand ((a,  q0, RBS), (ES, q1, RBS))]
    cmds = Commands $ Set.fromList [cmd]
    start = StartStates [q0]
    access = AccessStates [q1]
    tm = TM (inp, tapes, states, cmds, start, access)
    w = [[SMType.SmbY $ SMType.Y a], [], [SMType.SmbY $ SMType.Y $ BCommand cmd], []]

simpleTM :: TM
simpleTM = tm where
    a = defValue "a"
    q0 = State "q_0^1"
    q1 = State "q_1^1"
    q2 = State "q_2^1"
    inp = InputAlphabet (Set.fromList [a])
    tapes = [TapeAlphabet (Set.fromList [a])]
    states = MultiTapeStates [Set.fromList [q0, q1, q2]]
    cmds = Commands 
        $ Set.fromList [[SingleTapeCommand ((a,  q0, RBS), (ES, q1, RBS))],
                        [SingleTapeCommand ((LBS,  q1, RBS), (LBS, q2, RBS))]]
    start = StartStates [q0]
    access = AccessStates [q2]
    tm = TM (inp, tapes, states, cmds, start, access)    

tmForTestSm :: TM
tmForTestSm = tm where
    s = defValue "S"
    q0 = State "q_0"
    q0' = State "q_0'"
    q1 = State "q_1"
    q1' = State "q_1'"
    inp = InputAlphabet (Set.fromList [])
    tapes = [TapeAlphabet (Set.fromList [s]), TapeAlphabet (Set.fromList [])]
    states = MultiTapeStates [Set.fromList [q0, q0'], Set.fromList [q1, q1']]
    cmds = Commands $ Set.fromList [[PreSMCommand ((s, StateOmega q0), (ES, StateOmega q0')), 
                                    PreSMCommand ((ES, StateOmega q1), (ES, StateOmega q1'))]]
    start = StartStates []
    access = AccessStates []
    tm = TM (inp, tapes, states, cmds, start, access)    


testGrammar :: Grammar
testGrammar = grammar where
    terminal = Terminal "a"
    nonterminal = Nonterminal "S"
    grammar =
        Grammar(
            (Set.fromList [nonterminal]),
            (Set.fromList [terminal]),
            (Set.fromList [GrammarType.Relation (nonterminal, [GrammarType.T terminal])]),
            nonterminal
        )

epsTestGrammar :: Grammar
epsTestGrammar = grammar where
    terminal = Terminal "a"
    start = Nonterminal "S"
    nonterminal = Nonterminal "A"
    grammar =
        Grammar(
            (Set.fromList [nonterminal, start]),
            (Set.fromList [terminal]),
            (Set.fromList [
                GrammarType.Relation (start, [GrammarType.N nonterminal, GrammarType.N start]),
                GrammarType.Relation (start, [GrammarType.Eps]),
                GrammarType.Relation (nonterminal, [GrammarType.T terminal])
                ]),
            start
        )

epsTestLeftGrammar :: Grammar
epsTestLeftGrammar = grammar where
    terminal = Terminal "a"
    start = Nonterminal "S"
    nonterminal = Nonterminal "A"
    grammar =
        Grammar(
            (Set.fromList [nonterminal, start]),
            (Set.fromList [terminal]),
            (Set.fromList [
                GrammarType.Relation (start, [GrammarType.N start, GrammarType.N nonterminal]),
                GrammarType.Relation (start, [GrammarType.Eps]),
                GrammarType.Relation (nonterminal, [GrammarType.T terminal])
                ]),
            start
        )

abTestGrammar :: Grammar
abTestGrammar = grammar where
    a = Terminal "a"
    b = Terminal "b"
    s = Nonterminal "S"
    s1 = Nonterminal "C"
    aN = Nonterminal "A"
    bN = Nonterminal "B"
    grammar =
        Grammar(
            (Set.fromList [s, s1, aN, bN]),
            (Set.fromList [a, b]),
            (Set.fromList [
                GrammarType.Relation (s, [GrammarType.N aN, GrammarType.N s1]),
                GrammarType.Relation (s, [GrammarType.Eps]),
                GrammarType.Relation (s1, [GrammarType.N s, GrammarType.N bN]),
                GrammarType.Relation (aN, [GrammarType.T a]),
                GrammarType.Relation (bN, [GrammarType.T b])
                ]),
            s
        )

ab2TestGrammar :: Grammar
ab2TestGrammar = grammar where
    a = Terminal "a"
    b = Terminal "b"
    s = Nonterminal "S"
    s1 = Nonterminal "C"
    aN = Nonterminal "A"
    bN = Nonterminal "B"
    b1 = Nonterminal "D"
    grammar =
        Grammar(
            (Set.fromList [s, s1, aN, bN, b1]),
            (Set.fromList [a, b]),
            (Set.fromList [
                GrammarType.Relation (s, [GrammarType.N aN, GrammarType.N s1]),
                GrammarType.Relation (s, [GrammarType.Eps]),
                GrammarType.Relation (s1, [GrammarType.N s, GrammarType.N b1]),
                GrammarType.Relation (aN, [GrammarType.T a]),
                GrammarType.Relation (bN, [GrammarType.T b]),
                GrammarType.Relation (b1, [GrammarType.N bN, GrammarType.N s])
                ]),
            s
        )

abNoEpsTestGrammar :: Grammar
abNoEpsTestGrammar = grammar where
    a = Terminal "a"
    b = Terminal "b"
    s = Nonterminal "S"
    s1 = Nonterminal "C"
    aN = Nonterminal "A"
    bN = Nonterminal "B"
    grammar =
        Grammar(
            (Set.fromList [s, s1, aN, bN]),
            (Set.fromList [a, b]),
            (Set.fromList [
                GrammarType.Relation (s, [GrammarType.N aN, GrammarType.N s1]),
                GrammarType.Relation (s, [GrammarType.N aN, GrammarType.N bN]),
                GrammarType.Relation (s1, [GrammarType.N s, GrammarType.N bN]),
                GrammarType.Relation (aN, [GrammarType.T a]),
                GrammarType.Relation (bN, [GrammarType.T b])
                ]),
            s
        )

ab3TestGrammar :: Grammar
ab3TestGrammar = grammar where
    a = Terminal "a"
    b = Terminal "b"
    s0 = Nonterminal "S_0"
    s1 = Nonterminal "S_1"
    s = Nonterminal "S"
    c = Nonterminal "C"
    aN = Nonterminal "A"
    bN = Nonterminal "B"
    grammar =
        Grammar(
            (Set.fromList [s0, c, s, s1, aN, bN]),
            (Set.fromList [a, b]),
            (Set.fromList [
                GrammarType.Relation (s0, [GrammarType.N s, GrammarType.N s1]),
                GrammarType.Relation (s0, [GrammarType.N aN, GrammarType.N c]),
                GrammarType.Relation (s0, [GrammarType.Eps]),
                GrammarType.Relation (s1, [GrammarType.N aN, GrammarType.N c]),
                GrammarType.Relation (s1, [GrammarType.N s, GrammarType.N s1]),
                GrammarType.Relation (s, [GrammarType.N aN, GrammarType.N c]),
                GrammarType.Relation (s, [GrammarType.N s, GrammarType.N s1]),
                GrammarType.Relation (c, [GrammarType.N bN]),
                GrammarType.Relation (c, [GrammarType.N s, GrammarType.N bN]),
                GrammarType.Relation (aN, [GrammarType.T a]),
                GrammarType.Relation (bN, [GrammarType.T b])
                ]),
            s0
        )