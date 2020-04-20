module SMachineToGroup where

import SMType 
import GRType
import TM2SMHelpers
import Data.Set (Set)
import qualified Data.Set as Set 

smb2As :: Smb -> SmbR
smb2As smb = case smb of SmbY y -> SmbA $ A_Y y ; SmbY' y -> SmbA' $ A_Y y ; SmbQ q -> SmbA $ A_Q q

transitionRelations :: [SRule] -> [[State]] -> [GrRelation]
transitionRelations rules states = cmdQs ++ cmdRs
        where 
        powtau x tau = (SmbA' $ A_R tau) : x ++ [SmbA $ A_R tau]
        cmdRs = concat $ map (\r@(SRule x) -> map (\(Word u, Word v) -> Relation (powtau (map smb2As u) r, map smb2As v)) x) rules 
        us = map smb2As . concat . concat $ map (map ((\(SMType.Word w) -> w) . fst) . (\(SRule x) -> x)) rules                
        cmdQs = foldl (\x y -> case any (\q -> elem (SmbA $ A_Q q) us) y of 
                                True -> x 
                                False -> [ Relation (powtau [SmbA $ A_Q q] r, [SmbA $ A_Q q]) | r <- rules, q <- y ] ++ x
                                ) [] states              

nk = 1
k = map (A_K $) [1 .. 2 * nk]

hubRelation :: SMType.Word -> [SmbR]
hubRelation (Word w0) = posPart ++ negPart
        where 
        word = map smb2As w0
        negation x = case x of SmbA y -> SmbA' y ; SmbA' y -> SmbA y 
        negationWord = foldl (\x y -> (negation y) : x) []
        posPart = foldl (\x (i, y) -> x ++ (case i `mod` 2 == 0 of True -> word ; False -> negationWord word) ++ [SmbA y]) [] $ zip [1..] k 
        negPart = negationWord . foldl (\x (i, y) -> x ++ [SmbA y] ++ (case i `mod` 2 == 0 of False -> word ; True -> negationWord word)) [] $ reverse $ zip [1..] k

easyHubRelation (Word w0) = [SmbA $ (!!) k 0] ++ (map smb2As w0) ++ [SmbA $ (!!) k 1]

smToGRInternal :: (SMType.SM, SMType.Word) -> [A] -> GRType.GR
smToGRInternal (SMType.SM yn 
                  qn
                  sRules, w0) s
        = GRType.GR (Set.fromList a, Set.fromList relations)
        where
        ql = map Set.toList qn
        q = map (A_Q $) $ concat ql
        y = map (A_Y $) $ concat yn
        src = map (A_R $) sRules
        a = concat [s, k, y, q, src]
        transition = transitionRelations sRules ql
        auxiliary = [ Relation ([SmbA t, SmbA x], [SmbA x, SmbA t]) | x <- (s ++ y ++ k), t <- src ]
        hub = Relator $ hubRelation w0
        --hub = Relator $ easyHubRelation w0
        relations = auxiliary ++ transition ++ [hub]

smToGR :: (SMType.SM, SMType.Word) -> GRType.GR
smToGR sw = smToGRInternal sw [A_Y alpha, A_Y omega, A_Y delta]

smToGREmpty :: (SMType.SM, SMType.Word) -> GRType.GR
smToGREmpty sw = smToGRInternal sw []