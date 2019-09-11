module SMachineToGroup where

import SMType 
import GRType
import Data.Set (Set)
import qualified Data.Set as Set 
import TM2SM (alpha, omega, delta)

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

hubRelation :: [SmbR] -> [A] -> GrRelation
hubRelation word k = Relator (posPart ++ negPart)
        where 
        negation x = case x of SmbA y -> SmbA' y ; SmbA' y -> SmbA y 
        negationWord = foldl (\x y -> (negation y) : x) []
        posPart = foldl (\x (i, y) -> x ++ (case i `mod` 2 == 0 of True -> word ; False -> negationWord word) ++ [SmbA y]) [] $ zip [1..] k 
        negPart = negationWord . reverse . foldl (\x (i, y) -> x ++ (case i `mod` 2 == 0 of False -> word ; True -> negationWord word) ++ [SmbA y]) [] $ zip [1..] k

smToGR :: (SMType.SM, SMType.Word) -> GRType.GR
smToGR (SMType.SM yn 
                  qn
                  sRules, Word w0)
        = GRType.GR (a, relations)
        where 
        nk = 6
        s = [A_Y alpha, A_Y omega, A_Y delta]
        k = map (A_K $) [1 .. 2 * nk]
        ql = map Set.toList qn
        q = map (A_Q $) $ concat ql
        y = map (A_Y $) $ concat yn
        src = map (A_R $) sRules
        a = concat [s, k, y, q, src]
        transition = transitionRelations sRules ql
        auxiliary = [ Relation ([SmbA t, SmbA x], [SmbA x, SmbA t]) | x <- (s ++ y ++ k), t <- src ]
        hub = hubRelation (map smb2As w0) k
        relations = auxiliary ++ transition ++ [hub]