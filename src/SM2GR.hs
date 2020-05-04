module SM2GR where

import SMType 
import GRType
import TM2SMHelpers
import qualified Data.Set as Set 

smb2As :: Smb -> SmbR
smb2As smb = case smb of SmbY y -> SmbA $ A_Y y ; SmbY' y -> SmbA' $ A_Y y ; SmbQ q -> SmbA $ A_Q q

transitionRelations :: [SRule] -> [[State]] -> [GrRelation]
transitionRelations rules states = cmdQs ++ cmdRs
        where 
        powtau x tau = (SmbA' $ A_R tau) : x ++ [SmbA $ A_R tau]
        cmdRs = concatMap (\r@(SRule x) -> map (\(Word u, Word v) -> Relation (powtau (map smb2As u) r, map smb2As v)) x) rules 
        us = map smb2As . concat . concatMap (map ((\(SMType.Word w) -> w) . fst) . (\(SRule x) -> x)) $ rules                
        cmdQs = foldl (\x y -> case any (\q -> elem (SmbA $ A_Q q) us) y of 
                                True -> x 
                                False -> [ Relation (powtau [SmbA $ A_Q q] r, [SmbA $ A_Q q]) | r <- rules, q <- y ] ++ x
                                ) [] states              

nk :: Int
nk = 1
k :: [A]
k = map (A_K $) [1 .. 2 * nk]

hubRelation :: SMType.Word -> [SmbR]
hubRelation (Word w0) = posPart ++ negPart
        where 
        word = map smb2As w0
        negation x = case x of SmbA y -> SmbA' y ; SmbA' y -> SmbA y 
        negationWord = foldl (\x y -> (negation y) : x) []
        posPart = foldl (\x (i, y) -> x ++ (if i `mod` 2 == 0 then word else negationWord word) ++ [SmbA y]) [] $ zip [1..] k 
        negPart = negationWord . foldl (\x (i, y) -> x ++ [SmbA y] ++ (case i `mod` 2 == 0 of False -> word ; True -> negationWord word)) [] $ reverse $ zip [1..] k

easyHubRelation :: SMType.Word -> [SmbR]
easyHubRelation (Word w0) = [SmbA $ (!!) k 0] ++ (map smb2As w0) ++ [SmbA $ (!!) k 1]

sm2grInternal :: (SMType.SM, SMType.Word) -> [A] -> GRType.GR
sm2grInternal (SMType.SM ys
                  qs
                  sRules, w0) s
        = GRType.GR (Set.fromList a, Set.fromList relations)
        where
        ql = map Set.toList qs
        q = map (A_Q $) $ concat ql
        y = map (A_Y $) $ concat ys
        src = map (A_R $) sRules
        a = concat [s, k, y, q, src]
        transition = transitionRelations sRules ql
        auxiliary = [ Relation ([SmbA t, SmbA x], [SmbA x, SmbA t]) | x <- (s ++ y ++ k), t <- src ]
        hub = Relator $ hubRelation w0
        --hub = Relator $ easyHubRelation w0
        relations = auxiliary ++ transition ++ [hub]

sm2gr :: (SMType.SM, SMType.Word) -> GRType.GR
sm2gr sw = sm2grInternal sw [A_Y alphaVal, A_Y omegaVal, A_Y deltaVal]

sm2grEmpty :: (SMType.SM, SMType.Word) -> GRType.GR
sm2grEmpty sw = sm2grInternal sw []