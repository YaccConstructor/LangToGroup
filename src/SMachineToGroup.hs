module SMachineToGroup where

import SMType 
import GRType
import Data.Set (Set)
import qualified Data.Set as Set 
import Data.String
import Data.Char
import Data.List

transitionRelations :: [SRule] -> [[State]] -> [Relation]
transitionRelations rules states = cmdQs ++ cmdRs
        where 
                powtau x tau = (SmbA' $ A_R tau) : x ++ [SmbA $ A_R tau]
                smb2As smb = case smb of SmbY y -> SmbA $ A_Y y ; SmbY' y -> SmbA' $ A_Y y ; SmbQ q -> SmbA $ A_Q q 
                cmdRs = concat $ map (\r@(SRule x) -> map (\(Word u, Word v) -> Relation (powtau (map smb2As u) r, map smb2As v)) x) rules 
                us = map smb2As . concat $ map (map (fst) . (\(SRule x) -> x)) rules                
                cmdQs = foldl (\x y -> case any (elem (SmbA . A_Q $) us) y of 
                                        True -> x 
                                        False -> [ Relation (powtau [SmbA $ A_Q q] r, [SmbA $ A_Q q]) | r <- rules, q <- y ] ++ x
                                        ) [] states

k_w :: Word -> Int -> Int -> [A] -> [ASymb] -> A
k_w w i n [] x = W x
k_w w i n ((W [k]):ks) x = 
        if (i `mod` 2 == 0) then 
                k_w w (i+1) n ks (x ++ [(A_W w), k])
        else
                k_w w (i+1) n ks (x ++ [(A_W' w), k]) 

k_w' :: Word -> Int -> [A] -> [ASymb] -> A
k_w' w 0 [] x = W' x
k_w' w n ((W [k]):ks) x = 
        if (n `mod` 2 == 0) then 
                k_w' w (n-1) ks ([k, (A_W w)] ++ x)
        else
                k_w' w (n-1) ks ([k, (A_W' w)] ++ x)                 

hubRelation :: Word -> Int -> [A] -> Relation
hubRelation word n k = Relation(w, [W [(A_S "1")]])
        where w = [(k_w word 1 n k []), (k_w' word n k [])]


smToGR :: SMType.SM -> Int -> Word -> GRType.GR
smToGR (SMType.SM yn 
                  qn
                  sRules) nk
        = GRType.GR (a, relations)
        where s = [A_S "alpha", A_S "omega", A_S "delta"]
              k = map (A_K $) [1 .. 2 * nk]
              ql = map Set.toList qn
              q = map (A_Q $) $ concat ql
              y = map (A_Y $) $ concat yn
              src = map (A_R $) sRules
              a = concat [s, k, y, q, src]
              transition = transitionRelations sRules ql
              auxiliary = [ Relation ([SmbA t, SmbA x], [SmbA x, SmbA t]) | x <- (s ++ y ++ k), t <- src ]
              w0 = 
              hub = hubRelation w (2 * nk) k
              relations = auxiliary ++ transition ++ [hub]
 