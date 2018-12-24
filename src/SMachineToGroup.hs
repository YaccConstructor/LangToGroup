module SMachineToGroup where

import SMType 
import GRType
import Data.Set (Set)
import qualified Data.Set as Set 
import Data.String
import Data.Char
import Data.List

import Prelude hiding (Word)

intToString :: Int -> String
intToString 0 = ""
intToString n = intToString (div n 10) ++ [intToDigit (mod n 10)]

fillK :: [A] -> Int  -> [A]
fillK s 0 = s
fillK s n = fillK ([W [(A_K ("k" ++ intToString (n)))]] ++ s)  (n-1)

mergeQ :: [State String] -> [A] -> [A]
mergeQ [] ys = ys
mergeQ (x : xs) [] = (W [(A_Q x)]) : (mergeQ xs [])
mergeQ (x : xs) (y : ys) = (W [(A_Q x)]) : y : (mergeQ xs ys)

mergeY :: [Y] -> [A] -> [A]
mergeY [] ys = ys
mergeY (x : xs) [] = (W [(A_Y x)]) : (mergeY xs [])
mergeY (x : xs) (y : ys) = (W [(A_Y x)]) : y : (mergeY xs ys)

takeNElemFromQList :: [[State String]] -> [A] -> Int -> [A]
takeNElemFromQList l1 l 0 = l  
takeNElemFromQList (a:l1) l n = takeNElemFromQList l1 lst (n-1)
        where lst = mergeQ a l
takeNElemFromQList l1 l _ = l

takeNElemFromYList :: [[Y]] -> [A] -> Int -> [A]
takeNElemFromYList l1 l 0 = l  
takeNElemFromYList (a:l1) l n = takeNElemFromYList l1 lst (n-1)
        where lst = mergeY a l

sRuleListToA_RList :: [SRule] -> [A] 
sRuleListToA_RList [] = []
sRuleListToA_RList (x : xs) = (W [(A_R x)]) : (sRuleListToA_RList xs)

auxiliaryRelations :: [A] -> [A] -> [Relation]
auxiliaryRelations xs ts 
        = [(Relation ([t, x], [x, t])) | x <- xs, t <- ts]

--f xs ts  = [([t, x], [x, t]) | x <- xs, t <- ts]

getUfromSRules :: [(Word, Word)] -> [Word]
getUfromSRules [] = []
getUfromSRules ((u, v) : xs) = u : getUfromSRules xs

isWordElem :: [State String] -> Word -> Bool
isWordElem [] (Word u) = False
isWordElem (q : qs) (Word u) = 
         if (elem (SmbQ q) u) then 
             True
         else 
             isWordElem qs (Word u)

qRel :: [State String] -> SRule -> [Relation]
qRel [] t = []
qRel (q:qs) t = [Relation ([W [A_R' t], W [A_Q q], W [A_R t]], [W [A_Q q]])] 
                  ++ (qRel qs t)


checkQforTR :: [State String] -> [Word] -> Bool -> SRule -> [Relation] 
checkQforTR q u False t = []
checkQforTR q [] True t = qRel q t
checkQforTR q (u : us) True t = 
        if (isWordElem q u) then 
                []
        else
                checkQforTR q us True t 

genQRelations :: [[State String]] -> [Word] -> SRule -> [Relation]
genQRelations [] u t = []
genQRelations (q : qs) u t = x ++ genQRelations qs u t
        where x = checkQforTR q u True t 

toRel :: [(Word, Word)] -> SRule -> [Relation]
toRel [] rule = []
toRel ((u, v) : xs) rule = (Relation
        ([W [(A_R'(rule))], W [(A_W u)], 
        W [(A_R (rule))]], [W [(A_W v)]]))
        : toRel xs rule

transitionRelations :: [A] -> [[State String]] -> [Relation]
transitionRelations [] q = []
transitionRelations ((W [(A_R (SRule t))]) : ts) q = rel ++ transitionRelations ts q
        where u = getUfromSRules t
              rel = (toRel t (SRule t)) ++ sort (genQRelations q u (SRule t))

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
                  sRules) nk w
        = GRType.GR (a, relations)
        where s = ([W [(A_S "alpha")], W [(A_S "omega")], W [(A_S "delta")]])
              k = fillK [] (2 * nk)
              n = length qn
              q = sort (takeNElemFromQList qn [] (n+1))
              ny = (div n 5 - 3) 
              y = sort (takeNElemFromYList yn [] ny)
              src = sRuleListToA_RList sRules
              a = s ++ k ++ y ++ q ++ src
              auxiliary = auxiliaryRelations (s ++ y ++ k) src
              transition = transitionRelations src qn
              hub = hubRelation w (2 * nk) k
              relations = auxiliary ++ transition ++ [hub]
 