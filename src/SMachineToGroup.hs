module SMachineToGroup where

import SMType 
import GRType 
import Data.String
import Data.Char


getFromNewtypeA :: A -> [String]
getFromNewtypeA (A s) = s

getFromNewtypeQ :: Q -> String
getFromNewtypeQ (Q s) = s

getFromNewtypeY :: Y -> String
getFromNewtypeY (Y s) = s

fillK :: [String] -> Int  -> [String]
fillK s 0 = s
fillK s n = fillK (s ++ ["VectorK" ++ [intToDigit (n)]])  (n-1)

mergeQ :: [Q] -> [String] -> [String]
mergeQ [] ys = ys
mergeQ ((Q x):xs) [] = x : (mergeQ xs [])
mergeQ ((Q x):xs) (y : ys) = x : y : (mergeQ xs ys)

mergeY :: [Y] -> [String] -> [String]
mergeY [] ys = ys
mergeY ((Y x):xs) [] = x : (mergeY xs [])
mergeY ((Y x):xs) (y : ys) = x : y : (mergeY xs ys)

--[[Q String]]
--takeNElemFromQList :: [[Q]] -> [String] -> Int -> [String]
takeNElemFromQList l1 l 0 = l  
takeNElemFromQList (a:l1) l n = takeNElemFromQList l1 lst (n-1)
	where lst = mergeQ a l

takeNElemFromYList l1 l 0 = l  
takeNElemFromYList (a:l1) l n = takeNElemFromYList l1 lst (n-1)
	where lst = mergeY a l

smbToString :: Smb -> String
smbToString (SmbY (Y x)) = x
smbToString (SmbY' (Y x)) = x
smbToString (SmbQ (Q x)) = x 

smbListToStringList :: [Smb] -> [String]
smbListToStringList [] = [] 
smbListToStringList (x : xs) = (smbToString x) : smbListToStringList xs

pairWordListToStringList :: [(Word, Word)] -> [String]
pairWordListToStringList [] = []
pairWordListToStringList ((Word x, Word y) : xs) 
			= (smbListToStringList x) 
			++ (smbListToStringList y) 
			++ pairWordListToStringList xs 

sRuleListToStringList :: [SRule] -> [String] 
sRuleListToStringList [] = []
sRuleListToStringList ((SRule x) : xs) = l ++ (sRuleListToStringList xs)
		where l = pairWordListToStringList x


smToGR :: SMType.SM  -> Int -> GRType.GR
smToGR (SMType.SM (N n, 
		  yn, 
		  qn, 
		  sRules)) nk
        = GRType.GR (a, GRType.Relations relations)
 	where a' = A (["alpha", "omega", "delta"])
              k = reverse (fillK [] (2*nk))
              q = takeNElemFromQList qn [] (n+1)
	      ny = (div n 5 - 3) 
 	      y = takeNElemFromYList yn [] ny
	      src = sRuleListToStringList sRules
	      a = A ((getFromNewtypeA a) ++ k ++ q ++ y ++ src)
              relations = [GRType.Relation ([a], [a])]
 