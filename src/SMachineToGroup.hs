module SMachineToGroup where

import SMType 
import GRType 
import Data.String
import Data.Char


putInNewtypeA :: [String] -> A
putInNewtypeA s = A s

getFromNewtypeA :: A -> [String]
getFromNewtypeA (A s) = s

getFromNewtypeQ :: Q -> [String]
getFromNewtypeQ (Q s) = s

getFromNewtypeY :: Y -> [String]
getFromNewtypeY (Y s) = s

getFromNewtypeQn :: Qn -> [Q]
getFromNewtypeQn (Qn s) = s

fillK :: [String] -> Int  -> [String]
fillK s 0 = s
fillK s n = fillK (s ++ ["VectorK" ++ [intToDigit (n)]])  (n-1)

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

--Qn[Q[String]]
takeNElemFromQList l1 l 0 = l  
takeNElemFromQList (a:l1) l n = takeNElemFromQList l1 lst (n-1)
	where a' = getFromNewtypeQ a 
	      lst = merge l a'

takeNElemFromYList l1 l 0 = l  
takeNElemFromYList (a:l1) l n = takeNElemFromYList l1 lst (n-1)
	where a' = getFromNewtypeY a 
	      lst = merge l a'

smToGR :: SMType.SM -> Int -> GRType.GR
smToGR (SMType.SM (SMType.N n, 
                   SMType.Yn y, 
                   SMType.Qn q, 
                   SMType.SRules sRules)) nk
        = GRType.GR (a, GRType.Relations relations)
 	where a' = A (["alpha", "omega", "delta"])
              k = reverse (fillK [] (2*nk))
              q' = takeNElemFromQList q [] (n+1)
	      ny = (div n 5 - 3) 
 	      y' = takeNElemFromYList y [] ny
	      a = A ((getFromNewtypeA a) ++ k ++ q' ++ y')
              relations = [GRType.Relation (a, a)]
 