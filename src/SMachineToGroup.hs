module SMachineToGroup where

import SMType 
import GRType 
import Data.String
import Data.Char
import Data.List

intToString :: Int -> String
intToString 0 = ""
intToString n = intToString (div n 10) ++ [intToDigit (mod n 10)]

fillK :: [A] -> Int  -> [A]
fillK s 0 = s
fillK s n = fillK ([A_K ("VectorK" ++ intToString (n))] ++ s)  (n-1)

mergeQ :: [Q] -> [A] -> [A]
mergeQ [] ys = ys
mergeQ (x : xs) [] = (A_Q x) : (mergeQ xs [])
mergeQ (x : xs) (y : ys) = (A_Q x) : y : (mergeQ xs ys)

mergeY :: [Y] -> [A] -> [A]
mergeY [] ys = ys
mergeY (x : xs) [] = (A_Y x) : (mergeY xs [])
mergeY (x : xs) (y : ys) = (A_Y x) : y : (mergeY xs ys)

takeNElemFromQList :: [[Q]] -> [A] -> Int -> [A]
takeNElemFromQList l1 l 0 = l  
takeNElemFromQList (a:l1) l n = takeNElemFromQList l1 lst (n-1)
	where lst = mergeQ a l

takeNElemFromYList :: [[Y]] -> [A] -> Int -> [A]
takeNElemFromYList l1 l 0 = l  
takeNElemFromYList (a:l1) l n = takeNElemFromYList l1 lst (n-1)
	where lst = mergeY a l

sRuleListToA_RList :: [SRule] -> [A] 
sRuleListToA_RList [] = []
sRuleListToA_RList (x : xs) = (A_R x) : (sRuleListToA_RList xs)

auxiliaryRelations :: [A] -> [A] -> [Relation]
auxiliaryRelations xs ts 
	= [(Relation ([t, x], [x, t])) | x <- xs, t <- ts]

--f xs ts  = [([t, x], [x, t]) | x <- xs, t <- ts]

getUfromSRules :: [(Word, Word)] -> [Word]
getUfromSRules [] = []
getUfromSRules ((u, v) : xs) = u : getUfromSRules xs

smbToString :: Smb -> String
smbToString (SmbY (Y x)) = x
smbToString (SmbY' (Y x)) = x
smbToString (SmbQ (Q x)) = x 

smbListToStringList :: [Smb] -> String
smbListToStringList [] = [] 
smbListToStringList (x : xs) = (smbToString x) ++ smbListToStringList xs

isWordElem :: A -> Word -> Bool
isWordElem (A_Q (Q q)) (Word u) = isInfixOf q u'
		where u' = smbListToStringList (u)

checkQforTR :: A -> [Word] -> Bool -> SRule -> [Relation] -> [Relation]
checkQforTR q [] False t ans = []
checkQforTR q [] True t ans = ans
checkQforTR q (u : us) _ t ans = 
	if (isWordElem q u) then 
		checkQforTR q [] False t ans
	else 	
		checkQforTR q us True t (x:ans)
			where x = Relation ([A_R' t, q, A_R t], [q])

genQRelations :: [A] -> [Word] -> SRule -> [Relation]
genQRelations [] u t = []
genQRelations (q : qs) u t = x ++ genQRelations qs u t
	where x = checkQforTR q u True t []

toRel :: [(Word, Word)] -> [Relation]
toRel [] = []
toRel ((u, v) : xs) = (Relation
	([A_R'(SRule ((u, v) : xs)), A_W u, 
	A_R (SRule ((u, v) : xs))], [A_W v]))
	: toRel xs

transitionRelations :: [A] -> [A] -> [Relation]
transitionRelations [] q = []
transitionRelations ((A_R (SRule t)) : ts) q = rel ++ transitionRelations ts q
	where u = getUfromSRules t
	      rel = (toRel t) ++ (genQRelations q u (SRule t))


smToGR :: SMType.SM -> Int -> GRType.GR
smToGR (SMType.SM (N n, 
		  yn, 
		  qn, 
		  sRules)) nk
        = GRType.GR (a, relations)
 	where s = ([A_S "alpha", A_S "omega", A_S "delta"])
              k = fillK [] (2*nk)
              q = takeNElemFromQList qn [] (n+1)
	      ny = (div n 5 - 3) 
 	      y = takeNElemFromYList yn [] ny
	      src = sRuleListToA_RList sRules
	      a = s ++ k ++ q ++ y
	      auxiliary = auxiliaryRelations (s ++ y ++ k) src
	      transition = transitionRelations src q
              relations = auxiliary ++ transition 
 