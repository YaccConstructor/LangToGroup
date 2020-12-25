{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module GPinGAP where

import GPTypes
import GPGens
import Data.List (intercalate)
import qualified Set

newtype InGAP a = InGAP a

instance Show (InGAP a) => Show (InGAP [a]) where
    show (InGAP xs) = show $ map InGAP xs

instance Show (InGAP Generator) where
    show (InGAP (G x)) = "f." ++ show (x + 1)
    showList = (++) . intercalate ", " . map (\x -> "\"" ++ x ++ "\"") . map show

instance Show (InGAP Element) where
    show (InGAP (Positive g)) = show $ InGAP g
    show (InGAP (Negative g)) = "(" ++ show (InGAP g) ++ ")^(-1)"
    showList = (++) . intercalate "*" . map show

instance Show (InGAP Relation) where
    show (InGAP (ew1 `Equals` ew2)) = show $ map InGAP $ reverse (map neg ew1) ++ ew2
    showList = (++) . intercalate ", " . map show

instance Show (InGAP GroupPresentation) where
    show (InGAP (GP rs)) =
        "local f, g;\n" ++
        "f := FreeGroup( " ++ show (InGAP generators) ++ " );\n" ++
        "g := f / [ " ++ show (InGAP relations) ++ " ];\n" ++
        "return g;\n"
        where
            relations = Set.toList rs
            generators = map G [0..maxGenInd]
            maxGenInd = Set.findMax $ Set.map getMGIinRelation rs
            getMGIinRelation (ew1 `Equals` ew2) = getMGIinEWord $ ew1 ++ ew2
            getMGIinEWord = maximum . map getMGIinElement
            getMGIinElement (Positive (G x)) = x
            getMGIinElement (Negative (G x)) = x
            --generators = Set.toList $ Set.unions $ Set.map getGenerators rs
            --getGenerators (ew1 `Equals` ew2) =
            --    Set.fromList $ map getGenerator $ ew1 ++ ew2
            --getGenerator (Positive g) = g
            --getGenerator (Negative g) = g

toGAP :: Show (InGAP a) => a -> String
toGAP = show . InGAP
