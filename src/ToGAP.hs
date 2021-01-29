{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module ToGAP where

import GPTypes (GroupPresentation(GP))
import qualified GPTypes as GP
import SPTypes (SemigroupPresentation(SP))
import qualified SPTypes as SP
import GPGens (neg)
import Data.List (intercalate)
import qualified Set

newtype InGAP a = InGAP a

instance Show (InGAP a) => Show (InGAP [a]) where
    show (InGAP xs) = show $ map InGAP xs

instance Show (InGAP GP.Generator) where
    show (InGAP (GP.G x)) = "f." ++ show (x + 1)
    showList = (++) . intercalate ", " . map (\g -> "\"" ++ show g ++ "\"")

instance Show (InGAP GP.Element) where
    show (InGAP (GP.Positive g)) = show $ InGAP g
    show (InGAP (GP.Negative g)) = "(" ++ show (InGAP g) ++ ")^(-1)"
    showList = (++) . intercalate "*" . map show

instance Show (InGAP GP.Relation) where
    show (InGAP (ew1 `GP.Equals` ew2)) = show $ InGAP $ reverse (map neg ew1) ++ ew2
    showList = (++) . intercalate ", " . map show

instance Show (InGAP GroupPresentation) where
    show (InGAP (GP rs)) =
        "local f, g;\n" ++
        "f := FreeGroup( " ++ show (InGAP generators) ++ " );\n" ++
        "g := f / [ " ++ show (InGAP relations) ++ " ];\n" ++
        "return g;\n"
        where
            relations = Set.toList rs
            generators = map GP.G [0..maxGenInd]
            maxGenInd = Set.findMax $ Set.map getMGIinRelation rs
            getMGIinRelation (ew1 `GP.Equals` ew2) = getMGIinEWord $ ew1 ++ ew2
            getMGIinEWord = maximum . map getMGIinElement
            getMGIinElement (GP.Positive (GP.G x)) = x
            getMGIinElement (GP.Negative (GP.G x)) = x

instance Show (InGAP SP.Generator) where
    show (InGAP (SP.G x)) = "f." ++ show (x + 1)
    showList = (++) . intercalate ", " . map (\g -> "\"" ++ show g ++ "\"")

instance Show (InGAP SP.Relation) where
    show (InGAP (gw1 `SP.Equals` gw2)) =
        "[ " ++
        intercalate "*" (map (show . InGAP) gw1) ++
        ", " ++
        intercalate "*" (map (show . InGAP) gw2) ++
        " ]"
    showList = (++) . intercalate ", " . map show

instance Show (InGAP SemigroupPresentation) where
    show (InGAP (SP rs)) =
        "local f, g;\n" ++
        "f := FreeSemigroup( " ++ show (InGAP generators) ++ " );\n" ++
        "g := f / [ " ++ show (InGAP relations) ++ " ];\n" ++
        "return g;\n"
        where
            relations = Set.toList rs
            generators = map SP.G [0..maxGenInd]
            maxGenInd = Set.findMax $ Set.map getMGIinRelation rs
            getMGIinRelation (gw1 `SP.Equals` gw2) = getMGIinGWord $ gw1 ++ gw2
            getMGIinGWord = maximum . map getMGIinGenerator
            getMGIinGenerator (SP.G x) = x

toGAP :: Show (InGAP a) => a -> String
toGAP = show . InGAP
