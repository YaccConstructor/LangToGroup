module GapFuncWriter where

import GRType
import qualified Data.Map.Strict as Map
import SMType
import TMType

writeGenerators :: [A] -> Map.Map A String -> String
writeGenerators generators amap = genString
    where   
            toStr a =   case Map.lookup a amap of Just s -> s ; Nothing -> error $ show a
            quotes s = "\"" ++ s ++ "\""
            genString = foldl1 (\x y -> x ++ ", " ++ y) . map (quotes . toStr) $ generators

writeRelations :: [GrRelation] -> Map.Map A String -> String
writeRelations relations genmap = foldl1 (\x y -> x ++ ", " ++ y) $ map (foldl1 (\x y -> x ++ "*" ++ y) . (map printSmb) . revert) relations
    where   revertSmb smb = case smb of SmbA a -> SmbA' a ; SmbA' a -> SmbA a
            revert (Relation (from, to)) = foldl (\x y -> (revertSmb y) : x) from to
            revert (Relator smb) = smb
            printSmb (SmbA a) = case Map.lookup a genmap of Just s -> s ; Nothing -> error (show a) 
            printSmb (SmbA' a) = case Map.lookup a genmap of Just s -> "(" ++ s ++ ")^(-1)" ; Nothing -> error (show a)

writeGap :: GR -> String
writeGap (GR (generators, relations)) = foldl1 (\x y -> x ++ "\n" ++ y) [f, g, p, tzgo]
    where   genmap = Map.fromList $ zip generators $ map ((++) "f." . show) [1..]
            generatorsString = writeGenerators generators genmap
            relationsString = writeRelations relations genmap
            f = "f := FreeGroup( " ++ generatorsString ++ " );"
            g = "g := f / [ " ++ relationsString ++ " ];"
            p = "p := PresentationFpGroup( g );"
            tzgo = "SimplifyPresentation( p );"