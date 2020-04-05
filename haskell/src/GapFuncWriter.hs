module GapFuncWriter where

import GRType
import qualified Data.Map.Strict as Map
import SMType
import TMType
import System.IO
import Data.Set (Set)
import qualified Data.Set as Set 
import Helpers

writeGenerators :: [A] -> Map.Map A String -> Handle -> IO ()
writeGenerators generators amap handle = 
    do 
        hPutStr handle $ head mdata
        mapM_ ( \x -> hPutStr handle ", " <> hPutStr handle x) (tail mdata)
        where   toStr a =   case Map.lookup a amap of Just s -> s ; Nothing -> error $ show a
                quotes s = "\"" ++ s ++ "\""                
                mdata = map (quotes . toStr) $ generators

writeRelations :: [GrRelation] -> Map.Map A String -> Handle -> IO()
writeRelations relations genmap handle =
    do
        hPutStr handle $ head mdata
        mapM_ ( \x -> hPutStr handle ", " <> hPutStr handle x) (tail mdata)
    where   revertSmb smb = case smb of SmbA a -> SmbA' a ; SmbA' a -> SmbA a
            revert (Relation (from, to)) = foldl (\x y -> (revertSmb y) : x) from to
            revert (Relator smb) = smb
            mdata = map (foldl1 (\x y -> x ++ "*" ++ y) . (map (printSmb genmap)) . revert) relations

writeGap :: GR -> Handle -> Map.Map A String -> IO ()
writeGap (GR (g, r)) handle genmap =     
--            do  hPutStr handle "local f, g, p;\n"
            do  hPutStr handle "local f, g;\n"
                hPutStr handle "f := FreeGroup( "
                writeGenerators generators genmap handle
                hPutStr handle " );\n"
                hFlush handle
                hPutStr handle "g := f / [ "
                writeRelations relations genmap handle
                hPutStr handle " ];\n"
                hFlush handle
                --hPutStr handle "p := SimplifiedFpGroup( g );\n"
                --hPutStr handle "return p;\n"
                hPutStr handle "return g;\n"

                    where 
                        generators = Set.toList g
                        relations = Set.toList r

writeWord :: [SmbR] -> Handle -> Map.Map A String -> IO()
writeWord as handle genmap = 
            do  hPutStr handle mdata
                hPutStr handle ";"
                hFlush handle
            where 
                printSmb (SmbA a) = case Map.lookup a genmap of Just s -> s ; Nothing -> error (show a) 
                printSmb (SmbA' a) = case Map.lookup a genmap of Just s -> "(" ++ s ++ ")^(-1)" ; Nothing -> error (show a)
                mdata = foldl1 (\x y -> x ++ "*" ++ y) $ map printSmb as