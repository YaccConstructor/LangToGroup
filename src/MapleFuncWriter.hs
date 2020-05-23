module MapleFuncWriter where

import GRType
import qualified Data.Map.Strict as Map
import System.IO
import qualified Data.Set as Set 
import Helpers


writeGenerators :: [A] -> Map.Map A String -> Handle -> IO ()
writeGenerators generators amap handle = 
    do 
        hPutStr handle $ head mdata
        mapM_ ( \x -> hPutStr handle ", " <> hPutStr handle x) (tail mdata)
        where   toStr a =   case Map.lookup a amap of Just s -> s ; Nothing -> error $ show a                
                mdata = map toStr generators

writeRelations :: [GrRelation] -> Map.Map A String -> Handle -> IO()
writeRelations relations genmap handle =
    do
        hPutStr handle $ head mdata
        mapM_ ( \x -> hPutStr handle ", " <> hPutStr handle x) (tail mdata)
    where   
            mdata = map (foldl1 (\x y -> x ++ "." ++ y) . (map (printSmb genmap)) . revertRel) relations

writeMaple :: GR -> Handle -> Map.Map A String -> IO ()
writeMaple (GR (g, r)) handle genmap =
            do  hPutStr handle "with( GroupTheory ):\n"
                hPutStr handle "G := < "
                writeGenerators generators genmap handle
                hPutStr handle " | "
                hFlush handle
                writeRelations relations genmap handle
                hPutStr handle " = 1 >;\n"
                hFlush handle

                    where 
                        generators = Set.toList g
                        relations = Set.toList r