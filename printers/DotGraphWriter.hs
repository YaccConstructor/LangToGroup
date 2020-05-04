module DotGraphWriter where

import SMPrinter
import SMType
import System.IO
import Prelude hiding (Word)
import Text.LaTeX.Base.Render
import Text.LaTeX.Base
import Lib
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple.Utils

tex2text :: LaTeXM a -> String
tex2text = show . render . execLaTeXM

substCommandsInWord :: Word -> Word
substCommandsInWord (Word word) = Word $ snd3 $ substituteWord 0 word [] []

writeGraph :: ([(Word, Int, Word)], Map Word Int) -> Handle -> IO ()
writeGraph graph_map handle =
            do  
                hPutStr handle "digraph graphname {\n"
                hPutStr handle "node [shape=none]\n"
                mapM_ ( \x -> 
                            hPutStr handle ((fromJust $ Map.lookup x str_m) ++ " [label=" ++
                            (tex2text $ doLaTeX $ substCommandsInWord x) ++
                            "];\n") <>
                            hFlush handle) a
                mapM_ ( \(from_part, rule_i, to_part) -> 
                                hPutStr handle (
                                (fromJust $ Map.lookup from_part str_m) ++ 
                                " -> " ++
                                (fromJust $ Map.lookup to_part str_m) ++ 
                                "[label=\" " ++
                                (show rule_i) ++
                                " \"];\n") <>
                                hFlush handle) graph
                hPutStr handle "}\n"
                hFlush handle
                        where 
                                (graph, m) = graph_map
                                a = map fst $ Map.toList m
                                str_m = Map.fromList $ zip a $ map ((++) "a" . show) [1..]