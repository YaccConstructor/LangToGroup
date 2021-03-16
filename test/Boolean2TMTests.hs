module Boolean2TMTests where

import Boolean2TM
import Interpreter
import DebuggingTMTypes
import GrammarType
import TMTypes
import Boolean2TMConstants as Constants
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.List as List

test1 :: IO ()
test1 = do
    let grammar = Grammar (Set.fromList nonterminals1, Set.fromList [Terminal "b", Terminal "a"],
                            Set.fromList [relation11, relation12, relation13,
                                Relation (Nonterminal "S", [T $ Terminal "a"]),
                                Relation (Nonterminal "B", [T $ Terminal "b"]),
                                Relation (Nonterminal "C", [T $ Terminal "a"]),
                                Relation (Nonterminal "D", [T $ Terminal "b"]),
                                Relation (Nonterminal "F", [T $ Terminal "a"])],
                            Nonterminal "S")
    let dtm = boolean2tm grammar
    let tm = convertToTuringMachine dtm
    print tm
    --let alphabet' = Set.fromList [" ","+","-",")","a","b","#","*","(","C","D","1","6","2","3","4","5","B","F","S","0","!"]
    let alphabet' = Set.fromList ["!","#",Constants.leftBracket,")","*","+","-","0","1","2","3","4","5","6","B","C","D","F","S","a","b"]
    let states' = states $ startWithAlphabet' tm ["S",Constants.leftBracket,"b","a","b",")"] 0 alphabet'
    let states'' = getStates dtm
    let statesPairs = map (\t -> (elemIndex t states'', t)) states''
    let symbols = getSymbols dtm
    let symbolsPairs = map (\t -> (elemIndex t symbols, t)) symbols
    print symbolsPairs
    print statesPairs
    mapM (\(WS _ currentState tape alphabet) -> let
            (Q index') = currentState;
            state = snd $ head $ filter (\(Just index, DState s) -> index == index') statesPairs;
            (DState stringState) = state 
        in do {
            print currentState;
            print stringState;
            print tape;
            print alphabet;
            print " "}) states'
    print $ [Constants.leftBracket] ++ ["1","2"]    
    print $ length states'

nonterminals1 :: [Nonterminal]
nonterminals1 = [Nonterminal "S",Nonterminal "B",Nonterminal "C",Nonterminal "D", Nonterminal "F"]

relation11 :: Relation
relation11 = Relation (Nonterminal "S", 
    [N $ Nonterminal "B", N $ Nonterminal "C",O Conjunction, O Negation, N $ Nonterminal "D", N $ Nonterminal "F"])
    
relation12 :: Relation
relation12 = Relation (Nonterminal "S", [N $ Nonterminal "C", N $ Nonterminal "D"]) 

relation13 :: Relation
relation13 = Relation (Nonterminal "C", [N $ Nonterminal "B", N $ Nonterminal "C"])
