module Boolean2TMTests where

import Boolean2TM
import Interpreter
import DebuggingTMTypes
import GrammarType

import Data.Set as Set

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
    let tm = convertToTuringMachine $ boolean2tm grammar
    let states' = states $ start tm "bab" 0                        
    print states'

nonterminals1 :: [Nonterminal]
nonterminals1 = [Nonterminal "S",Nonterminal "B",Nonterminal "C",Nonterminal "D", Nonterminal "F"]

relation11 :: Relation
relation11 = Relation (Nonterminal "S", 
    [N $ Nonterminal "B", N $ Nonterminal "C",O Conjunction, O Negation, N $ Nonterminal "D", N $ Nonterminal "F"])
    
relation12 :: Relation
relation12 = Relation (Nonterminal "S", [N $ Nonterminal "C", N $ Nonterminal "D"]) 

relation13 :: Relation
relation13 = Relation (Nonterminal "C", [N $ Nonterminal "B", N $ Nonterminal "C"])
