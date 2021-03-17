module Boolean2TMTests where

import Boolean2TM
import Interpreter
import DebuggingTMTypes
import GrammarType
import TMTypes

import qualified Data.Set as Set
import Data.List as List
import Test.HUnit.Base (assertEqual)

test11 :: IO ()
test11 = do
    let grammar = testGr1
    let dtm = boolean2tm grammar
    let tm = convertToTuringMachine dtm
    print tm
    let alphabet' = alphabet1
    let states' = states $ startWithAlphabet' tm ["S","(","b","a","b",")"] 0 alphabet'
    let states'' = getStates dtm
    let statesPairs = map (\t -> (elemIndex t states'', t)) states''
    let symbols = getSymbols dtm
    let symbolsPairs = map (\t -> (elemIndex t symbols, t)) symbols
    print symbolsPairs
    print statesPairs
    mapM (\(WS _ currentState tape alphabet) -> let
            (Q index') = currentState;
            state = snd $ head $ filter (\(Just index, DState _) -> index == index') statesPairs;
            (DState stringState) = state
        in do {
            print currentState;
            print stringState;
            print tape;
            print alphabet;
            print " "}) states'
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states')

test12 :: IO ()
test12 = do
    let grammar = testGr1
    let dtm = boolean2tm grammar
    let tm = convertToTuringMachine dtm
    let alphabet' = alphabet1
    let states1 = states $ startWithAlphabet' tm ["S","(","a",")"] 0 alphabet'
    let states2 = states $ startWithAlphabet' tm ["S","(","a","b",")"] 0 alphabet'
    let states3 = states $ startWithAlphabet' tm ["S","(","b","b","b","a",")"] 0 alphabet'
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states1)
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states2)
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states3)



alphabet1 :: Set.Set [Char]
alphabet1 = Set.fromList ["!","#","(",")",
    "*","+","-","0","1","2","3","4","5","6","7","B","C","D","F","S","a","b"]

testGr1 :: Grammar
testGr1 = Grammar (Set.fromList nonterminals1, Set.fromList [Terminal "b", Terminal "a"],
                    Set.fromList [relation11, relation12, relation13,
                    Relation (Nonterminal "S", [T $ Terminal "a"]),
                    Relation (Nonterminal "B", [T $ Terminal "b"]),
                    Relation (Nonterminal "C", [T $ Terminal "a"]),
                    Relation (Nonterminal "D", [T $ Terminal "b"]),
                    Relation (Nonterminal "F", [T $ Terminal "a"])],
                    Nonterminal "S")

nonterminals1 :: [Nonterminal]
nonterminals1 = [Nonterminal "S",Nonterminal "B",Nonterminal "C",Nonterminal "D", Nonterminal "F"]

relation11 :: Relation
relation11 = Relation (Nonterminal "S", 
    [N $ Nonterminal "B", N $ Nonterminal "C",O Conjunction, O Negation, N $ Nonterminal "D", N $ Nonterminal "F"])
    
relation12 :: Relation
relation12 = Relation (Nonterminal "S", [N $ Nonterminal "C", N $ Nonterminal "D"]) 

relation13 :: Relation
relation13 = Relation (Nonterminal "C", [N $ Nonterminal "B", N $ Nonterminal "C"])
