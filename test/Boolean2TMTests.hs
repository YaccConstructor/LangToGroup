module Boolean2TMTests where

import Boolean2TM
import Interpreter
import DebuggingTMTypes
import GrammarType
import TMTypes

import qualified Data.Set as Set
import Data.List as List
import Test.HUnit.Base (assertEqual, assertBool)
import Test.HUnit

testsWordAccepted :: Test
testsWordAccepted = let
    numbers = [2..10]
    tests1 = map (\t -> let
        tm1 = convertToTuringMachine $ boolean2tm testGr1
        word = ["S","("] ++ replicate t "b" ++ ["a", ")"]
        xs = states $ startWithAlphabet' tm1 word 0 alphabet1
        testCase = TestCase (assertEqual "Check word accepted" (Q 0) (currentState $ last xs)) in
        TestLabel ("Test for gr1 " ++ concat word) testCase) numbers
    tests2 = concatMap (\t -> let
        tm1 = convertToTuringMachine $ boolean2tm testGr1
        tm2 = convertToTuringMachine $ boolean2tm testGr2
        word = ["S","("] ++ replicate t "b" ++ ["a", "b", ")"]
        xs = states $ startWithAlphabet' tm1 word 0 alphabet1
        xs' = states $ startWithAlphabet' tm2 word 0 alphabet2
        testCase1 = TestCase (assertEqual "Check word accepted" (Q 0) (currentState $ last xs))
        testCase2 = TestCase (assertEqual "Check word accepted" (Q 0) (currentState $ last xs')) in
        [TestLabel ("Test for gr1 " ++ concat word) testCase1,
        TestLabel ("Test for gr2 " ++ concat word) testCase2]) numbers
    tests3 = map (\t -> let
        tm2 = convertToTuringMachine $ boolean2tm testGr2
        word = ["S","("] ++ replicate t "b" ++ [")"]
        xs' = states $ startWithAlphabet' tm2 word 0 alphabet2
        testCase = TestCase (assertEqual "Check word accepted" (Q 0) (currentState $ last xs')) in
        TestLabel ("Test for gr1 " ++ concat word) testCase) numbers
    tests4 = map (\t -> let
        tm3 = convertToTuringMachine $ boolean2tm testGr3
        word = ["S","("] ++ replicate t "a" ++ [")"]
        xs' = states $ startWithAlphabet' tm3 word 0 alphabet3
        testCase = TestCase (assertEqual "Check word accepted" (Q 0) (currentState $ last xs')) in
        TestLabel ("Test for gr3 " ++ concat word) testCase) numbers
    tests = tests1 ++ tests2 ++ tests3 ++ tests4
    in TestLabel "Correctness of algorithm building TM by boolean grammar:"
        $ TestList tests

test11 :: IO ()
test11 = do
    printInfo (boolean2tm testGr3) (convertToTuringMachine $ boolean2tm testGr3) alphabet3 ["S","(","a","a",")"]
    let tm = convertToTuringMachine $ boolean2tm testGr1
    let states' = states $ startWithAlphabet' tm ["S","(","b","a","b",")"] 0 alphabet1
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states')

test12 :: IO ()
test12 = do
    let tm = convertToTuringMachine $ boolean2tm testGr1
    let states1 = states $ startWithAlphabet' tm ["S","(","a",")"] 0 alphabet1
    let states2 = states $ startWithAlphabet' tm ["S","(","a","b",")"] 0 alphabet1
    let states3 = states $ startWithAlphabet' tm ["S","(","b","b","b","a",")"] 0 alphabet1
    let states4 = states $ startWithAlphabet' tm ["S","(","b"] 0 alphabet1
    let states5 = states $ startWithAlphabet' tm ["S","(","b","b","b","b",")"] 0 alphabet1
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states1)
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states2)
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states3)
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states5)
    assertBool "Check that word is accepted" (currentState (last states4) /= Q 0)

test22 :: IO()
test22 = do
    let tm = convertToTuringMachine $ boolean2tm testGr2
    let states1 = states $ startWithAlphabet' tm ["S","(","b","b","a","b",")"] 0 alphabet2
    --printInfo (boolean2tm testGr2) tm alphabet2 ["S","(","b","b","a","b",")"]
    assertEqual "Check that word is accepted" (Q 0) (currentState $ last states1)

-- print info
printInfo :: DebuggingTuringMachine -> TuringMachine -> Set.Set String -> [String] -> IO [()]
printInfo dtm tm inputAlphabet word = do
    let states' = states $ startWithAlphabet' tm word 0 inputAlphabet
    let states'' = getStates dtm
    let statesPairs = map (\t -> (elemIndex t states'', t)) states''
    let symbols = getSymbols dtm
    let symbolsPairs = map (\t -> (elemIndex t symbols, t)) symbols
    print symbolsPairs
    print statesPairs
    print tm
    mapM (\(WS _ currentState' tape' alphabet') -> let
        (Q index') = currentState';
        state = snd $ head $ filter (\(Just index, DState _) -> index == index') statesPairs;
        (DState stringState) = state
        in do {
            print currentState';
            print stringState;
            print tape';
            print alphabet';}) states'

--data for test grammar 1
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
relation11 :: Relation
relation11 = Relation (Nonterminal "S", 
    [N $ Nonterminal "B", N $ Nonterminal "C",O Conjunction, O Negation, N $ Nonterminal "D", N $ Nonterminal "F"])
relation12 :: Relation
relation12 = Relation (Nonterminal "S", [N $ Nonterminal "C", N $ Nonterminal "D"])
relation13 :: Relation
relation13 = Relation (Nonterminal "C", [N $ Nonterminal "B", N $ Nonterminal "C"])
nonterminals1 :: [Nonterminal]
nonterminals1 = [Nonterminal "S",Nonterminal "B",Nonterminal "C",Nonterminal "D", Nonterminal "F"]


--data for test grammar 2
alphabet2 :: Set.Set [Char]
alphabet2 = Set.fromList ["!","#","(",")",
                "*","+","-","0","1","2","3","4","5","6","7","A","B","C","D","S","a","b"]
testGr2 :: Grammar
testGr2 = Grammar (Set.fromList nonterminals2, Set.fromList [Terminal "b", Terminal "a"],
                    Set.fromList [relation21, relation22, relation23,
                    Relation (Nonterminal "S", [T $ Terminal "a"]),
                    Relation (Nonterminal "B", [T $ Terminal "b"]),
                    Relation (Nonterminal "D", [T $ Terminal "a"]),
                    Relation (Nonterminal "C", [T $ Terminal "b"])],
                    Nonterminal "S")
relation21 :: Relation
relation21 = Relation (Nonterminal "S",
    [O Negation, N $ Nonterminal "A", N $ Nonterminal "C", O Conjunction, N $ Nonterminal "B", N $ Nonterminal "C"])
relation22 :: Relation
relation22 = Relation (Nonterminal "A", [N $ Nonterminal "D", N $ Nonterminal "B"])
relation23 :: Relation
relation23 = Relation (Nonterminal "C", [N $ Nonterminal "B", N $ Nonterminal "A"])
nonterminals2 :: [Nonterminal]
nonterminals2 = [Nonterminal "S",Nonterminal "A", Nonterminal "B",Nonterminal "C", Nonterminal "D"]

--data for test grammar 3
alphabet3 :: Set.Set [Char]
alphabet3 = Set.fromList ["!","#","(",")","*","+","-","0","1","2","3","4","5","6","S","a"]
testGr3 :: Grammar
testGr3 = Grammar (Set.fromList nonterminals3, Set.fromList [Terminal "a"],
                    Set.fromList [
                    Relation (Nonterminal "S", [T $ Terminal "a"]),
                    Relation (Nonterminal "S", [N $ Nonterminal "S", N $ Nonterminal "S"])],
                    Nonterminal "S")
nonterminals3 :: [Nonterminal]
nonterminals3 = [Nonterminal "S"]