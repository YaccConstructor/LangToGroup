module InterpreterInputData (
    InterpreterInputData (IN), inAlphabet,  inGrammar,  inId,
    testData1,  testData2,  testData3,  testData4,  testData5, testData6, testData7, testData8) where

import GrammarType
import qualified Data.Set as Set

data InterpreterInputData = IN {
    inGrammar :: Grammar, 
    inAlphabet :: Set.Set String, 
    inId :: String
}

--data for test grammar 1
testData1 :: InterpreterInputData
testData1 = IN testGr1 alphabet1 "1"

alphabet1 :: Set.Set [Char]
alphabet1 = Set.fromList ["!", "#", "(", ")", 
    "*", "+", "-", "0", "1", "2", "3", "4", "5", "6", "7", "B", "C", "D", "F", "S", "a", "b"]
testGr1 :: Grammar
testGr1 = Grammar (Set.fromList nonterminals1,  Set.fromList [Terminal "b",  Terminal "a"], 
                    Set.fromList [relation11,  relation12,  relation13, 
                    Relation (Nonterminal "S", [T $ Terminal "a"]),
                    Relation (Nonterminal "B", [T $ Terminal "b"]),
                    Relation (Nonterminal "C", [T $ Terminal "a"]),
                    Relation (Nonterminal "D", [T $ Terminal "b"]),
                    Relation (Nonterminal "F", [T $ Terminal "a"])],
                    Nonterminal "S")
relation11 :: Relation
relation11 = BooleanRelation (Nonterminal "S",
    [PosConj [N $ Nonterminal "B", N $ Nonterminal "C"], NegConj [N $ Nonterminal "D", N $ Nonterminal "F"]])
relation12 :: Relation
relation12 = BooleanRelation (Nonterminal "S", [PosConj [N $ Nonterminal "C", N $ Nonterminal "D"]])
relation13 :: Relation
relation13 = BooleanRelation (Nonterminal "C", [PosConj [N $ Nonterminal "B", N $ Nonterminal "C"]])
nonterminals1 :: [Nonterminal]
nonterminals1 = [Nonterminal "S", Nonterminal "B", Nonterminal "C", Nonterminal "D",  Nonterminal "F"]


--data for test grammar 2
testData2 :: InterpreterInputData
testData2 = IN testGr2 alphabet2 "2"

alphabet2 :: Set.Set [Char]
alphabet2 = Set.fromList ["!", "#", "(", ")", 
                "*", "+", "-", "0", "1", "2", "3", "4", "5", "6", "7", "A", "B", "C", "D", "S", "a", "b"]
testGr2 :: Grammar
testGr2 = Grammar (Set.fromList nonterminals2, Set.fromList [Terminal "b", Terminal "a"], 
                    Set.fromList [relation21, relation22, relation23, relation24, 
                    Relation (Nonterminal "S", [T $ Terminal "a"]),
                    Relation (Nonterminal "B", [T $ Terminal "b"]),
                    Relation (Nonterminal "D", [T $ Terminal "a"]),
                    Relation (Nonterminal "C", [T $ Terminal "b"])],
                    Nonterminal "S")
relation21 :: Relation
relation21 = BooleanRelation (Nonterminal "S",
    [NegConj [N $ Nonterminal "A", N $ Nonterminal "C"], PosConj [N $ Nonterminal "B", N $ Nonterminal "C"]])
relation22 :: Relation
relation22 = Relation (Nonterminal "A", [N $ Nonterminal "D", N $ Nonterminal "B"])
relation23 :: Relation
relation23 = Relation (Nonterminal "C", [N $ Nonterminal "B", N $ Nonterminal "A"])
relation24 :: Relation
relation24 = Relation (Nonterminal "A", [N $ Nonterminal "B", N $ Nonterminal "A"])
nonterminals2 :: [Nonterminal]
nonterminals2 = [Nonterminal "S", Nonterminal "A", Nonterminal "B", Nonterminal "C", Nonterminal "D"]

--data for test grammar 3
testData3 :: InterpreterInputData
testData3 = IN testGr3 alphabet3 "3"

alphabet3 :: Set.Set [Char]
alphabet3 = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4", "5", "6", "S", "a"]
testGr3 :: Grammar
testGr3 = Grammar (Set.fromList nonterminals3, Set.fromList [Terminal "a"], 
                    Set.fromList [
                    Relation (Nonterminal "S", [T $ Terminal "a"]), 
                    Relation (Nonterminal "S", [N $ Nonterminal "S", N $ Nonterminal "S"])], 
                    Nonterminal "S")
nonterminals3 :: [Nonterminal]
nonterminals3 = [Nonterminal "S"]

--data for test grammar 4
testData4 :: InterpreterInputData
testData4 = IN testGr4 alphabet4 "4"

alphabet4 :: Set.Set [Char]
alphabet4 = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4", "5", "6", "B", "C", "D", "S", "b", "c"]
testGr4 :: Grammar
testGr4 = Grammar (Set.fromList nonterminals4, Set.fromList [Terminal "b", Terminal "c"], 
                    Set.fromList [
                    Relation (Nonterminal "B", [T $ Terminal "b"]), 
                    Relation (Nonterminal "C", [T $ Terminal "c"]), 
                    Relation (Nonterminal "S", [N $ Nonterminal "C", N $ Nonterminal "B"]), 
                    Relation (Nonterminal "B", [N $ Nonterminal "D", N $ Nonterminal "B"]), 
                    Relation (Nonterminal "D", [N $ Nonterminal "C", N $ Nonterminal "B"])], 
                    Nonterminal "S")
nonterminals4 :: [Nonterminal]
nonterminals4 = [Nonterminal "S", Nonterminal "B", Nonterminal "C", Nonterminal "D"]

--data for test grammar 5
testData5 :: InterpreterInputData
testData5 = IN testGr5 alphabet5 "5"

alphabet5 :: Set.Set [Char]
alphabet5 = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4", 
    "5", "6", "7", "B", "C", "D", "S", "b", "c"]
testGr5 :: Grammar
testGr5 = Grammar (Set.fromList nonterminals5,  Set.fromList [Terminal "b",  Terminal "c"], 
                    Set.fromList [
                    Relation (Nonterminal "D", [T $ Terminal "b"]), 
                    Relation (Nonterminal "B", [T $ Terminal "b"]), 
                    Relation (Nonterminal "C", [T $ Terminal "c"]), 
                    BooleanRelation (Nonterminal "S", [PosConj [N $ Nonterminal "B", N $ Nonterminal "C"],
                        PosConj [N $ Nonterminal "D",  N $ Nonterminal "C"]]),
                    Relation (Nonterminal "C", [N $ Nonterminal "C", N $ Nonterminal "B"]), 
                    BooleanRelation (Nonterminal "S", [PosConj [N $ Nonterminal "C", N $ Nonterminal "D"],
                        NegConj [N $ Nonterminal "D", N $ Nonterminal "B"]])],
                    Nonterminal "S")
nonterminals5 :: [Nonterminal]
nonterminals5 = [Nonterminal "S", Nonterminal "B", Nonterminal "C", Nonterminal "D"]

--common data for test grammars 6 and 7
nonterminals :: [Nonterminal]
nonterminals = [Nonterminal "S", Nonterminal "B", Nonterminal "C", Nonterminal "D",
                    Nonterminal "A", Nonterminal "A1", Nonterminal "B1", Nonterminal "B2", Nonterminal "D1", Nonterminal "C1"]

terminals :: [Terminal]
terminals = [Terminal "b",  Terminal "c", Terminal "a"]

commonRel1 :: Relation
commonRel1 = Relation (Nonterminal "A", [N $ Nonterminal "A1", N $ Nonterminal "A"])

commonRel2 :: Relation
commonRel2 = Relation (Nonterminal "B", [N $ Nonterminal "B1", N $ Nonterminal "B2"])

commonRel3 :: Relation
commonRel3 = Relation (Nonterminal "B2", [N $ Nonterminal "B", N $ Nonterminal "C1"])

commonRel4 :: Relation
commonRel4 = Relation (Nonterminal "B", [N $ Nonterminal "B1", N $ Nonterminal "C1"])

commonRel5 :: Relation
commonRel5 = Relation (Nonterminal "C", [N $ Nonterminal "C1", N $ Nonterminal "C"])

commonRel6 :: Relation
commonRel6 = Relation (Nonterminal "D", [N $ Nonterminal "A1", N $ Nonterminal "D1"])

commonRel7 :: Relation
commonRel7 = Relation (Nonterminal "D1", [N $ Nonterminal "D", N $ Nonterminal "B1"])

commonRel8 :: Relation
commonRel8 = Relation (Nonterminal "D", [N $ Nonterminal "A1", N $ Nonterminal "B1"])

commonRels :: [Relation]
commonRels = [commonRel1, commonRel2, commonRel3, commonRel4, commonRel5, commonRel6, commonRel7, commonRel8] ++
                   [Relation (Nonterminal "A", [T $ Terminal "a"]),
                    Relation (Nonterminal "A1", [T $ Terminal "a"]),
                    Relation (Nonterminal "B1", [T $ Terminal "b"]),
                    Relation (Nonterminal "C1", [T $ Terminal "c"]),
                    Relation (Nonterminal "C", [T $ Terminal "c"])]

testData6 :: InterpreterInputData
testData6 = IN testGr6 alphabet6 "6"
alphabet6 :: Set.Set [Char]
alphabet6 = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4",
    "5", "6", "7", "A","A1","B","B1","B2", "C","C1","D","D1","S", "a", "b","c"]

rel60 :: Relation
rel60 = BooleanRelation (Nonterminal "S",
    [PosConj [N $ Nonterminal "A", N $ Nonterminal "B"], NegConj [N $ Nonterminal "D", N $ Nonterminal "C"]])

testGr6 :: Grammar
testGr6 = Grammar (Set.fromList nonterminals,
                    Set.fromList terminals,
                    Set.fromList $ rel60 : commonRels,
                    Nonterminal "S")

testData7 :: InterpreterInputData
testData7 = IN testGr7 alphabet7 "7"
alphabet7 :: Set.Set [Char]
alphabet7 = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4",
    "5", "6", "A","A1","B","B1","B2", "C","C1","D","D1","S", "a", "b","c"]

rel70 :: Relation
rel70 = BooleanRelation (Nonterminal "S",
    [PosConj [N $ Nonterminal "A", N $ Nonterminal "B"], PosConj [N $ Nonterminal "D", N $ Nonterminal "C"]])

testGr7 :: Grammar
testGr7 = Grammar (Set.fromList nonterminals,
                    Set.fromList terminals,
                    Set.fromList $ rel70 : commonRels,
                    Nonterminal "S")

testData8 :: InterpreterInputData
testData8 = IN testGr8 alphabet8 "8"
alphabet8 :: Set.Set [Char]
alphabet8 = Set.fromList ["!", "#", "(", ")", "*", "+", "-", "0", "1", "2", "3", "4",
    "5", "6", "A1", "B1", "S", "S1", "a", "b"]
testGr8 :: Grammar
testGr8 = Grammar (Set.fromList nonterminals8,
                    Set.fromList terminals8,
                    Set.fromList [rel81, rel82, rel83,
                        Relation (Nonterminal "B1", [T $ Terminal "b"]),
                        Relation (Nonterminal "A1", [T $ Terminal "a"])],
                    Nonterminal "S")

nonterminals8 :: [Nonterminal]
nonterminals8 = [Nonterminal "S", Nonterminal "S1", Nonterminal "A1", Nonterminal "B1"]

terminals8 :: [Terminal]
terminals8 = [Terminal "a", Terminal "b"]

rel81 :: Relation
rel81 = Relation (Nonterminal "S", [N $ Nonterminal "A1", N $ Nonterminal "S1"])

rel82 :: Relation
rel82 = Relation (Nonterminal "S1", [N $ Nonterminal "S", N $ Nonterminal "B1"])

rel83 :: Relation
rel83 = Relation (Nonterminal "S", [N $ Nonterminal "A1", N $ Nonterminal "B1"])


