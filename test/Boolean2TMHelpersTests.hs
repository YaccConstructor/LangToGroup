module Boolean2TMHelpersTests where

import Test.HUnit
import GrammarType
import DebuggingTMTypes
import qualified Boolean2TMHelpers as Helpers
import qualified Data.Set as Set
import qualified Data.Map as Map

-- tests written for boolean grammars in normal form
calculateNextConjunctionInSameRuleTest1 :: IO ()
calculateNextConjunctionInSameRuleTest1 = do
  let gr = Grammar (
        Set.fromList [Nonterminal "Abc",Nonterminal "Cr",Nonterminal "D",Nonterminal "S"],
        Set.fromList [Terminal "b"],
        Set.fromList [
        Relation (Nonterminal "S",[N $ Nonterminal "D", N $ Nonterminal "Cr",
            O Conjunction, N $ Nonterminal "S", N $ Nonterminal "Abc"])
        ],
        Nonterminal "S")
  let testConjunctionPair = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 0, False, N $ Nonterminal "D", N $ Nonterminal "Cr")
  let expectedConjunctionPair = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 0, False, N $ Nonterminal "S", N $ Nonterminal "Abc")
  let actualConjunctionPair = Helpers.calculateNextConjunctionInSameRule gr testConjunctionPair
  case actualConjunctionPair of
        Nothing -> assertFailure "In relation no such conjunction, or next conjunction does not exist"
        Just pair -> assertEqual "assert getting next conjunction" expectedConjunctionPair  pair  
  let testConjunctionPair1 = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 0, False, N $ Nonterminal "S", N $ Nonterminal "Abc")
  let expectedConjunctionPair1 = Nothing
  let actualConjunctionPair1 = Helpers.calculateNextConjunctionInSameRule gr testConjunctionPair1
  case actualConjunctionPair1 of
        Nothing -> assertEqual "assert getting next conjunction" actualConjunctionPair1 expectedConjunctionPair1
        Just _ -> assertFailure "Next conjunction does not exist, but result is not Nothing"
    
    
calculateNextConjunctionInSameRuleTest2 :: Assertion
calculateNextConjunctionInSameRuleTest2 = do
  let gr = Grammar (
        Set.fromList [Nonterminal "S",Nonterminal "Sa", Nonterminal "Sb"],
        Set.fromList [Terminal "b"],
        Set.fromList [
            Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "S",
                O Conjunction, O Negation, N $ Nonterminal "Sa", N $ Nonterminal "Sb"]),
            Relation (Nonterminal "S",[N $ Nonterminal "Sb", N $ Nonterminal "Sa"])
        ],
        Nonterminal "S")
  let testConjunctionPair1 = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 1, False, N $ Nonterminal "Sb", N $ Nonterminal "Sa")
  let actualConjunctionPair1 = Helpers.calculateNextConjunctionInSameRule gr testConjunctionPair1
  case actualConjunctionPair1 of
        Nothing -> assertEqual "assert getting next conjunction" actualConjunctionPair1 Nothing 
        Just _ -> assertFailure "Next conjunction does not exist, but result is not Nothing"  
  let testConjunctionPair2 = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 0, False, N $ Nonterminal "Sa", N $ Nonterminal "S")
  let actualConjunctionPair2 = Helpers.calculateNextConjunctionInSameRule gr testConjunctionPair2
  let expectedConjunctionPair2 = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 0, True, N $ Nonterminal "Sa", N $ Nonterminal "Sb")
  case actualConjunctionPair2 of
        Just pair -> assertEqual "assert getting next conjunction in the same rule" pair expectedConjunctionPair2
        Nothing -> assertFailure "Next conjunction does not exist, but result is not Nothing"

calculateFirstConjunctionInNextRuleTest1 :: Assertion
calculateFirstConjunctionInNextRuleTest1 = do
  let gr = Grammar (
        Set.fromList [Nonterminal "S",Nonterminal "Sa", Nonterminal "Sb", Nonterminal "Sc"],
        Set.fromList [Terminal "b"],
        Set.fromList [
            Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "S", O Conjunction,
                O Negation, N $ Nonterminal "Sa", N $ Nonterminal "Sb"]),
            Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "Sd", O Conjunction,
                N $ Nonterminal "Sb", N $ Nonterminal "Sc"])
        ],
        Nonterminal "S")
  let testConjunctionPair1 = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 0, False, N $ Nonterminal "Sa", N $ Nonterminal "S")
  let testConjunctionPair2 = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 0, True, N $ Nonterminal "Sa", N $ Nonterminal "Sb")

  let actualConjunctionPair1 = Helpers.calculateFirstConjunctionInNextRule gr testConjunctionPair1
  let actualConjunctionPair2 = Helpers.calculateFirstConjunctionInNextRule gr testConjunctionPair2

  let expectedConjunctionPair = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 1, False, N $ Nonterminal "Sa", N $ Nonterminal "Sd")

  case actualConjunctionPair1 of
        Nothing -> assertFailure "Next conjunction does not exist"
        Just pair -> assertEqual "assert getting first conjunction in next rule" expectedConjunctionPair pair
  case actualConjunctionPair2 of
        Nothing -> assertFailure "Next conjunction does not exist"
        Just pair -> assertEqual "assert getting first conjunction in next rule" expectedConjunctionPair pair

calculateFirstConjunctionInNextRuleTest2 :: Assertion
calculateFirstConjunctionInNextRuleTest2 = do
  let gr = Grammar (
        Set.fromList [Nonterminal "S",Nonterminal "Sa", Nonterminal "Sb", Nonterminal "Sc"],
        Set.fromList [Terminal "b"],
        Set.fromList [
            Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "S", O Conjunction,
                O Negation, N $ Nonterminal "Sa", N $ Nonterminal "Sb"]),
            Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "S", O Conjunction,
                N $ Nonterminal "Sb", N $ Nonterminal "Sc"]),
            Relation (Nonterminal "S", [O Negation, N $ Nonterminal "Sb", N $ Nonterminal "Sa"])
            ],
        Nonterminal "S")
  let testConjunctionPair = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 1, False, N $ Nonterminal "Sa", N $ Nonterminal "S")
  let expectedConjunctionPair = DebuggingTMTypes.SymbolsPair (
        Nonterminal "S", 2, True, N $ Nonterminal "Sb", N $ Nonterminal "Sa")
  let actualConjunctionPair = Helpers.calculateFirstConjunctionInNextRule gr testConjunctionPair
  case actualConjunctionPair of
          Nothing -> assertFailure "Next conjunction does not exist"
          Just pair -> assertEqual "assert getting first conjunction in next rule" expectedConjunctionPair pair


--input for next tests
testGr :: Grammar
testGr = Grammar (Set.fromList [Nonterminal "S", Nonterminal "B", Nonterminal "C", Nonterminal "D", Nonterminal "F"],
                Set.fromList [Terminal "b", Terminal "a"],
                Set.fromList testRels,
                Nonterminal "S")

testRels :: [Relation]
testRels = [relation11, relation12, relation13,
                       Relation (Nonterminal "S", [T $ Terminal "a"]),
                       Relation (Nonterminal "B", [T $ Terminal "b"]),
                       Relation (Nonterminal "C", [T $ Terminal "a"]),
                       Relation (Nonterminal "D", [T $ Terminal "b"]),
                       Relation (Nonterminal "F", [T $ Terminal "a"])]

relation11 :: Relation
relation11 = Relation (Nonterminal "S",
    [N $ Nonterminal "B", N $ Nonterminal "C",O Conjunction, O Negation, N $ Nonterminal "D", N $ Nonterminal "F"])

relation12 :: Relation
relation12 = Relation (Nonterminal "S", [N $ Nonterminal "C", N $ Nonterminal "D"])

relation13 :: Relation
relation13 = Relation (Nonterminal "C", [N $ Nonterminal "B", N $ Nonterminal "C"])

getLongRelsTest :: Assertion
getLongRelsTest = do
   let expectedRels = Helpers.getLongRels testRels
   let actualRels = [relation11, relation12, relation13]
   assertEqual "assert getting long relations" actualRels expectedRels

checkIfConjHasNegTest1 :: Assertion
checkIfConjHasNegTest1 = do
    assertEqual "Assert if conjunction has negation" False $ Helpers.checkIfConjHasNeg testGr ("1","S","B","C")

checkIfConjHasNegTest2 :: Assertion
checkIfConjHasNegTest2 = do
    assertEqual "Assert if conjunction has negation" True $ Helpers.checkIfConjHasNeg testGr ("1","S","D","F")

relationHasOneTerminalInRightPartTest :: Assertion
relationHasOneTerminalInRightPartTest = do
    assertEqual "Assert if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 3
    assertEqual "Assert if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 4
    assertEqual "Assert if relation is short" False $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 2
    assertEqual "Assert if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 5
    assertEqual "Assert if relation is short" False $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 1
    assertEqual "Assert if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 6
    assertEqual "Assert if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 7
    assertEqual "Assert if relation is short" False $ Helpers.relationHasOneTerminalInRightPart $ head testRels

getFstNontermsInConjOfGivenRelationTest :: Assertion
getFstNontermsInConjOfGivenRelationTest = do
  let expectedS1 = ["B", "D"]
  let expectedS2 = ["C"]
  let actualS1 = Helpers.getFirstNonterminalsInConjunctionsOfGivenRelation testGr "S" "1"
  let actualS2 = Helpers.getFirstNonterminalsInConjunctionsOfGivenRelation testGr "S" "2"
  assertEqual "Getting fst nonterns in conjs in given rel" expectedS1 actualS1
  assertEqual "Getting fst nonterns in conjs in given rel" expectedS2 actualS2
  let expectedC1 = ["B"]
  let actualC1 = Helpers.getFirstNonterminalsInConjunctionsOfGivenRelation testGr "C" "1"
  assertEqual "Getting fst nonterns in conjs in given rel" expectedC1 actualC1

getSndNontermsInConjOfGivenRelationTest :: Assertion
getSndNontermsInConjOfGivenRelationTest = do
  let actualS1' = Helpers.getSecondNonterminalsInConjunctionsOfGivenRelation testGr "S" "1" "B"
  let actualS1'' = Helpers.getSecondNonterminalsInConjunctionsOfGivenRelation testGr "S" "1" "D"
  let actualS2 = Helpers.getSecondNonterminalsInConjunctionsOfGivenRelation testGr "S" "2" "C"
  assertEqual "Getting fst nonterns in conjs in given rel" ["C"] actualS1'
  assertEqual "Getting fst nonterns in conjs in given rel" ["F"] actualS1''
  assertEqual "Getting fst nonterns in conjs in given rel" ["D"] actualS2
  let actualC1 = Helpers.getSecondNonterminalsInConjunctionsOfGivenRelation testGr "C" "1" "B"
  assertEqual "Getting fst nonterns in conjs in given rel" ["C"] actualC1

calculateQuadsTest1 :: Assertion
calculateQuadsTest1 = do
  let actual = Helpers.calculateQuads testGr "1" ["S", "C"]
  let expected = [("1", "S", "B","C"),("1", "S", "D","F"), ("1", "C", "B", "C")]
  assertEqual "Getting quads: (,,,) from relations with given number" expected actual

calculateQuadsTest2 :: Assertion
calculateQuadsTest2 = do
  let actual = Helpers.calculateQuads testGr "2" ["S"]
  let expected = [("2", "S", "C","D")]
  assertEqual "Getting quads: (,,,) from relations with given number" expected actual

calculateQuadsFromGrammarTest :: Assertion
calculateQuadsFromGrammarTest = do
  let gr@(Grammar (nonterminals,_, rels,_)) = testGr
  let maxNumber = Helpers.calculateMaxNumberOfRulesForNonterminal gr
  assertEqual "Assert max number" 3 maxNumber
  let indices = map show [0..maxNumber - 1]
  let nonterminalsList = map nonterminalValue $ Set.toList nonterminals
  let indicesWithNonterms = map (\i -> (i, filter (\t ->
        Helpers.kthRelForNonterminalLong (Set.toList rels) t i) nonterminalsList)) indices
  assertEqual "Assert indices with nonterms" [("0", []), ("1",["C","S"]), ("2", ["S"])] indicesWithNonterms
  let actualQuads = concatMap (uncurry $ Helpers.calculateQuads gr) indicesWithNonterms
  let expectedQuads = [("1", "C", "B", "C"), ("1", "S", "B","C"),
        ("1", "S", "D","F"), ("2", "S", "C","D")]
  assertEqual "Testing process of calculating all quads for all indices" actualQuads expectedQuads

calculateAllQuadsTest :: Assertion
calculateAllQuadsTest = do
  let expectedQuads = [("1", "C", "B", "C"), ("1", "S", "B","C"),
        ("1", "S", "D","F"), ("2", "S", "C","D")]
  let actualQuads = Helpers.calculateAllQuads testGr
  assertEqual "Testing process of calculating all quads for all indices" actualQuads expectedQuads

kthRelForNonterminalLongTest1 :: Assertion
kthRelForNonterminalLongTest1 = do
  let actual0 = Helpers.kthRelForNonterminalLong testRels "S" "0"
  let actual2 = Helpers.kthRelForNonterminalLong testRels "S" "2"
  assertEqual "Assert if 0th relation for given nonterminal long" False actual0
  assertEqual "Assert if 2th relation for given nonterminal long" True actual2

kthRelForNonterminalLongTest2 :: Assertion
kthRelForNonterminalLongTest2 = do
  let (Grammar (_,_,relations',_)) = testGr
  let relations = Set.toList relations'
  let actual0 = Helpers.kthRelForNonterminalLong relations "C" "0"
  let actual2 = Helpers.kthRelForNonterminalLong relations "C" "1"
  assertEqual "Assert if 0th relation for given nonterminal long" False actual0
  assertEqual "Assert if 2th relation for given nonterminal long" True actual2


getShiftsDecrementsTest :: Assertion 
getShiftsDecrementsTest = do
  let expected1 = [("*","7"),("1","*"),("2","1"),("3","2"),("4","3"),("5","4"),("6","5"),("7","6")]
  let actual1 = Helpers.getShiftsDecrements 7 "*"
  assertEqual "Assert correctness of shifts generating" expected1 actual1
  let expected2 = [("*","6"),("1","*"),("2","1"),("3","2"),("4","3"),("5","4"),("6","5")]
  let actual2 = Helpers.getShiftsDecrements 6 "*"
  assertEqual "Assert correctness of shifts generating" expected2 actual2

symbolAcceptedByNonterminalTest1 :: Assertion
symbolAcceptedByNonterminalTest1 = do
  let actual1 = Helpers.symbolAcceptedByNonterminal testGr "S" "b"
  let actual2 = Helpers.symbolAcceptedByNonterminal testGr "S" "a"
  assertEqual "Assert that testGr does not have S -> b rel" False actual1
  assertEqual "Assert that testGr has S -> a rel" True actual2

symbolAcceptedByNonterminalTest2 :: Assertion
symbolAcceptedByNonterminalTest2 = do
  let actual1 = Helpers.symbolAcceptedByNonterminal testGr "D" "a"
  let actual2 = Helpers.symbolAcceptedByNonterminal testGr "D" "b"
  assertEqual "Assert that testGr does not have D -> a rel" False actual1
  assertEqual "Assert that testGr has D -> b rel" True actual2

symbolAcceptedByNonterminalTest3 :: Assertion
symbolAcceptedByNonterminalTest3 = do
  let actual1 = Helpers.symbolAcceptedByNonterminal testGr "F" "b"
  let actual2 = Helpers.symbolAcceptedByNonterminal testGr "F" "a"
  assertEqual "Assert that testGr does not have F -> a rel" False actual1
  assertEqual "Assert that testGr has F -> b rel" True actual2

symbolAcceptedByNonterminalTest4 :: Assertion
symbolAcceptedByNonterminalTest4 = do
  let actual1 = Helpers.symbolAcceptedByNonterminal testGr "C" "b"
  let actual2 = Helpers.symbolAcceptedByNonterminal testGr "C" "a"
  assertEqual "Assert that testGr does not have C -> b rel" False actual1
  assertEqual "Assert that testGr has C -> a rel" True actual2

getNumbersOfShortRelationsTest :: Assertion
getNumbersOfShortRelationsTest = do
    let expected = Map.fromList [(Nonterminal "C",["0"]), (Nonterminal "B",["0"]),
            (Nonterminal "D", ["0"]), (Nonterminal "F", ["0"]), (Nonterminal "S",["0"])]
    let actual = Helpers.getNumbersOfShortRelations testGr
    assertEqual "Assert numbers of short relations" expected actual