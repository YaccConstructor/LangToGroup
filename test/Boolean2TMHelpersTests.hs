module Boolean2TMHelpersTests where

import Test.HUnit
import GrammarType
import DebuggingTMTypes
import qualified Boolean2TMHelpers as Helpers
import qualified Data.Set as Set

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
    assertEqual "Check if conjunction has negation" False $ Helpers.checkIfConjHasNeg testGr ("1","S","B","C")

checkIfConjHasNegTest2 :: Assertion
checkIfConjHasNegTest2 = do
    assertEqual "Check if conjunction has negation" True $ Helpers.checkIfConjHasNeg testGr ("1","S","D","F")

relationHasOneTerminalInRightPartTest :: Assertion
relationHasOneTerminalInRightPartTest = do
    assertEqual "Check if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 3
    assertEqual "Check if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 4
    assertEqual "Check if relation is short" False $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 2
    assertEqual "Check if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 5
    assertEqual "Check if relation is short" False $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 1
    assertEqual "Check if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 6
    assertEqual "Check if relation is short" True $ Helpers.relationHasOneTerminalInRightPart $ testRels !! 7
    assertEqual "Check if relation is short" False $ Helpers.relationHasOneTerminalInRightPart $ head testRels

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
