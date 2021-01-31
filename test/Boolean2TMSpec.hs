module Boolean2TMSpec where

import Test.HUnit
import qualified Boolean2TM as SUT

calculateNextConjunctionInSameRuleTest :: Assertion
calculateNextConjunctionInSameRuleTest = do
  let testGr = Grammar (
                                  Set.fromList [Nonterminal "Abc",Nonterminal "Cr",Nonterminal "D",Nonterminal "S"],
                                  Set.fromList [Terminal "b",Terminal "c",Terminal "d",Terminal "e"],
                                  Set.fromList [
                                                  Relation (Nonterminal "Abc",[T (Terminal "b")]),
                                                  Relation (Nonterminal "Cr",[T (Terminal "e")]),
                                                  Relation (Nonterminal "D",[N (Nonterminal "Cr")]),
                                                  Relation (Nonterminal "S",[N (Nonterminal "D"),T (Terminal "c"),O Conjunction,T (Terminal "d"),N (Nonterminal "Abc")])
                                               ],
                                  Nonterminal "S")
  let testConjunctionPair = ConjunctionPair (Nonterminal "S", 1, Nothing, Nonterminal "D", Terminal "c")
  let expectedConjunctionPair = ConjunctionPair (Nonterminal "S", 1, Nothing, Nonterminal "d", Terminal "Abc")
  let actualConjunctionPair = calculateNextConjunctionInSameRule testGr testConjunctionPair 
  assertEqual "assert getting next conjunction" expectedConjunctionPair test