module Boolean2TMHelpersTests where

import Test.HUnit
import GrammarType
import DebuggingTMTypes
import qualified Boolean2TMHelpers as Helpers
import qualified Data.Set as Set

-- tests written for boolean grammars in normal form
calculateNextConjunctionInSameRuleTest1 :: IO ()
calculateNextConjunctionInSameRuleTest1 = do
  let testGr = Grammar (
                            Set.fromList [Nonterminal "Abc",Nonterminal "Cr",Nonterminal "D",Nonterminal "S"],
                            Set.fromList [Terminal "b"],
                            Set.fromList [
                                            Relation (Nonterminal "S",[N $ Nonterminal "D", N $ Nonterminal "Cr",O Conjunction, N $ Nonterminal "S", N $ Nonterminal "Abc"])
                                         ],
                            Nonterminal "S")
  let testConjunctionPair = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 0, False, N $ Nonterminal "D", N $ Nonterminal "Cr")
  let expectedConjunctionPair = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 0, False, N $ Nonterminal "S", N $ Nonterminal "Abc")
  let actualConjunctionPair = Helpers.calculateNextConjunctionInSameRule testGr testConjunctionPair
  case actualConjunctionPair of
        Nothing -> assertFailure "In relation no such conjunction, or next conjunction does not exist"
        Just pair -> assertEqual "assert getting next conjunction" expectedConjunctionPair  pair  
  let testConjunctionPair1 = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 0, False, N $ Nonterminal "S", N $ Nonterminal "Abc")
  let expectedConjunctionPair1 = Nothing
  let actualConjunctionPair1 = Helpers.calculateNextConjunctionInSameRule testGr testConjunctionPair1
  case actualConjunctionPair1 of
        Nothing -> assertEqual "assert getting next conjunction" actualConjunctionPair1 expectedConjunctionPair1
        Just _ -> assertFailure "Next conjunction does not exist, but result is not Nothing"
    
    
calculateNextConjunctionInSameRuleTest2 :: Assertion
calculateNextConjunctionInSameRuleTest2 = do
  let testGr = Grammar (
                            Set.fromList [Nonterminal "S",Nonterminal "Sa", Nonterminal "Sb"],
                            Set.fromList [Terminal "b"],
                            Set.fromList [
                                            Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "S", O Conjunction, O Negation, N $ Nonterminal "Sa", N $ Nonterminal "Sb"]),
                                            Relation (Nonterminal "S",[N $ Nonterminal "Sb", N $ Nonterminal "Sa"])
                                         ],
                            Nonterminal "S")
  let testConjunctionPair1 = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 1, False, N $ Nonterminal "Sb", N $ Nonterminal "Sa")
  let actualConjunctionPair1 = Helpers.calculateNextConjunctionInSameRule testGr testConjunctionPair1
  case actualConjunctionPair1 of
        Nothing -> assertEqual "assert getting next conjunction" actualConjunctionPair1 Nothing 
        Just _ -> assertFailure "Next conjunction does not exist, but result is not Nothing"  
  let testConjunctionPair2 = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 0, False, N $ Nonterminal "Sa", N $ Nonterminal "S")
  let actualConjunctionPair2 = Helpers.calculateNextConjunctionInSameRule testGr testConjunctionPair2
  let expectedConjunctionPair2 = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 0, True, N $ Nonterminal "Sa", N $ Nonterminal "Sb")
  case actualConjunctionPair2 of
        Just pair -> assertEqual "assert getting next conjunction in the same rule" pair expectedConjunctionPair2
        Nothing -> assertFailure "Next conjunction does not exist, but result is not Nothing"

calculateFirstConjunctionInNextRuleTest1 :: Assertion
calculateFirstConjunctionInNextRuleTest1 = do
  let testGr = Grammar (
                              Set.fromList [Nonterminal "S",Nonterminal "Sa", Nonterminal "Sb", Nonterminal "Sc"],
                              Set.fromList [Terminal "b"],
                              Set.fromList [
                                              Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "S", O Conjunction, O Negation, N $ Nonterminal "Sa", N $ Nonterminal "Sb"]),
                                              Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "Sd", O Conjunction, N $ Nonterminal "Sb", N $ Nonterminal "Sc"])
                                           ],
                              Nonterminal "S")
  let testConjunctionPair1 = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 0, False, N $ Nonterminal "Sa", N $ Nonterminal "S")
  let testConjunctionPair2 = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 0, True, N $ Nonterminal "Sa", N $ Nonterminal "Sb")

  let actualConjunctionPair1 = Helpers.calculateFirstConjunctionInNextRule testGr testConjunctionPair1
  let actualConjunctionPair2 = Helpers.calculateFirstConjunctionInNextRule testGr testConjunctionPair2

  let expectedConjunctionPair = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 1, False, N $ Nonterminal "Sa", N $ Nonterminal "Sd")

  case actualConjunctionPair1 of
        Nothing -> assertFailure "Next conjunction does not exist"
        Just pair -> assertEqual "assert getting first conjunction in next rule" expectedConjunctionPair pair
  case actualConjunctionPair2 of
        Nothing -> assertFailure "Next conjunction does not exist"
        Just pair -> assertEqual "assert getting first conjunction in next rule" expectedConjunctionPair pair

calculateFirstConjunctionInNextRuleTest2 :: Assertion
calculateFirstConjunctionInNextRuleTest2 = do
  let testGr = Grammar (
                              Set.fromList [Nonterminal "S",Nonterminal "Sa", Nonterminal "Sb", Nonterminal "Sc"],
                              Set.fromList [Terminal "b"],
                              Set.fromList [
                                              Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "S", O Conjunction, O Negation, N $ Nonterminal "Sa", N $ Nonterminal "Sb"]),
                                              Relation (Nonterminal "S",[N $ Nonterminal "Sa", N $ Nonterminal "S", O Conjunction, N $ Nonterminal "Sb", N $ Nonterminal "Sc"]),
                                              Relation (Nonterminal "S", [O Negation, N $ Nonterminal "Sb", N $ Nonterminal "Sa"])
                                           ],
                              Nonterminal "S")
  let testConjunctionPair = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 1, False, N $ Nonterminal "Sa", N $ Nonterminal "S")
  let expectedConjunctionPair = DebuggingTMTypes.SymbolsPair (Nonterminal "S", 2, True, N $ Nonterminal "Sb", N $ Nonterminal "Sa")
  let actualConjunctionPair = Helpers.calculateFirstConjunctionInNextRule testGr testConjunctionPair
  case actualConjunctionPair of
          Nothing -> assertFailure "Next conjunction does not exist"
          Just pair -> assertEqual "assert getting first conjunction in next rule" expectedConjunctionPair pair
