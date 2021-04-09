{-# LANGUAGE LambdaCase #-}

module TestGenerationHelpers where
  
import InterpreterInputData
import DebuggingTMTypes 
import TMTypes 
import Interpreter

import Test.HUnit
import qualified Data.Set as Set
import Data.List as List

generateTestLabel :: TuringMachine -> InterpreterInputData -> [String] -> Bool -> Test
generateTestLabel tm input word accepted = let
    testCase = generateTestCase tm input word accepted in
    TestLabel ("Test for grammar " ++ inId input) testCase

generateTestCase :: TuringMachine -> InterpreterInputData -> [String] -> Bool -> Test
generateTestCase tm input word accepted = let
    --tm = boolean2tm $ inGrammar input
    xs = states $ startWithAlphabet' tm word 0 $ inAlphabet input
    testCase = if accepted
        then TestCase (assertEqual (concat word) (Q 0) (currentState $ last xs))
        else TestCase (assertEqual (concat word) (Q (-1)) (currentState $ last xs))
    description = if accepted
        then "check word " ++ concat word ++ " accepted"
        else "check word " ++ concat word ++ " not accepted"
    in TestLabel description testCase

-- print info
printInfo :: DebuggingTuringMachine -> TuringMachine -> Set.Set String -> [String] -> IO [()]
printInfo dtm tm inputAlphabet word = do
    let states' = states $ startWithAlphabet' tm word 0 inputAlphabet
    let states'' = getStates dtm
    let statesPairs = map (\t -> (elemIndex t states'',  t)) states''
    let symbols = getSymbols dtm
    let symbolsPairs = map (\t -> (elemIndex t symbols,  t)) symbols
    print symbolsPairs
    print statesPairs
    print tm
    mapM (\(WS _ currentState' tape' alphabet') -> let
        (Q index') = currentState';
        state = snd $ head $ filter (\case
            (Just index,  DState _) -> index == index' + 1
            (Nothing,  _) -> False) statesPairs;
        (DState stringState) = state
        in do {
            print currentState';
            print stringState;
            print tape';
            print alphabet';}) states'
