{-# LANGUAGE LambdaCase #-}

module TestGenerationHelpers where
  
import InterpreterInputData
import DebuggingTMTypes 
import TuringMachine.Interpreter 

import Test.HUnit
import Control.Lens ((^.))
import qualified Data.Set as Set
import Data.List as List

generateTestLabel :: Maybe TuringMachine -> InterpreterInputData -> String -> Bool -> Test
generateTestLabel Nothing input _ _ =
    TestLabel ("Test for grammar " ++ inId input) $
        TestCase $ assertString "Turing Machine wasn't built"
generateTestLabel (Just tm) input word accepted = let
    testCase = generateTestCase tm input word accepted in
    TestLabel ("Test for grammar " ++ inId input) testCase

generateTestCase :: TuringMachine -> InterpreterInputData -> String -> Bool -> Test
generateTestCase tm input word accepted = let
    --tm = boolean2tm $ inGrammar input
    finalWS = last $ smartRun tm $ initWS word 0
    testCase =
        if accepted
        then TestCase $ assertBool word $ finalWS^.currentState == finalState
        else TestCase $ assertBool word $ finalWS^.currentState /= finalState
    description =
        if accepted
        then "check word " ++ word ++ " accepted"
        else "check word " ++ word ++ " not accepted"
    in TestLabel description testCase

-- print info
{-printInfo :: DebuggingTuringMachine -> TuringMachine -> Set.Set String -> [String] -> IO [()]
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
            print alphabet';}) states'-}
