import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import CfgToTMMapper
import Helpers
import GrammarType
import TMType
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import TMInterpreter
import ConfigType
import Helpers
import SMTests
import GrTests


test1Grammar = grammar where
    terminal = Terminal "a"
    nonterminal = Nonterminal "S"
    eps = Epsilon "ε"
    grammar =
        Grammar(
            (Set.fromList [nonterminal]),
            (Set.fromList [terminal]),
            (Set.fromList [GrammarType.Relation (nonterminal,
            [GrammarType.T terminal])]),
            nonterminal,
            eps
        )

configsTest :: Assertion
configsTest = do
    let q1 = TMType.State "q_{3}^2"
    let a = Value "a"
    let a' = Value "a'"
    let expectedConfigs = Configs ([
            [([leftBoundingLetter, a], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter], startStateSecondTape, [rightBoundingLetter])],
            [([leftBoundingLetter, a], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter, Value "S"], intermediateStateSecondTape, [rightBoundingLetter])],
            [([leftBoundingLetter, a], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter, a'], q1, [rightBoundingLetter])],
            [([leftBoundingLetter, a], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter, a'], intermediateStateSecondTape, [rightBoundingLetter])],
            [([leftBoundingLetter], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter], intermediateStateSecondTape, [rightBoundingLetter])],
            [([leftBoundingLetter], finalStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter], finalStateSecondTape, [rightBoundingLetter])]
            ])
    
    assertEqual "config test" expectedConfigs (interpretTM ["a"] $ mapCfgToTM test1Grammar)

simpleCfgToTMMapTest :: Assertion
simpleCfgToTMMapTest = do
    let letter_a = Value "a"
    let letter_S = Value "S"
    let workState = TMType.State "q_{3}^2"
    
    let expectedTM = TM (
            InputAlphabet (Set.fromList [letter_a]),
            [TapeAlphabet (Set.fromList [letter_a]), TapeAlphabet (Set.fromList [getDisjoinSquare letter_a, letter_S])],
            MultiTapeStates [(Set.fromList [startStateFirstTape, finalStateFirstTape]), 
                            (Set.fromList [startStateSecondTape, intermediateStateSecondTape, finalStateSecondTape, workState])],
            Commands (Set.fromList [
                [
                    SingleTapeCommand (
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter)
                        ),
                    SingleTapeCommand (
                        (emptySymbol, 
                        startStateSecondTape, 
                        rightBoundingLetter), 
                        (letter_S, 
                        intermediateStateSecondTape, 
                        rightBoundingLetter)
                        )
                    ],
                [
                    SingleTapeCommand (
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter)
                        ),
                    SingleTapeCommand (
                        (letter_S, 
                        intermediateStateSecondTape, 
                        rightBoundingLetter), 
                        (getDisjoinSquare letter_a, 
                        workState, 
                        rightBoundingLetter)
                        )
                    ],
                [
                    SingleTapeCommand (
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter)
                        ), 
                    SingleTapeCommand (
                        (emptySymbol, 
                        workState, 
                        rightBoundingLetter), 
                        (emptySymbol, 
                        intermediateStateSecondTape, 
                        rightBoundingLetter)
                        )
                    ],
                [ 
                    SingleTapeCommand (
                        (letter_a, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter)
                        ),
                    SingleTapeCommand (
                        (getDisjoinSquare letter_a, 
                        intermediateStateSecondTape, 
                        rightBoundingLetter), 
                        (emptySymbol, 
                        intermediateStateSecondTape, 
                        rightBoundingLetter)
                        )
                    ],
                [
                    SingleTapeCommand (
                        (leftBoundingLetter, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (leftBoundingLetter, 
                        finalStateFirstTape, 
                        rightBoundingLetter)
                        ),
                    SingleTapeCommand (
                        (leftBoundingLetter, 
                        intermediateStateSecondTape, 
                        rightBoundingLetter), 
                        (leftBoundingLetter, 
                        finalStateSecondTape, 
                        rightBoundingLetter)
                        )
                    ]
                ]),
            StartStates [startStateFirstTape, startStateSecondTape],
            AccessStates [finalStateFirstTape, finalStateSecondTape]
                )
    assertEqual "simple cfg to TMs convertion" expectedTM (mapCfgToTM test1Grammar)

main :: IO ()
main = defaultMainWithOpts
       [testCase "simple cfg to TM map" simpleCfgToTMMapTest,
        testCase "cft to TM to config test" configsTest,
        testCase "simple sm test" smInterpretationTest,
        testCase "sm test 2" smInterpretationTest2,
        testCase "sm2gr test" sm2GrTest]
       mempty
