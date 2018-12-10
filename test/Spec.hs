import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import CfgToTm1Mapper
import GrammarType
import Tm1Type
import Data.Set (Set)
import qualified Data.Set as Set

simpleCfgToTM1MapTest :: Assertion
simpleCfgToTM1MapTest = do
    let terminal = Terminal 'a'
    let nonterminal = Nonterminal 'S'
    let grammar =
            Grammar(
                (Set.fromList [nonterminal]), 
                (Set.fromList [terminal]), 
                (Set.fromList [Relation (nonterminal, [T terminal])]),
                nonterminal
            )
    let letter_a = 'a'
    let letter_S = 'S'
    let workState = Tm1Type.State "q1"
    
    let expectedTM1 = TM1 (
            InputAlphabet (Set.fromList [letter_a]),
            TapeAlphabet (Set.fromList [letter_a, letter_S]),
            MultiTapeStates [(Set.fromList [startStateFirstTape, intermediateStateFirstTape, finalStateFirstTape]), 
                            (Set.fromList [startStateSecondTape, intermediateStateSecondTape, finalStateSecondTape, workState])],
            Commands (Set.fromList [
                [
                    SingleTapeCommand (
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (emptySymbol, 
                        intermediateStateFirstTape, 
                        rightBoundingLetter)
                        ),
                    SingleTapeCommand (
                        (emptySymbol, 
                        startStateSecondTape, 
                        rightBoundingLetter), 
                        (letter_S, 
                        startStateSecondTape, 
                        rightBoundingLetter)
                        )
                    ],
                [ 
                    NoCommand,
                    SingleTapeCommand (
                        (letter_S, 
                        startStateSecondTape, 
                        emptySymbol), 
                        (emptySymbol, 
                        intermediateStateSecondTape,
                        letter_S)
                        )
                    ],
                [
                    NoCommand,
                    SingleTapeCommand (
                        (emptySymbol, 
                        intermediateStateSecondTape, 
                        letter_S), 
                        (emptySymbol, 
                        workState, 
                        letter_S)
                        )
                    ],
                [
                    NoCommand,
                    SingleTapeCommand (
                        (emptySymbol, 
                        workState, 
                        letter_S), 
                        (emptySymbol, 
                        workState, 
                        letter_a)
                        )
                    ],
                [
                    NoCommand, 
                    SingleTapeCommand (
                        (emptySymbol, 
                        workState, 
                        letter_a), 
                        (emptySymbol, 
                        intermediateStateSecondTape, 
                        letter_a)
                        )
                    ],
                [
                    SingleTapeCommand (
                        (letter_a, 
                        intermediateStateFirstTape, 
                        emptySymbol), 
                        (emptySymbol, 
                        intermediateStateFirstTape, 
                        letter_a)
                        ), 
                    SingleTapeCommand (
                        (emptySymbol, 
                        intermediateStateSecondTape, 
                        letter_a), 
                        (letter_a, 
                        intermediateStateSecondTape, 
                        emptySymbol)
                        )
                    ],
                [ 
                    SingleTapeCommand (
                        (leftBoundingLetter, 
                        intermediateStateFirstTape, 
                        emptySymbol), 
                        (leftBoundingLetter, 
                        finalStateFirstTape, 
                        emptySymbol)
                        ),
                    SingleTapeCommand (
                        (emptySymbol, 
                        intermediateStateSecondTape, 
                        emptySymbol), 
                        (emptySymbol, 
                        finalStateSecondTape, 
                        emptySymbol)
                        )
                    ]
                ]),
            StartStates [startStateFirstTape, startStateSecondTape],
            AccessStates [finalStateFirstTape, finalStateSecondTape]
                )
    assertEqual "simple cfg to tm1s convertion" (mapCfgToTm1 grammar) expectedTM1
    
main :: IO ()
main = defaultMainWithOpts
       [testCase "simple cfg to tm1 map" simpleCfgToTM1MapTest]
       mempty