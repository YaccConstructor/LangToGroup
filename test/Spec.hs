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
                Nonterminals (Set.fromList [nonterminal]), 
                Terminals (Set.fromList [terminal]), 
                Relations (Set.fromList [Relation (nonterminal, [T terminal])]),
                nonterminal
            )
    let letter_a = Letter 'a'
    let letter_S = Letter 'S'
    let workState = Tm1Type.State "q1"
    
    let expectedTM1 = TM1 (
            InputAlphabet (Set.fromList [letter_a]),
            TapeAlphabet (Set.fromList [letter_a, letter_S]),
            MultiTapeStates [States (Set.fromList [startStateFirstTape, finalStateFirstTape]), 
                            States (Set.fromList [startStateSecondTape, finalStateSecondTape, workState])],
            Commands (Set.fromList [
                Command [
                    SingleTapeCommand (
                        (emptySymbol, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (emptySymbol, 
                        finalStateFirstTape, 
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
                Command [ 
                    NoCommand,
                    SingleTapeCommand (
                        (letter_S, 
                        startStateSecondTape, 
                        emptySymbol), 
                        (emptySymbol, 
                        finalStateSecondTape,
                        letter_S)
                        )
                    ],
                Command [
                    NoCommand,
                    SingleTapeCommand (
                        (emptySymbol, 
                        finalStateSecondTape, 
                        letter_S), 
                        (emptySymbol, 
                        workState, 
                        letter_S)
                        )
                    ],
                Command [
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
                Command [
                    NoCommand, 
                    SingleTapeCommand (
                        (emptySymbol, 
                        workState, 
                        letter_a), 
                        (emptySymbol, 
                        finalStateSecondTape, 
                        letter_a)
                        )
                    ],
                Command [
                    SingleTapeCommand (
                        (letter_a, 
                        finalStateFirstTape, 
                        emptySymbol), 
                        (emptySymbol, 
                        finalStateFirstTape, 
                        letter_a)
                        ), 
                    SingleTapeCommand (
                        (emptySymbol, 
                        finalStateSecondTape, 
                        letter_a), 
                        (letter_a, 
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