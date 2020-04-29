import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import CFG2TM
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
    let a = Value "a"
    let a' = Value "a'"
    let expectedConfigs = Configs ([
            [([lBL, a], sSFT, [rBL]), ([lBL], sSST, [rBL])],
            [([lBL, a], sSFT, [rBL]), ([lBL, Value "S"], iSST, [rBL])],
            [([lBL, a], sSFT, [rBL]), ([lBL, a'], iSST, [rBL])],
            [([lBL], sSFT, [rBL]), ([lBL], iSST, [rBL])],
            [([lBL], fSFT, [rBL]), ([lBL], fSST, [rBL])]
            ])
    
    assertEqual "config test" expectedConfigs (interpretTM ["a"] $ cfg2tm test1Grammar)

simpleCfgToTMMapTest :: Assertion
simpleCfgToTMMapTest = do
    let letter_a = Value "a"
    let letter_S = Value "S"
    
    let expectedTM = TM (
            InputAlphabet (Set.fromList [letter_a]),
            [TapeAlphabet (Set.fromList [letter_a]), TapeAlphabet (Set.fromList [getDisjoinSquare letter_a, letter_S])],
            MultiTapeStates [(Set.fromList [sSFT, fSFT]), 
                            (Set.fromList [sSST, iSST, fSST])],
            Commands (Set.fromList [
                [
                    SingleTapeCommand ((eL, sSFT, rBL), (eL, sSFT, rBL)),
                    SingleTapeCommand ((eL, sSST, rBL), (letter_S, iSST, rBL))],
                [
                    SingleTapeCommand ((letter_a, sSFT, rBL), (letter_a, sSFT, rBL)),
                    SingleTapeCommand ((letter_S, iSST, rBL), (getDisjoinSquare letter_a, iSST, rBL))],
                [ 
                    SingleTapeCommand ((letter_a, sSFT, rBL), (eL, sSFT, rBL)),
                    SingleTapeCommand ((getDisjoinSquare letter_a, iSST, rBL), (eL, iSST, rBL))],
                [
                    SingleTapeCommand ((lBL, sSFT, rBL), (lBL, fSFT, rBL)),
                    SingleTapeCommand ((lBL, iSST, rBL), (lBL, fSST, rBL))]
                ]),
            StartStates [sSFT, sSST],
            AccessStates [fSFT, fSST]
                )
    assertEqual "simple cfg to TMs convertion" expectedTM (cfg2tm test1Grammar)

main :: IO ()
main = defaultMainWithOpts
       [testCase "simple cfg to TM map" simpleCfgToTMMapTest,
        testCase "cft to TM to config test" configsTest,
        testCase "simple sm test" smInterpretationTest,
        testCase "sm test 2" smInterpretationTest2,
        testCase "sm2gr test" sm2grTest]
       mempty
