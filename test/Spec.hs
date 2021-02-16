import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import CFG2TM
import Helpers
import GrammarType
import TMType
import qualified Data.Set as Set
import TMInterpreter
import ConfigType
import SMTests
import GrTests
import ParserTests
import TmsTests
import TuringMachine2TmsTests
import Tms2TuringMachineTests
import TmsParserTests
import qualified SPTests (tests)

test1Grammar :: Grammar
test1Grammar = grammar where
    terminal = Terminal "a"
    nonterminal = Nonterminal "S"
    grammar =
        Grammar(
            Set.fromList [nonterminal],
            Set.fromList [terminal],
            Set.fromList [GrammarType.Relation (nonterminal,
            [GrammarType.T terminal])],
            nonterminal
        )

configsTest :: Assertion
configsTest = do
    let a = Value "a" 0
    let a' = Value "a" 1
    let expectedConfigs = Configs [
            [([LBS, a], sSFT, [RBS]), ([LBS], sSST, [RBS])],
            [([LBS, a], sSFT, [RBS]), ([LBS, defValue "S"], iSST, [RBS])],
            [([LBS, a], sSFT, [RBS]), ([LBS, a'], iSST, [RBS])],
            [([LBS], sSFT, [RBS]), ([LBS], iSST, [RBS])],
            [([LBS], fSFT, [RBS]), ([LBS], fSST, [RBS])]
            ]
    
    assertEqual "config test" expectedConfigs (interpretTM ["a"] $ cfg2tm test1Grammar)

simpleCfgToTMMapTest :: Assertion
simpleCfgToTMMapTest = do
    let letter_a = defValue "a"
    let letter_S = defValue "S"
    
    let expectedTM = TM (
            InputAlphabet (Set.fromList [letter_a]),
            [TapeAlphabet (Set.fromList [letter_a]), TapeAlphabet (Set.fromList [getDisjoinSquare letter_a, letter_S])],
            MultiTapeStates [Set.fromList [sSFT, fSFT], 
                             Set.fromList [sSST, iSST, fSST]],
            Commands (Set.fromList [
                [
                    SingleTapeCommand ((ES, sSFT, RBS), (ES, sSFT, RBS)),
                    SingleTapeCommand ((ES, sSST, RBS), (letter_S, iSST, RBS))],
                [
                    SingleTapeCommand ((letter_a, sSFT, RBS), (letter_a, sSFT, RBS)),
                    SingleTapeCommand ((letter_S, iSST, RBS), (getDisjoinSquare letter_a, iSST, RBS))],
                [ 
                    SingleTapeCommand ((letter_a, sSFT, RBS), (ES, sSFT, RBS)),
                    SingleTapeCommand ((getDisjoinSquare letter_a, iSST, RBS), (ES, iSST, RBS))],
                [
                    SingleTapeCommand ((LBS, sSFT, RBS), (LBS, fSFT, RBS)),
                    SingleTapeCommand ((LBS, iSST, RBS), (LBS, fSST, RBS))]
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
        testCase "apply rule test" applyRuleTest,
        testCase "sm2gr test" sm2grTest,
        testCase "custom CFG to TM" testCustomCFG,
        testCase "custom conjunctive to TM" testCustomConjunctive,
        testCase "custom boolean to TM" testCustomBoolean,
        testCase "show one tape Tms" testShowOneTapeTms,
        testCase "show multi tape Tms" testShowMultiTapeTms,
        testCase "one tape TM to Tms" testOneTapeTM2Tms,
        testCase "multi tape TM to Tms" testMultiTapeTM2Tms,
        testCase "TuringMachine to Tms" testTuringMachine2Tms,
        testCase "Tms to TuringMachine simple" testTms2TuringMachineSimple,
        testCase "Tms to TuringMachine leave stay" testTms2TuringMachineLeaveStay,
        testCase "Tms to TuringMachine leave move" testTms2TuringMachineLeaveMove,
        testCase "Tms to TuringMachine change move" testTms2TuringMachineChangeMove,
        testCase "Tms to TuringMachine id move" testTms2TuringMachineIdMove,
        testCase "parse Tms: test1tape1cmd" test1tape1cmd,
        testCase "parse Tms: test1tape3cmd" test1tape3cmd,
        testCase "parse Tms: test3tape3cmd" test3tape3cmd,
        head $ hUnitTestToTests SPTests.tests
        ]
       mempty
