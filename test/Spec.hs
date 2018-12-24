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
import SMType
import GRType
import SMachineToGroup
import Data.List

simpleCfgToTM1MapTest :: Assertion
simpleCfgToTM1MapTest = do
    let terminal = Terminal 'a'
    let nonterminal = Nonterminal 'S'
    let grammar =
            Grammar(
                (Set.fromList [nonterminal]), 
                (Set.fromList [terminal]), 
                (Set.fromList [GrammarType.Relation (nonterminal, 
                [GrammarType.T terminal])]),
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

genY :: Int -> [[Y]] -> [[Y]]
genY 0 x = x
genY n x = genY (n-1) ([[Y ("y" ++ intToString(n))]] ++ x) 

genQ :: Int -> [[SMType.State String]] -> [[SMType.State String]]
genQ 0 x = x
genQ n x = genQ (n-1) ([[SMType.State Q n (Set.fromList [Quote]) "", 
                        SMType.State P n (Set.fromList [Quote]) ""]] ++ x) 

genQA :: Int -> [A] -> [A]
genQA 0 x = x
genQA n x = genQA (n-1) (x ++ 
                  [W [A_Q (SMType.State Q n (Set.fromList [Quote]) "")], 
                  W [A_Q (SMType.State P n (Set.fromList [Quote]) "")]])

genTransitionQ :: Int -> SRule -> [GRType.Relation]
genTransitionQ 3 sRule = []
genTransitionQ n sRule = [GRType.Relation([W [(A_R' sRule)], 
                  W [(A_Q (SMType.State Q n (Set.fromList [Quote]) "") )],
                  W [(A_R sRule)] ], 
                  [W [A_Q (SMType.State Q n (Set.fromList [Quote]) "")] ])] 
                 ++ [GRType.Relation([W [(A_R' sRule)], 
                  W [(A_Q (SMType.State P n (Set.fromList [Quote]) "") )],
                  W [(A_R sRule)] ], 
                 [W [A_Q (SMType.State P n (Set.fromList [Quote]) "")] ])] 
                 ++ genTransitionQ (n-1) sRule

sMachineToGroupTest :: Assertion
sMachineToGroupTest = do
    let y = genY 20 []
    let q = genQ 21 [] 
    let sRule = SRule [(Word [SmbQ (SMType.State Q 1 (Set.fromList [Quote]) "")], 
                        Word [SmbQ (SMType.State P 1 (Set.fromList [Quote]) ""), 
                        SmbY' (Y "y1")]),
                  (Word[SmbQ (SMType.State Q 2 (Set.fromList [Quote]) ""), 
                   SmbY (Y "y2"), 
                   SmbQ (SMType.State Q 3 (Set.fromList [Quote]) "")],
                   Word[SmbY' (Y "y1"), 
                   SmbQ (SMType.State P 2 (Set.fromList [Quote]) ""), 
                   SmbY (Y "y2"), 
                   SmbQ (SMType.State Q 3 (Set.fromList [Quote]) ""), 
                   SmbY (Y "y3")])]
    let sm = SM y q [sRule]  
    let w = Word [SmbY (Y "y1")]
    let nk = 6
    let a = [W [(A_S "alpha")], 
             W [(A_S "omega")], 
             W [(A_S "delta")], 
             W [(A_K "k1")], 
             W [(A_K "k2")], 
             W [(A_K "k3")], 
             W [(A_K "k4")],
             W [(A_K "k5")], 
             W [(A_K "k6")], 
             W [(A_K "k7")], 
             W [(A_K "k8")],
             W [(A_K "k9")], 
             W [(A_K "k10")], 
             W [(A_K "k11")], 
             W [(A_K "k12")],
             W [A_Y (Y "y1")]] 
             ++ (sort (genQA 21 [])) ++ [W [(A_R sRule)]]
    let auxiliary = [GRType.Relation([W [(A_R sRule)], 
                     W [(A_S "alpha")]], 
                     [W [(A_S "alpha")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_S "omega")]], 
                     [W [(A_S "omega")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_S "delta")]], 
                     [W [(A_S "delta")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_Y (Y "y1"))]], 
                     [W [(A_Y (Y "y1"))], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k1")]], 
                     [W [(A_K "k1")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k2")]], 
                     [W [(A_K "k2")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k3")]], 
                     [W [(A_K "k3")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k4")]], 
                     [W [(A_K "k4")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k5")]], 
                     [W [(A_K "k5")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k6")]], 
                     [W [(A_K "k6")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k7")]], 
                     [W [(A_K "k7")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k8")]], 
                     [W [(A_K "k8")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k9")]], 
                     [W [(A_K "k9")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k10")]], 
                     [W [(A_K "k10")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k11")]], 
                     [W [(A_K "k11")], 
                     W [(A_R sRule)]]),
                     GRType.Relation([W [(A_R sRule)], 
                     W [(A_K "k12")]], 
                     [W [(A_K "k12")], 
                     W [(A_R sRule)]])]
    let transition = [GRType.Relation([W [(A_R' sRule)], 
                  W [(A_W (Word [SmbQ (SMType.State Q 1 (Set.fromList [Quote]) "")]))],
                  W [(A_R sRule)] ],[
                  W [A_W ((Word [SmbQ (SMType.State P 1 (Set.fromList [Quote]) ""),
                  SmbY' (Y "y1")]))]]),

                  GRType.Relation([W [(A_R' sRule)], 
                  W [(A_W (Word[SmbQ (SMType.State Q 2 (Set.fromList [Quote]) ""), 
                  SmbY (Y "y2"), 
                  SmbQ (SMType.State Q 3 (Set.fromList [Quote]) "")]))],
                  W [(A_R sRule)] ],
                  [W [A_W (Word[SmbY' (Y "y1"), 
                  SmbQ (SMType.State P 2 (Set.fromList [Quote]) ""), 
                  SmbY (Y "y2"), 
                  SmbQ (SMType.State Q 3 (Set.fromList [Quote]) ""), 
                  SmbY (Y "y3")] )]])] 
                  ++ sort (genTransitionQ 21 sRule)

    let hub = GRType.Relation ([W[A_W' w, A_K "k1", A_W w, A_K "k2", A_W' w, A_K "k3",
              A_W w, A_K "k4", A_W' w, A_K "k5", A_W w, A_K "k6",
              A_W' w, A_K "k7", A_W w, A_K "k8", A_W' w, A_K "k9",
              A_W w, A_K "k10", A_W' w, A_K "k11", A_W w, A_K "k12"], 
              W' [A_K "k12", A_W' w, A_K "k11", A_W w, A_K "k10",
              A_W' w, A_K "k9", A_W w, A_K "k8", A_W' w, A_K "k7",
              A_W w, A_K "k6", A_W' w, A_K "k5", A_W w, A_K "k4",
              A_W' w, A_K "k3", A_W w, A_K "k2", A_W' w, A_K "k1", A_W w]],
              [W [(A_S "1")]]) 
    let rel = auxiliary  ++ transition ++ [hub] 
    let expectedGR = GR(a, rel)
    assertEqual "sm to gr convertion" (smToGR sm nk w) expectedGR

main :: IO ()
main = defaultMainWithOpts
       [testCase "simple cfg to tm1 map" simpleCfgToTM1MapTest, 
        testCase "sm to gr map" sMachineToGroupTest]
       mempty