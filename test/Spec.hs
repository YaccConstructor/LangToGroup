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
import SMType
import GRType
import SMachineToGroup
import Data.List
import TMInterpreter
import ConfigType

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
    let q1 = TMType.State "q_3^2"
    let expectedConfigs = Configs ([
            [([leftBoundingLetter, "a"], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter], startStateSecondTape, [rightBoundingLetter])],
            [([leftBoundingLetter, "a"], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter, "S"], intermediateStateSecondTape, [rightBoundingLetter])],
            [([leftBoundingLetter, "a"], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter, "a'"], q1, [rightBoundingLetter])],
            [([leftBoundingLetter, "a"], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter, "a'"], intermediateStateSecondTape, [rightBoundingLetter])],
            [([leftBoundingLetter], startStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter], intermediateStateSecondTape, [rightBoundingLetter])],
            [([leftBoundingLetter], finalStateFirstTape, [rightBoundingLetter]), ([leftBoundingLetter], finalStateSecondTape, [rightBoundingLetter])]
            ])
    
    assertEqual "config test" expectedConfigs (interpretTM ["a"] $ mapCfgToTM test1Grammar)

simpleCfgToTMMapTest :: Assertion
simpleCfgToTMMapTest = do
    let letter_a = "a"
    let letter_S = "S"
    let workState = TMType.State "q_3^2"
    
    let expectedTM = TM (
            InputAlphabet (Set.fromList [letter_a]),
            [TapeAlphabet (Set.fromList [letter_a]), TapeAlphabet (Set.fromList [getDisjoinLetter letter_a, letter_S])],
            MultiTapeStates [(Set.fromList [startStateFirstTape, finalStateFirstTape]), 
                            (Set.fromList [startStateSecondTape, intermediateStateSecondTape, finalStateSecondTape, workState])],
            Commands (Set.fromList [
                [
                    SingleTapeCommand (
                        (letter_a, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (letter_a, 
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
                        (letter_a, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (letter_a, 
                        startStateFirstTape, 
                        rightBoundingLetter)
                        ),
                    SingleTapeCommand (
                        (letter_S, 
                        intermediateStateSecondTape, 
                        rightBoundingLetter), 
                        (getDisjoinLetter letter_a, 
                        workState, 
                        rightBoundingLetter)
                        )
                    ],
                [
                    SingleTapeCommand (
                        (letter_a, 
                        startStateFirstTape, 
                        rightBoundingLetter), 
                        (letter_a, 
                        startStateFirstTape, 
                        rightBoundingLetter)
                        ), 
                    SingleTapeCommand (
                        (getDisjoinLetter letter_a, 
                        workState, 
                        rightBoundingLetter), 
                        (getDisjoinLetter letter_a, 
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
                        (getDisjoinLetter letter_a, 
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

-- cfgWithEpsToTMMapTest :: Assertion
-- cfgWithEpsToTMMapTest = do
--     let letter_a = "a"
--     let letter_S = "S"
--     let workState = TMType.State "q_3^2"
    
--     let expectedTM = TM (
--             InputAlphabet (Set.fromList [letter_a]),
--             [TapeAlphabet (Set.fromList [letter_a]), TapeAlphabet (Set.fromList [getDisjoinLetter letter_a, letter_S])],
--             MultiTapeStates [(Set.fromList [startStateFirstTape, finalStateFirstTape]), 
--                             (Set.fromList [startStateSecondTape, intermediateStateSecondTape, finalStateSecondTape, workState])],
--             Commands (Set.fromList [
--                 [
--                 ]),
--             StartStates [startStateFirstTape, startStateSecondTape],
--             AccessStates [finalStateFirstTape, finalStateSecondTape]
--                 )
--     assertEqual "simple cfg to TMs convertion" expectedTM (mapCfgToTM test1Grammar)

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

merge l1 l2 = foldl (\x (a,b) -> x ++ [a,b]) [] (zip l1 l2) 

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
             W [(A_S "delta")]]
             ++ [ W[A_K ("k" ++ intToString (i))]|i <- [1..12]]
             ++ [W [A_Y (Y "y1")]] 
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
                     W [(A_R sRule)]])]
                     ++ [GRType.Relation([W [A_R sRule], 
                         W [A_K ("k" ++ intToString (i))]],
                         [W [A_K ("k" ++ intToString (i))],  
                         W [A_R sRule]]) |i <- [1..12]]
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

    let lst_k = [A_K ("k" ++ intToString (i))|i <- [1..12]]
    let lst_aw' = [A_W' w | i <- [1..6]]
    let lst_aw = [A_W w | i <- [1..6]]
    let lst_w = merge lst_aw' lst_aw
    let hub = GRType.Relation ( [W (merge lst_w lst_k), W' (merge (reverse lst_k) lst_w)],
              [W [(A_S "1")]]) 
    let rel = auxiliary  ++ transition ++ [hub] 
    let expectedGR = GR(a, rel)
    assertEqual "sm to gr convertion" expectedGR (smToGR sm nk w)

main :: IO ()
main = defaultMainWithOpts
       [testCase "simple cfg to TM map" simpleCfgToTMMapTest, 
        testCase "sm to gr map" sMachineToGroupTest,
        testCase "cft to TM to config test" configsTest]
       mempty
