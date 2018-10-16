import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import CfgToTm1Mapper
import GrammarType
import PdaType
import Data.Set (Set)
import qualified Data.Set as Set

simpleCfgToPdaMapTest :: Assertion
simpleCfgToPdaMapTest = do
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
    let startState = PdaType.State 's'
    let finalState = PdaType.State 'f'
    let states = PdaType.States (Set.fromList [startState, finalState])
    let expectedPda =
            Pda (
                states,
                InputAlphabet (Set.fromList [letter_a]),
                StackAlphabet (Set.fromList [letter_S,letter_a]),
                TransitionRelations (Set.fromList [
                    TransitionRelation ((startState, emptySymbol, emptySymbol), (finalState, [Letter 'S'])), 
                    TransitionRelation ((finalState, emptySymbol, letter_S), (finalState, [letter_a])),
                    TransitionRelation ((finalState, letter_a, letter_a),(finalState, [emptySymbol]))
                    ]),
                startState,
                InitialStackSymbols [], 
                AcceptingStates (States (Set.fromList [finalState]))
            )
    assertEqual "simple cfg to pda convertion" (mapCfgToPda grammar) expectedPda

cfgToPdaMapTest :: Assertion
cfgToPdaMapTest = do
    let nonterminal_S = Nonterminal 'S'
    let nonterminal_A = Nonterminal 'A'
    let nonterminal_B = Nonterminal 'B'
    let nonterminal_C = Nonterminal 'C'
    let terminal_a = Terminal 'a'
    let terminal_b = Terminal 'b'
    let grammar =
            Grammar(
                Nonterminals (Set.fromList [nonterminal_S, nonterminal_A, nonterminal_B, nonterminal_C]), 
                Terminals (Set.fromList [terminal_a, terminal_b]), 
                Relations (Set.fromList [
                            Relation (nonterminal_S, [N nonterminal_A, N nonterminal_C]),
                            Relation (nonterminal_A, [N nonterminal_C, N nonterminal_B]),
                            Relation (nonterminal_B, [N nonterminal_A, N nonterminal_C]),
                            Relation (nonterminal_C, [T terminal_a]),
                            Relation (nonterminal_B, [T terminal_b])
                            ]),
                nonterminal_S
            )
    let startState = PdaType.State 's'
    let finalState = PdaType.State 'f'
    let states = PdaType.States (Set.fromList [startState, finalState])
    let letter_S = Letter 'S'
    let letter_A = Letter 'A'
    let letter_B = Letter 'B'
    let letter_C = Letter 'C'
    let letter_a = Letter 'a'
    let letter_b = Letter 'b'
    let expectedPda =
            Pda (
                states,
                InputAlphabet (Set.fromList [letter_a, letter_b]),
                StackAlphabet (Set.fromList [letter_S, letter_A, letter_B, letter_C, letter_a, letter_b]),
                TransitionRelations (Set.fromList [
                    -- start transition
                    TransitionRelation ((startState, emptySymbol, emptySymbol), (finalState, [Letter 'S'])),
                    -- mapped relations
                    TransitionRelation ((finalState, emptySymbol, letter_S), (finalState, [letter_A, letter_C])),
                    TransitionRelation ((finalState, emptySymbol, letter_A), (finalState, [letter_C, letter_B])),
                    TransitionRelation ((finalState, emptySymbol, letter_B), (finalState, [letter_A, letter_C])),
                    TransitionRelation ((finalState, emptySymbol, letter_C), (finalState, [letter_a])),
                    TransitionRelation ((finalState, emptySymbol, letter_B), (finalState, [letter_b])), 
                    -- terminals transition
                    TransitionRelation ((finalState, letter_a, letter_a),(finalState, [emptySymbol])),
                    TransitionRelation ((finalState, letter_b, letter_b),(finalState, [emptySymbol]))
                    ]),
                startState,
                InitialStackSymbols [], 
                AcceptingStates (States (Set.fromList [finalState]))
            )
    assertEqual "cfg to pda convertion" (mapCfgToPda grammar) expectedPda

main :: IO ()
main = defaultMainWithOpts
       [testCase "simple cfg to pda map" simpleCfgToPdaMapTest,
       testCase "cfg to pda map" cfgToPdaMapTest]
       mempty