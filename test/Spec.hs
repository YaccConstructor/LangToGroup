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

cfgToPdaMapTest :: Assertion
cfgToPdaMapTest = do
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

main :: IO ()
main = defaultMainWithOpts
       [testCase "cfg to pda map" cfgToPdaMapTest]
       mempty