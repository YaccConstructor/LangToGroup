module SMTests where

import SMType
import SMInterpreter
import Test.HUnit hiding (State)
import qualified TMType
import Data.Set (fromList)
import Helpers

smInterpretationTest :: Assertion
smInterpretationTest = do
    let y = Y $ defValue "a"
    let q0 = SMType.State Q "0" (fromList []) Nothing
    let q1 = SMType.State Q "1" (fromList []) Nothing
    let q0' = SMType.State Q "2" (fromList []) Nothing
    let q1' = SMType.State Q "3" (fromList []) Nothing
    let from1 = Word [SmbQ q0]
    let to1 = Word [SmbQ q0', SmbY' y]
    let from2 = Word [SmbQ q1]
    let to2 = Word [SmbQ q1']
    let r = SRule $ [(from1, to1), (from2, to2)]
    let sm = SM [[y]] [fromList [q0, q0'], fromList [q1, q1']] [r]
    let startWord = Word [SmbQ q0, SmbY y, SmbQ q1]
    let accessWord = Word [SmbQ q0', SmbQ q1']
    let history = (interpretSM startWord sm accessWord)
    assertEqual "assert access word" accessWord (last history)

smInterpretationTest2 :: Assertion
smInterpretationTest2 = do
    let y = Y $ defValue "a"
    let q0 = SMType.State Q "0" (fromList []) Nothing
    let q1 = SMType.State Q "1" (fromList []) Nothing
    let q0' = SMType.State Q "2" (fromList []) Nothing
    let q1' = SMType.State Q "3" (fromList []) Nothing
    let from1 = Word [SmbQ q0]
    let to1 = Word [SmbQ q0, SmbY' y]
    let from2 = Word [SmbQ q1]
    let to2 = Word [SmbQ q1']
    let r1 = SRule $ [(from1, to1), (from2, to2)]
    let r2 = SRule $ [(from1, to1)]
    let to3 = Word [SmbQ q0']
    let r3 = SRule $ [(from1, to3)]
    let sm = SM [[y]] [fromList [q0, q0'], fromList [q1, q1']] [r1, r2, r3]
    let startWord = Word [SmbQ q0, SmbY y, SmbY y, SmbQ q1]
    let accessWord = Word [SmbQ q0', SmbQ q1']
    let history = (interpretSM startWord sm accessWord)
    assertEqual "assert access word 2" accessWord (last history)

applyRuleTest :: Assertion
applyRuleTest = do
    let q1 = SmbQ $ SMType.State Q "1" (fromList []) Nothing
    let q2 = SmbQ $ SMType.State Q "2" (fromList []) Nothing
    let q3 = SmbQ $ SMType.State Q "3" (fromList []) Nothing
    let q4 = SmbQ $ SMType.State Q "4" (fromList []) Nothing
    let p1 = SmbQ $ SMType.State P "1" (fromList []) Nothing
    let p2 = SmbQ $ SMType.State P "2" (fromList []) Nothing
    let a = SMType.Y $ defValue "a"
    let b = SMType.Y $ defValue "b"
    let b' = SMType.Y $ TMType.Value "b" 1
    let c = SMType.Y $ defValue "c"
    let w = Word [q1, SmbY a, SmbY a, q2, SmbY b, q3, SmbY c, SmbY c, q4]
    let r = SRule [(Word [q1], Word [p1, SmbY' a]), (Word [q2, SmbY b, q3], Word [SmbY' a, p2, SmbY b', q3, SmbY c])]
    let resW = Word [p1, p2, SmbY b', q3, SmbY c, SmbY c, SmbY c, q4]
    assertEqual "assert right replace" resW (applyRule w r) 