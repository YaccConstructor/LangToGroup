module SMTests where

import SMType
import SMInterpreter
import Test.HUnit hiding (State)
import qualified TMType
import Data.Set (fromList)

smInterpretationTest :: Assertion
smInterpretationTest = do
    let y = Y $ TMType.Value "a"
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
    let y = Y $ TMType.Value "a"
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