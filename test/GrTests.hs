module GrTests where

import SMType
import Test.HUnit
import GRType
import qualified TMType
import SM2GR
import Data.Set (fromList)
import Helpers

sm2grTest :: Assertion
sm2grTest = do
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
    let accessWord = Word [SmbQ q0', SmbQ q1']
    let gr = sm2grEmpty (sm, accessWord)
    let as = [A_Y y, A_Q q0, A_Q q1, A_Q q0', A_Q q1', A_R r] ++ k
    let powr x = (SmbA' $ A_R r) : x ++ [SmbA $ A_R r]
    let u = [SmbA $ A_Q q0', SmbA $ A_Q q1']
    let u' = [SmbA' $ A_Q q1', SmbA' $ A_Q q0']
    let relations = [   Relation (powr [smb2As $ SmbQ q0], map smb2As [SmbQ q0', SmbY' y]),
                        Relation (powr [smb2As $ SmbQ q1], [smb2As $ SmbQ q1'])] ++
                        [Relation ([SmbA $ A_R r, SmbA x], [SmbA x, SmbA $ A_R r]) | x <- [A_Y y] ++ k] ++
                        [Relator (u' ++ [SmbA $ A_K 1] ++ u ++ [SmbA $ A_K 2] ++ u' ++  [SmbA' $ A_K 1] ++ u ++ [SmbA' $ A_K 2])]
    let expectedGr = GR (fromList as, fromList relations)
    assertEqual "assert group" expectedGr gr