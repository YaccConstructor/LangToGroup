module Tms2TuringMachineTests (
    testTms2TuringMachineSimple,
    testTms2TuringMachineLeaveStay,
    testTms2TuringMachineLeaveMove,
    testTms2TuringMachineChangeMove,
    testTms2TuringMachineIdMove) where

import Test.HUnit

import TmsType
import TMTypes
import Tms2TuringMachine

testTms2TuringMachine :: Tms -> TuringMachine -> Assertion
testTms2TuringMachine tms turingMachine = case tms2turingMachine tms of
    Left err -> assertFailure $ "Conversion from Tms to TMTypes.TuringMachine failed: " ++ err
    Right tm -> assertEqual "Invalid conversion from Tms to TMTypes.TuringMachine" turingMachine tm

testTms2TuringMachineSimple ::  Assertion
testTms2TuringMachineSimple = testTms2TuringMachine tmsSimple turingMachineSimple

testTms2TuringMachineLeaveStay ::  Assertion
testTms2TuringMachineLeaveStay = testTms2TuringMachine tmsLeaveStay turingMachineLeaveStay

testTms2TuringMachineLeaveMove ::  Assertion
testTms2TuringMachineLeaveMove = testTms2TuringMachine tmsLeaveMove turingMachineLeaveMove

testTms2TuringMachineChangeMove ::  Assertion
testTms2TuringMachineChangeMove = testTms2TuringMachine tmsChangeMove turingMachineChangeMove

testTms2TuringMachineIdMove ::  Assertion
testTms2TuringMachineIdMove = testTms2TuringMachine tmsIdMove turingMachineIdMove

tmsSimple :: Tms
tmsSimple = Tms (
        "TuringMachine",
        TmsState "Q_1",
        [TmsState "Q_0"],
        [
            TmsCommand (tmq1, [TmsSingleTapeCommand (ChangeFromTo 'a' '_', Stay)],     tmq0),
            TmsCommand (tmq2, [TmsSingleTapeCommand (ChangeFromTo 'b' 'b', MoveLeft)], tmq3)
        ],
        ["abc"]
    )

turingMachineSimple :: TuringMachine
turingMachineSimple = fromList [
        ((startState, a), (C emptySymbol, finalState)),
        ((q2,         b), (L,             q3))
    ]

tmsLeaveStay :: Tms
tmsLeaveStay = Tms (
        "TuringMachine",
        TmsState "Q_1",
        [TmsState "Q_0"],
        [
            TmsCommand (tmq2, [TmsSingleTapeCommand (Leave, Stay)], tmq3)
        ],
        ["abc"]
    )

turingMachineLeaveStay :: TuringMachine
turingMachineLeaveStay = fromList [
        ((q2, a), (C a, q3)),
        ((q2, b), (C b, q3)),
        ((q2, c), (C c, q3)),
        ((q2, emptySymbol), (C emptySymbol, q3))
    ]

tmsLeaveMove :: Tms
tmsLeaveMove = Tms (
        "TuringMachine",
        TmsState "Q_1",
        [TmsState "Q_0"],
        [
            TmsCommand (tmq2, [TmsSingleTapeCommand (Leave, MoveRight)], tmq3)
        ],
        ["abc"]
    )

turingMachineLeaveMove :: TuringMachine
turingMachineLeaveMove = fromList [
        ((q2, emptySymbol), (R, q3)),
        ((q2, a), (R, q3)),
        ((q2, b), (R, q3)),
        ((q2, c), (R, q3))
    ]

tmsChangeMove :: Tms
tmsChangeMove = Tms (
        "TuringMachine",
        TmsState "Q_1",
        [TmsState "Q_0"],
        [
            TmsCommand (tmq1, [TmsSingleTapeCommand (ChangeFromTo 'c' '_', MoveRight)], tmq0)
        ],
        ["abc"]
    )

turingMachineChangeMove :: TuringMachine
turingMachineChangeMove = fromList [
        ((q1, c), (C emptySymbol, trans)),
        ((trans, emptySymbol), (R, q0))
    ]
    where
        trans = Q $ hash "Q_1_0"

tmsIdMove :: Tms
tmsIdMove = Tms (
        "TuringMachine",
        TmsState "Q_1",
        [TmsState "Q_0"],
        [
            TmsCommand (tmq2, [TmsSingleTapeCommand (ChangeFromTo 'c' 'c', MoveRight)], tmq3)
        ],
        ["abc"]
    )

turingMachineIdMove :: TuringMachine
turingMachineIdMove = fromList [
        ((q2, c), (R, q3))
    ]

a :: Symbol
b :: Symbol
c :: Symbol
(a, b, c) = (S 97, S 98, S 99)

q0 :: TMTypes.State
q1 :: TMTypes.State
q2 :: TMTypes.State
q3 :: TMTypes.State
(q0, q1, q2, q3) = (finalState, startState, Q 2, Q 3)

tmq0 :: TmsState
tmq1 :: TmsState
tmq2 :: TmsState
tmq3 :: TmsState
[tmq0, tmq1, tmq2, tmq3] = TmsState . ("Q_" ++) . show <$> [0 .. 3]
