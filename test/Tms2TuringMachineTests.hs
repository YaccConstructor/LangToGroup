module Tms2TuringMachineTests where

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
        [TmsState "Q_1"],
        [[TmsState "Q_0"]],
        [
            TmsCommand [
                TmsSingleTapeCommand (ChangeFromTo 'a' '_', tmq1, tmq0, Stay)
            ],
            TmsCommand [
                TmsSingleTapeCommand (ChangeFromTo 'b' 'b', tmq2, tmq3, MoveLeft)
            ]
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
        [TmsState "Q_1"],
        [[TmsState "Q_0"]],
        [
            TmsCommand [
                TmsSingleTapeCommand (Leave, tmq2, tmq3, Stay)
            ]
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
        [TmsState "Q_1"],
        [[TmsState "Q_0"]],
        [
            TmsCommand [
                TmsSingleTapeCommand (Leave, tmq2, tmq3, MoveRight)
            ]
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
        [TmsState "Q_1"],
        [[TmsState "Q_0"]],
        [
            TmsCommand [
                TmsSingleTapeCommand (ChangeFromTo 'c' '_', tmq1, tmq0, MoveRight)
            ]
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
        [TmsState "Q_1"],
        [[TmsState "Q_0"]],
        [
            TmsCommand [
                TmsSingleTapeCommand (ChangeFromTo 'c' 'c', tmq2, tmq3, MoveRight)
            ]
        ],
        ["abc"]
    )

turingMachineIdMove :: TuringMachine
turingMachineIdMove = fromList [
        ((q2, c), (R, q3))
    ]

(a, b, c) = (S 97, S 98, S 99)
(q0, q1, q2, q3, q4) = (finalState, startState, Q 2, Q 3, Q 4)
[tmq0, tmq1, tmq2, tmq3, tmq4] = TmsState <$> ("Q_" ++) . show <$> [0 .. 4]
