{-# LANGUAGE OverloadedStrings #-}

module Tms2TuringMachineTests (
    testTms2TuringMachineSimple,
    testTms2TuringMachineLeaveStay,
    testTms2TuringMachineLeaveMove,
    testTms2TuringMachineChangeMove,
    testTms2TuringMachineIdMove) where

import Test.HUnit

import TmsType
import TuringMachine as TM
import TuringMachine.Constructors (makeStandartTM)
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
        ["ab"]
    )

turingMachineSimple :: TuringMachine
turingMachineSimple = makeStandartTM [
        (startState, "a", S blank, finalState),
        (q2,         "b", M toLeft,   q3)
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
turingMachineLeaveStay = makeStandartTM [
        (q2, "a", S "a", q3),
        (q2, "b", S "b", q3),
        (q2, "c", S "c", q3),
        (q2, blank, S blank, q3)
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
turingMachineLeaveMove = makeStandartTM [
        (q2, blank, M toRight, q3),
        (q2, "a", M toRight, q3),
        (q2, "b", M toRight, q3),
        (q2, "c", M toRight, q3)
    ]

tmsChangeMove :: Tms
tmsChangeMove = Tms (
        "TuringMachine",
        TmsState "Q_1",
        [TmsState "Q_0"],
        [
            TmsCommand (tmq1, [TmsSingleTapeCommand (ChangeFromTo 'c' '_', MoveRight)], tmq0)
        ],
        ["c"]
    )

turingMachineChangeMove :: TuringMachine
turingMachineChangeMove = makeStandartTM [
        (q1, "c", S blank, trans),
        (trans, blank, M toRight, q0)
    ]
    where
        trans = state $ hash "Q_1_0"

tmsIdMove :: Tms
tmsIdMove = Tms (
        "TuringMachine",
        TmsState "Q_1",
        [TmsState "Q_0"],
        [
            TmsCommand (tmq2, [TmsSingleTapeCommand (ChangeFromTo 'c' 'c', MoveRight)], tmq3)
        ],
        ["c"]
    )

turingMachineIdMove :: TuringMachine
turingMachineIdMove = makeStandartTM [
        (q2, "c", M toRight, q3)
    ]

{-a :: Symbol
b :: Symbol
c :: Symbol
(a, b, c) = (S 97, S 98, S 99)-}

q0 :: TM.State
q1 :: TM.State
q2 :: TM.State
q3 :: TM.State
(q0, q1, q2, q3) = (finalState, startState, state 2, state 3)

tmq0 :: TmsState
tmq1 :: TmsState
tmq2 :: TmsState
tmq3 :: TmsState
[tmq0, tmq1, tmq2, tmq3] = TmsState . ("Q_" ++) . show <$> [0 .. 3]
