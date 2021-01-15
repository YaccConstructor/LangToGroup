module Tms2turingMachineTests where

import Test.HUnit

import TmsType
import TMTypes
import TuringMachine2Tms


testTuringMachine2Tms :: Assertion
testTuringMachine2Tms = assertEqual "Invalid conversion from TMTypes.TuringMachine to Tms" (turingMachine2tms turingMachine) tms
    where
        turingMachine :: TuringMachine
        turingMachine = fromList [
                ((startState, a), (C b, q2)),
                ((q2,         b), (L,   q4)),
                ((q4,         a), (R,   q3)),
                ((q3,         c), (C a, q2)),
                ((startState, emptySymbol), (C emptySymbol, finalState))
            ]

        tms :: Tms
        tms = Tms (
                "TMTypes.TuringMachine",
                [TmsState "Q_1"],
                [[TmsState "Q_0"]],
                [
                    TmsCommand [
                        TmsSingleTapeCommand (ChangeFromTo '_' '_', tmq1, tmq0, Stay)
                    ],
                    TmsCommand [
                        TmsSingleTapeCommand (ChangeFromTo 'a' 'b', tmq1, tmq2, Stay)
                    ],
                    TmsCommand [
                        TmsSingleTapeCommand (ChangeFromTo 'b' 'b', tmq2, tmq4, MoveLeft)
                    ],
                    TmsCommand [
                        TmsSingleTapeCommand (ChangeFromTo 'c' 'a', tmq3, tmq2, Stay)
                    ],
                    TmsCommand [
                        TmsSingleTapeCommand (ChangeFromTo 'a' 'a', tmq4, tmq3, MoveRight)
                    ]
                ],
                ["_abc"]
            )

        (a, b, c) = (S 1, S 2, S 3)
        (q2, q3, q4) = (Q 2, Q 3, Q 4)
        [tmq0, tmq1, tmq2, tmq3, tmq4] = TmsState <$> ("Q_" ++) . show <$> [0 .. 4]
