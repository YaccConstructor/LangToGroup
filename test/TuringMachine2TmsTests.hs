module TuringMachine2TmsTests where

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
                "TMTypes_TuringMachine",
                TmsState "Q_1",
                [TmsState "Q_0"],
                [
                    TmsCommand (tmq1, [TmsSingleTapeCommand (ChangeFromTo '_' '_', Stay)],      tmq0),
                    TmsCommand (tmq1, [TmsSingleTapeCommand (ChangeFromTo 'a' 'b', Stay)],      tmq2),
                    TmsCommand (tmq2, [TmsSingleTapeCommand (ChangeFromTo 'b' 'b', MoveLeft)],  tmq4),
                    TmsCommand (tmq3, [TmsSingleTapeCommand (ChangeFromTo 'c' 'a', Stay)],      tmq2),
                    TmsCommand (tmq4, [TmsSingleTapeCommand (ChangeFromTo 'a' 'a', MoveRight)], tmq3)
                ],
                ["abc"]
            )

        (a, b, c) = (S 1, S 2, S 3)
        (q2, q3, q4) = (Q 2, Q 3, Q 4)
        [tmq0, tmq1, tmq2, tmq3, tmq4] = TmsState <$> ("Q_" ++) . show <$> [0 .. 4]
