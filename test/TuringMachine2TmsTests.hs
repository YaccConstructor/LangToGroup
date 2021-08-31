module TuringMachine2TmsTests where

import Test.HUnit

import TmsType
import TuringMachine.Constructors
import TuringMachine2Tms


testTuringMachine2Tms :: Assertion
testTuringMachine2Tms = assertEqual "Invalid conversion from TMTypes.TuringMachine to Tms" (turingMachine2tms turingMachine) tms
    where
        turingMachine :: TuringMachine
        turingMachine = makeStandartTM [
                (startState, 'a', C 'b',      q2),
                (q2,         'b', M' toLeft,  q4),
                (q4,         'a', M' toRight, q3),
                (q3,         'c', C 'a',      q2),
                (startState, blankChar, C blankChar, finalState)
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

        (q2, q3, q4) = (state 2, state 3, state 4)
        [tmq0, tmq1, tmq2, tmq3, tmq4] = TmsState . ("Q_" ++) . show <$> [0 .. 4]
