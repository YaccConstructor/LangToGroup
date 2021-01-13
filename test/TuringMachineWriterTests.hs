module TuringMachineWriterTests (testShowOneTapeTms, testShowMultiTapeTms, testOneTapeTM2Tms, testMultiTapeTM2Tms) where

import Test.HUnit
import Data.Set (fromList)

import TmsType
import TMType
import TuringMachineWriter

oneTapeTM :: TM
oneTapeTM = TM (
    InputAlphabet (fromList [
        a, b
    ]),
    [
        TapeAlphabet (
            fromList [
                a, b
            ]
        )
    ],
    MultiTapeStates [
        fromList [
            q11
        ]
    ],
    Commands (
        fromList [
            [SingleTapeCommand ((a,   q11, RBS), (ES,  q11, RBS))]
        ]
    ),
    StartStates [q11],
    AccessStates [q22]
    )

multiTapeTM :: TM
multiTapeTM = TM (
    InputAlphabet (fromList [
        a, b
    ]),
    [
        TapeAlphabet (
            fromList [
                a
            ]
        ),
        TapeAlphabet (
            fromList [
                a'
            ]
        ),
        TapeAlphabet (
            fromList [
                a'
            ]
        )
    ],
    MultiTapeStates [
        fromList [
            q11
        ],
        fromList [
            q22
        ],
        fromList [
            q33
        ]
    ],
    Commands (
        fromList [
            [
                SingleTapeCommand ((a,  q11, RBS), (a,   q11, RBS)),
                SingleTapeCommand ((a', q22, RBS), (ES,  q22, RBS)),
                SingleTapeCommand ((a', q33, RBS), (a',  q33, RBS))
            ]
        ]
    ),
    StartStates [q11, q22, q33],
    AccessStates [q11, q22, q33]
    )

testShowOneTapeTms :: Assertion
testShowOneTapeTms = testShowTms oneTapeTms "\
    \name: TuringMachine\n\
    \init: Q__0__q_1v1\n\
    \accept: Q__0__q_2v2\n\n\
    \Q__0__q_1v1, a\n\
    \Q__0__q_1v1, _, >\n"

testShowMultiTapeTms :: Assertion
testShowMultiTapeTms = testShowTms multiTapeTms "\
    \name: TuringMachine\n\
    \init: Q__0__q_1v1__1__q_2v2__2__q_3v3\n\
    \accept: Q__0__q_1v1__1__q_2v2__2__q_3v3\n\n\
    \Q__0__q_1v1__1__q_2v2__2__q_3v3, a, à, à\n\
    \Q__0__q_1v1__1__q_2v2__2__q_3v3, a, _, à, -, >, -\n"

testOneTapeTM2Tms :: Assertion
testOneTapeTM2Tms = testTM2Tms oneTapeTM oneTapeTms

testMultiTapeTM2Tms :: Assertion
testMultiTapeTM2Tms = testTM2Tms multiTapeTM multiTapeTms

q11 :: TMType.State
q11 = TMType.State "q_{1}^{1}"

q22 :: TMType.State
q22 = TMType.State "q_{2}^{2}"

q33 :: TMType.State
q33 = TMType.State "q_{3}^{3}"

a :: Square
a = Value "a" 0

b :: Square
b = Value "b" 0

a' :: Square
a' = Value "a" 1

oneTapeTms :: Tms
oneTapeTms = Tms (
        "TuringMachine",
        [q11],
        [[q22]],
        [
            TmsCommand [
                TmsSingleTapeCommand (ChangeFromTo 'a' '_', q11, q11, MoveRight)
            ]
        ],
        [
            ['a', 'b']
        ]
    )

multiTapeTms :: Tms
multiTapeTms = Tms (
        "TuringMachine",
        [q11, q22, q33],
        [[q11, q22, q33]],
        [
            TmsCommand [
                TmsSingleTapeCommand (ChangeFromTo 'a' 'a', q11, q11, Stay),
                TmsSingleTapeCommand (ChangeFromTo 'à' '_', q22, q22, MoveRight),
                TmsSingleTapeCommand (ChangeFromTo 'à' 'à', q33, q33, Stay)
            ]
        ],
        [
            ['a'],
            ['à'],
            ['à']
        ]
    )

testShowTms :: Tms -> String -> Assertion
testShowTms tms res = assertEqual ("Invalid presentation of Tms.") res' res
    where
        res' = show tms

testTM2Tms :: TM -> Tms -> Assertion
testTM2Tms tm tms = case tm2tms tm of
    Left err -> assertFailure $ "Conversion from TM to Tms has failed: " ++ err
    Right tm' -> assertEqual ("TM is not correctly converted to Tms.") tm' tms
