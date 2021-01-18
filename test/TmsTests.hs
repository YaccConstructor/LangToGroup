module TmsTests (testShowOneTapeTms,
                 testShowMultiTapeTms,
                 testOneTapeTM2Tms,
                 testMultiTapeTM2Tms) where

import Test.HUnit
import Data.Set (fromList)

import TmsType
import TMType
import TM2Tms

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
            q1
        ]
    ],
    Commands (
        fromList [
            [SingleTapeCommand ((a, q1, RBS), (ES, q1, RBS))]
        ]
    ),
    StartStates [q1],
    AccessStates [q2]
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
            q1
        ],
        fromList [
            q2
        ],
        fromList [
            q3
        ]
    ],
    Commands (
        fromList [
            [
                SingleTapeCommand ((a,  q1, RBS), (a,   q1, RBS)),
                SingleTapeCommand ((a', q2, RBS), (ES,  q2, RBS)),
                SingleTapeCommand ((a', q3, RBS), (a',  q3, RBS))
            ]
        ]
    ),
    StartStates [q1, q2, q3],
    AccessStates [q1, q2, q3]
    )

testShowOneTapeTms :: Assertion
testShowOneTapeTms = testShowTms oneTapeTms "\
    \name: TMType_TM\
    \init: Q__0__q_1v1\
    \accept: Q__0__q_2v2\
    \Q__0__q_1v1, a\n\
    \Q__0__q_1v1, _, >"

testShowMultiTapeTms :: Assertion
testShowMultiTapeTms = testShowTms multiTapeTms "\
    \name: TMType_TM\
    \init: Q__0__q_1v1__1__q_2v2__2__q_3v3\
    \accept: Q__0__q_1v1__1__q_2v2__2__q_3v3\
    \Q__0__q_1v1__1__q_2v2__2__q_3v3, a, à, à\
    \Q__0__q_1v1__1__q_2v2__2__q_3v3, a, _, à, -, >, -"

testOneTapeTM2Tms :: Assertion
testOneTapeTM2Tms = testTM2Tms oneTapeTM oneTapeTms

testMultiTapeTM2Tms :: Assertion
testMultiTapeTM2Tms = testTM2Tms multiTapeTM multiTapeTms

q11 :: TmsState
q11 = TmsState "q_{1}^{1}"

q22 :: TmsState
q22 = TmsState "q_{2}^{2}"

q33 :: TmsState
q33 = TmsState "q_{3}^{3}"

q1 :: TMType.State
q1 = TMType.State "q_{1}^{1}"

q2 :: TMType.State
q2 = TMType.State "q_{2}^{2}"

q3 :: TMType.State
q3 = TMType.State "q_{3}^{3}"

a :: Square
a = Value "a" 0

b :: Square
b = Value "b" 0

a' :: Square
a' = Value "a" 1

oneTapeTms :: Tms
oneTapeTms = Tms (
        "TMType_TM",
        TmsState "Q__0__q_1v1",
        [TmsState "Q__0__q_2v2"],
        [
            TmsCommand (TmsState "Q__0__q_1v1", [TmsSingleTapeCommand (ChangeFromTo 'a' '_', MoveRight)], TmsState "Q__0__q_1v1")
        ],
        [
            ['a', 'b']
        ]
    )

multiTapeTms :: Tms
multiTapeTms = Tms (
        "TMType_TM",
        TmsState "Q__0__q_1v1__1__q_2v2__2__q_3v3",
        [TmsState "Q__0__q_1v1__1__q_2v2__2__q_3v3"],
        [
            TmsCommand (
                TmsState "Q__0__q_1v1__1__q_2v2__2__q_3v3",
                [
                    TmsSingleTapeCommand (ChangeFromTo 'a' 'a', Stay),
                    TmsSingleTapeCommand (ChangeFromTo 'à' '_', MoveRight),
                    TmsSingleTapeCommand (ChangeFromTo 'à' 'à', Stay)
                ],
                TmsState "Q__0__q_1v1__1__q_2v2__2__q_3v3"
            )
        ],
        [
            ['a'],
            ['à'],
            ['à']
        ]
    )

testShowTms :: Tms -> String -> Assertion
testShowTms tms res = assertEqual ("Invalid presentation of Tms.") (nonEndl res') (nonEndl res)
    where
        res' = show tms
        nonEndl = filter (/= '\n')

testTM2Tms :: TM -> Tms -> Assertion
testTM2Tms tm tms = case tm2tms tm of
    Left err -> assertFailure $ "Conversion from TM to Tms has failed: " ++ err
    Right tm' -> assertEqual ("TM is not correctly converted to Tms.") tm' tms
