{-# LANGUAGE OverloadedStrings #-}

module TmsParserTests where

import Data.Char (isSeparator)
import Test.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Byte (string)
import Data.Text (Text, pack)
import Control.Monad (guard)

import TmsParser
import TmsType

testParse :: Text -> Tms -> Assertion
testParse asStr actualTms = do
    parsedTms <- case parseMaybe parser asStr of
        Nothing   -> fail $ "Tms parsing failure."
        Just t    -> return t
    guard $ parsedTms == actualTms

test1tape1cmd :: Assertion
test1tape1cmd = testParse str1tape1cmd tms1tape1cmd

test1tape3cmd :: Assertion
test1tape3cmd = testParse str1tape3cmd tms1tape3cmd

test3tape3cmd :: Assertion
test3tape3cmd = testParse str3tape3cmd tms3tape3cmd

str1tape1cmd :: Text
str1tape1cmd = pack "name: some\n\
               \init: q0\n\
               \accept: q1\n\
               \q0, a\n\
               \q1, b, -"

tms1tape1cmd :: Tms
tms1tape1cmd = Tms (
        "some",
        TmsState "q0",
        [TmsState "q1"],
        [
            TmsCommand (TmsState "q0",
                        [
                            TmsSingleTapeCommand (ChangeFromTo 'a' 'b', Stay)
                        ],
                        TmsState "q1")
        ],
        ["ab"]
    )

str1tape3cmd :: Text
str1tape3cmd = pack "name: SomeOther\n\
               \init: q0\n\
               \accept: q3\n\
               \q0,   _\n\
               \q1,   a,   >\n\n\
               \q1   ,a\n\
               \ q2,  b, <\n\n\
               \ q3  ,c\n\n\n\n\
               \ q2,  _, -"

tms1tape3cmd :: Tms
tms1tape3cmd = Tms (
        "SomeOther",
        TmsState "q0",
        [TmsState "q3"],
        [
            TmsCommand (TmsState "q0",
                        [
                            TmsSingleTapeCommand (ChangeFromTo '_' 'a', MoveRight)
                        ],
                        TmsState "q1"),

            TmsCommand (TmsState "q1",
                        [
                            TmsSingleTapeCommand (ChangeFromTo 'a' 'b', MoveLeft)
                        ],
                        TmsState "q2"),

            TmsCommand (TmsState "q3",
                        [
                            TmsSingleTapeCommand (ChangeFromTo 'c' '_', Stay)
                        ],
                        TmsState "q2")
        ],
        ["abc"]
    )


str3tape3cmd :: Text
str3tape3cmd = pack "name: ThreeTapeThreeCmds\n\
               \init:   Q0\n\
               \accept: Q4\n\
               \\n\
               \Q0, _, _, a\n\
               \Q0, _, b, a, <, -, >\n\
               \\n\
               \Q2, c, _, a\n\
               \Q0, c, _, a, >, >, <\n\
               \\n\
               \Q2, c, _, a\n\
               \Q0, c, _, a, -, <, >"

tms3tape3cmd :: Tms
tms3tape3cmd = Tms (
        "ThreeTapeThreeCmds",
        TmsState "Q0",
        [TmsState "Q4"],
        [
            TmsCommand (TmsState "Q0",
                        [
                            TmsSingleTapeCommand (ChangeFromTo '_' '_', MoveLeft),
                            TmsSingleTapeCommand (ChangeFromTo '_' 'b', Stay),
                            TmsSingleTapeCommand (ChangeFromTo 'a' 'a', MoveRight)
                        ],
                        TmsState "Q0"),

            TmsCommand (TmsState "Q2",
                        [
                            TmsSingleTapeCommand (ChangeFromTo 'c' 'c', MoveRight),
                            TmsSingleTapeCommand (ChangeFromTo '_' '_', MoveRight),
                            TmsSingleTapeCommand (ChangeFromTo 'a' 'a', MoveLeft)
                        ],
                        TmsState "Q0"),

            TmsCommand (TmsState "Q2",
                        [
                            TmsSingleTapeCommand (ChangeFromTo 'c' 'c', Stay),
                            TmsSingleTapeCommand (ChangeFromTo '_' '_', MoveLeft),
                            TmsSingleTapeCommand (ChangeFromTo 'a' 'a', MoveRight)
                        ],
                        TmsState "Q0")
        ],
        ["c", "b", "a"]
    )
