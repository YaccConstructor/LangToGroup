-- |This module provides functionality for converting the Turing machine 'TMType.TM' to 'Tms'.
module TM2Tms (tm2tms) where

import Data.List (transpose, intercalate)
import Data.Set (toList)
import Data.Map (fromList, lookup, Map)
import Data.List.NonEmpty (reverse, NonEmpty(..), length)
import Data.Maybe (fromMaybe)

import TMType
import TmsType

tm2tms :: TM -> Either String Tms
tm2tms
    (TM
        (_, -- Input Alphabet
        tapeAlphabets,
        _, -- Tape States
        Commands commands,
        StartStates startStates,
        AccessStates accessStates)
    ) = do
        tmsCmdsBlocks <- traverse cmd2tmsTapeCmds (toList commands)
        let tmsCmds = concat tmsCmdsBlocks
        tmsAlph <- traverse alph2tmsAlph tapeAlphabets
        return $ Tms (
            "TMType_TM",
            mergeMultipleNames $ (\(State s) -> s) <$> startStates,
            pure $ mergeMultipleNames $ (\(State s) -> s) <$> accessStates,
            tmsCmds,
            tmsAlph)
    where
        alph2tmsAlph :: TapeAlphabet -> Either String String
        alph2tmsAlph (TapeAlphabet squares) = traverse toValue (toList squares)

tmState2tmsState :: State -> TmsState
tmState2tmsState (State s) = TmsState s

-- Section of helper functions.

-- |Make transitional state.
-- Being used in 'cmd2tmsTapeCmd' if more than one command is created.
makeTransSt :: State -> State -> State
makeTransSt (State from) (State to) = State $ "FROM_" ++ from ++ "_TO_" ++ to

-- |Convert 'TapeCommand' to non empty list of 'TmsSingleTapeCommand'.
cmd2tmsTapeCmd :: TapeCommand -> Either String (NonEmpty OneTapeTMCommand)

-- Command translation cases.

-- 'TM' : Do not change anything.
-- 'Tms': Do not change anything.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (ES, iniSt, RBS),
        (ES, folSt, RBS))
    ) = return $
    (tmState2tmsState iniSt, TmsSingleTapeCommand (Leave, Stay), tmState2tmsState folSt) :|
    []

-- 'TM' : Insert value to the left from head.
-- 'Tms': Move head to left and put the value there.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (ES,                 iniSt, RBS),
        (Value name nquts, folSt, RBS)
    )) = do
    ch <- quotName2Char name nquts
    let transit = tmState2tmsState $ makeTransSt iniSt folSt
    return $
        (tmState2tmsState iniSt, TmsSingleTapeCommand (Leave,                   MoveLeft), transit) :|
        [(transit,               TmsSingleTapeCommand (ChangeFromTo '_' ch,   Stay),     tmState2tmsState folSt)]

-- 'TM' : Erase symbol to the left from the head.
-- 'Tms': Erase (put empty symbol) value in head position and move head to right.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (Value name nquts, iniSt, RBS),
        (ES,                 folSt, RBS)
    )) = do
    ch <- quotName2Char name nquts
    return $
        (tmState2tmsState iniSt, TmsSingleTapeCommand (ChangeFromTo ch '_', MoveRight), tmState2tmsState folSt) :|
        []

-- 'TM' : Replace value to left from the head.
-- 'Tms': Change value in head position.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (Value iniName iniNquts, iniSt, RBS),
        (Value folName folNquts, folSt, RBS)
    )) = do
    iniCh <- quotName2Char iniName iniNquts
    folCh <- quotName2Char folName folNquts
    return $
        (tmState2tmsState iniSt, TmsSingleTapeCommand (ChangeFromTo iniCh folCh, Stay), tmState2tmsState folSt) :|
        []

-- 'TM' : Check if tape is empty.
-- 'Tms': Check if tape is empty.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (LBS, iniSt, RBS),
        (LBS, folSt, RBS)
    )) = return $
        (tmState2tmsState iniSt, TmsSingleTapeCommand (ChangeFromTo '_' '_', Stay), tmState2tmsState folSt) :|
        []

-- Command can not be translated to Tms format.
cmd2tmsTapeCmd cmd = Left $ "Command '" ++ show cmd ++ "' can not be converted to '[TmsSingleTapeCommand]'"

cmd2tmsTapeCmds :: [TapeCommand] -> Either String [TmsCommand]
cmd2tmsTapeCmds tapeCmds = do
    tapeCmdSeqs <- traverse cmd2tmsTapeCmd tapeCmds
    let tapeCmdSeqsRev = Prelude.map Data.List.NonEmpty.reverse tapeCmdSeqs
    let mxLen = foldl (\mx sq -> max mx (Data.List.NonEmpty.length sq)) 0 tapeCmdSeqs
    let sameLenCmds = fmap (fillSeqWithIdCmds mxLen) tapeCmdSeqsRev
    let sameLenCmdsRev = transpose $ fmap Prelude.reverse sameLenCmds
    return $ (\cmds -> TmsCommand (startState cmds, commands cmds, finalState cmds)) <$> sameLenCmdsRev
    where
        fillSeqWithIdCmds :: Int -> NonEmpty OneTapeTMCommand -> [OneTapeTMCommand]
        fillSeqWithIdCmds len (x :| xs) = replicate (len - Prelude.length xs - 1) (makeIdTapeCommand x) ++ pure x ++ xs
        makeIdTapeCommand :: OneTapeTMCommand -> OneTapeTMCommand
        makeIdTapeCommand (_, _, st) = (st, TmsSingleTapeCommand (Leave, Stay), st)
        startState = mergeMultipleNames . fmap (\(TmsState s, _, _) -> s)
        finalState = mergeMultipleNames . fmap (\(_, _, TmsState f) -> f)
        commands = fmap $ \(_, x, _) -> x

-- |Convert 'Square' to 'Char'.
toValue :: Square -> Either String Char
toValue (Value name n) =
    quotName2Char name n
toValue _              = Left "Square is expected to be 'Value' to convert to 'Char'"

-- |Convert 'String' with 'Int' quotes to 'Char'.
-- Character length is exactly 1, so longer strings can not be supported, so as many quotes.
quotName2Char :: String -> Int -> Either String Char
quotName2Char (c : "") 0 = return c
quotName2Char (c : "") 1 = return $ toQuot c
quotName2Char name nQuot = Left $ "Can not convert string '" ++ name ++ "' with " ++ show nQuot ++ " quotes into 'Char'."

-- |Quoted (or just fancy) version of characters.
quoted :: Data.Map.Map Char Char
quoted = Data.Map.fromList [('a', 'à'), ('b', 'ƀ'), ('c', 'ć'), ('d', 'ď'), ('e', 'ė'), ('f', 'ƒ'), ('g', 'ĝ'), ('h', 'ĥ'), ('i', 'ĩ'), ('j', 'ĵ'), ('k', 'ķ'), ('l', 'ĺ'), ('m', 'ɱ'), ('n', 'ń'), ('o', 'ō'), ('p', 'ƥ'), ('q', 'ɋ'), ('r', 'ŕ'), ('s', 'ś'), ('t', 'ť'), ('u', 'ū'), ('v', 'ʌ'), ('w', 'ŵ'), ('x', '×'), ('y', 'ŷ'), ('z', 'ź'), ('A', 'Ã'), ('B', 'Ɓ'), ('C', 'Ć'), ('D', 'Đ'), ('E', 'Ė'), ('F', 'Ƒ'), ('G', 'Ĝ'), ('H', 'Ĥ'), ('I', 'Ĩ'), ('J', 'Ĵ'), ('K', 'Ķ'), ('L', 'Ĺ'), ('M', 'Ɯ'), ('N', 'Ń'), ('O', 'Ō'), ('P', 'Ƥ'), ('Q', 'Ɋ'), ('R', 'Ŕ'), ('S', 'Ś'), ('T', 'Ť'), ('U', 'Ū'), ('V', 'Ʌ'), ('W', 'Ŵ'), ('X', 'χ'), ('Y', 'Ŷ'), ('Z', 'Ź')]

toQuot :: Char -> Char
toQuot c = fromMaybe (error $ "Can not find character with quote for '" ++ pure c ++ "'") (Data.Map.lookup c quoted)

-- |Concat and filter list of states.
mergeMultipleNames :: [String] -> TmsState
mergeMultipleNames = TmsState . ("Q__" ++ ) . intercalate "__" . map filterStateName . enum
    where
        enum ss = fmap (\(num, s) -> show num ++ "__" ++ s) (zip [0 ..] ss)
