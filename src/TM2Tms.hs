-- |This module provides functionality for converting the Turing machine 'TMType.TM' to 'Tms'.
module TM2Tms (tm2tms, tmState2tmsState) where

import Data.List (transpose)
import Data.Set (toList)
import Data.Map (fromList, lookup, Map)
import Data.List.NonEmpty (reverse, NonEmpty(..), length)

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
        let tmsCmds = filter (not . noChangeCommand) $ concat tmsCmdsBlocks
        tmsAlph <- traverse alph2tmsAlph tapeAlphabets
        return $ Tms ("TM", tmState2tmsState <$> startStates, [tmState2tmsState <$> accessStates], tmsCmds, tmsAlph)
    where
        alph2tmsAlph :: TapeAlphabet -> Either String [Char]
        alph2tmsAlph (TapeAlphabet squares) = traverse toValue (toList squares)

tmState2tmsState :: State -> TmsState
tmState2tmsState (State s) = TmsState s
 
-- Section of helper functions.

-- |Make transitional state.
-- Being used in 'cmd2tmsTapeCmd' if more than one command is created.
makeTransSt :: State -> State -> State
makeTransSt (State from) (State to) = State $ "FROM_" ++ from ++ "_TO_" ++ to

-- |Convert 'TapeCommand' to non empty list of 'TmsSingleTapeCommand'.
cmd2tmsTapeCmd :: TapeCommand -> Either String (NonEmpty TmsSingleTapeCommand)

-- Command translation cases.

-- 'TM' : Do not change anything.
-- 'Tms': Do not change anything.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (ES, iniSt, RBS),
        (ES, folSt, RBS))
    ) = return $
    (TmsSingleTapeCommand (Leave, tmState2tmsState iniSt, tmState2tmsState folSt, Stay)) :|
    []

-- 'TM' : Insert value to the left from head.
-- 'Tms': Move head to left and put the value there.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (ES,                 iniSt, RBS),
        ((Value name nquts), folSt, RBS)
    )) = do
    ch <- quotName2Char name nquts
    let transit = tmState2tmsState $ makeTransSt iniSt folSt
    return $
        (TmsSingleTapeCommand  (Leave,                 tmState2tmsState iniSt,           transit, MoveLeft)) :|
        [(TmsSingleTapeCommand (ChangeFromTo '_' $ ch, transit,         tmState2tmsState folSt,   Stay))]

-- 'TM' : Erase symbol to the left from the head.
-- 'Tms': Erase (put empty symbol) value in head position and move head to right.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        ((Value name nquts), iniSt, RBS),
        (ES,                 folSt, RBS)
    )) = do
    ch <- quotName2Char name nquts
    return $
        (TmsSingleTapeCommand (ChangeFromTo ch '_', tmState2tmsState iniSt, tmState2tmsState folSt, MoveRight)) :|
        []

-- 'TM' : Replace value to left from the head.
-- 'Tms': Change value in head position.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        ((Value iniName iniNquts), iniSt, RBS),
        ((Value folName folNquts), folSt, RBS)
    )) = do
    iniCh <- quotName2Char iniName iniNquts
    folCh <- quotName2Char folName folNquts
    return $
        (TmsSingleTapeCommand
            (ChangeFromTo iniCh folCh,
            tmState2tmsState iniSt,
            tmState2tmsState folSt,
            Stay)) :|
        []

-- 'TM' : Check if tape is empty.
-- 'Tms': Check if tape is empty.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (LBS, iniSt, RBS),
        (LBS, folSt, RBS)
    )) = return $
        (TmsSingleTapeCommand (ChangeFromTo '_' '_', tmState2tmsState iniSt, tmState2tmsState folSt, Stay)) :|
        []

-- Command can not be translated to Tms format.
cmd2tmsTapeCmd cmd = fail $ "Command '" ++ show cmd ++ "' can not be converted to '[TmsSingleTapeCommand]'"

cmd2tmsTapeCmds :: [TapeCommand] -> Either String [TmsCommand]
cmd2tmsTapeCmds cmds = do
    tapeCmdSeqs <- traverse cmd2tmsTapeCmd cmds -- :: [NonEmpty TmsSingleTapeCommand]
    let tapeCmdSeqsRev = Prelude.map Data.List.NonEmpty.reverse tapeCmdSeqs
    let mxLen = foldl (\mx sq -> max mx (Data.List.NonEmpty.length sq)) 0 tapeCmdSeqs
    let sameLenCmds = fmap (fillSeqWithIdCmds mxLen) tapeCmdSeqsRev
    let sameLenCmdsRev = fmap Prelude.reverse sameLenCmds
    return $ TmsCommand <$> transpose sameLenCmdsRev
    where
        fillSeqWithIdCmds :: Int -> NonEmpty TmsSingleTapeCommand -> [TmsSingleTapeCommand]
        fillSeqWithIdCmds len (x :| xs) = replicate (len - (Prelude.length xs) - 1) (makeIdTapeCommand x) ++ pure x ++ xs
        makeIdTapeCommand :: TmsSingleTapeCommand -> TmsSingleTapeCommand
        makeIdTapeCommand (TmsSingleTapeCommand (_, _, st, _)) = TmsSingleTapeCommand (Leave, st, st, Stay)

-- |Convert 'Square' to 'Char'.
toValue :: Square -> Either String Char
toValue (Value name n) = do
    c <- quotName2Char name n
    return c
toValue _              = fail "Square is expected to be 'Value' to convert to 'Char'"

-- |Convert 'String' with 'Int' quotes to 'Char'.
-- Character length is exactly 1, so longer strings can not be supported, so as many quotes.
quotName2Char :: String -> Int -> Either String Char
quotName2Char (c : "") 0 = return c
quotName2Char (c : "") 1 = return $ toQuot c
quotName2Char name nQuot = fail $ "Can not convert string '" ++ name ++ "' with " ++ show nQuot ++ " quotes into 'Char'."

-- |Quoted (or just fancy) version of characters.
quoted :: Data.Map.Map Char Char
quoted = Data.Map.fromList [('a', 'à'), ('b', 'ƀ'), ('c', 'ć'), ('d', 'ď'), ('e', 'ė'), ('f', 'ƒ'), ('g', 'ĝ'), ('h', 'ĥ'), ('i', 'ĩ'), ('j', 'ĵ'), ('k', 'ķ'), ('l', 'ĺ'), ('m', 'ɱ'), ('n', 'ń'), ('o', 'ō'), ('p', 'ƥ'), ('q', 'ɋ'), ('r', 'ŕ'), ('s', 'ś'), ('t', 'ť'), ('u', 'ū'), ('v', 'ʌ'), ('w', 'ŵ'), ('x', '×'), ('y', 'ŷ'), ('z', 'ź'), ('A', 'Ã'), ('B', 'Ɓ'), ('C', 'Ć'), ('D', 'Đ'), ('E', 'Ė'), ('F', 'Ƒ'), ('G', 'Ĝ'), ('H', 'Ĥ'), ('I', 'Ĩ'), ('J', 'Ĵ'), ('K', 'Ķ'), ('L', 'Ĺ'), ('M', 'Ɯ'), ('N', 'Ń'), ('O', 'Ō'), ('P', 'Ƥ'), ('Q', 'Ɋ'), ('R', 'Ŕ'), ('S', 'Ś'), ('T', 'Ť'), ('U', 'Ū'), ('V', 'Ʌ'), ('W', 'Ŵ'), ('X', 'χ'), ('Y', 'Ŷ'), ('Z', 'Ź')]

toQuot :: Char -> Char
toQuot c = case Data.Map.lookup c quoted of
    Nothing -> error $ "Can not find character with quote for '" ++ pure c ++ "'"
    Just c' -> c'

-- |Check if command changes nothing, so it can be paifully removed.
noChangeCommand :: TmsCommand -> Bool
noChangeCommand (TmsCommand cmds) = all noChangeTapeCommand cmds
    where
        noChangeTapeCommand :: TmsSingleTapeCommand -> Bool
        noChangeTapeCommand (TmsSingleTapeCommand (Leave,            ini, fol, Stay)) | ini == fol           = True
        noChangeTapeCommand (TmsSingleTapeCommand (ChangeFromTo f t, ini, fol, Stay)) | ini == fol && f == t = True
        noChangeTapeCommand _                                                                                = False
