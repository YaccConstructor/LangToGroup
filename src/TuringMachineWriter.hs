-- |This module provides functionality for presenting the Turing machine 'TM' in a text format appropriate to this service: https://turingmachinesimulator.com/
-- Below this format will be called Tms.
module TuringMachineWriter (tm2tms) where

import GHC.Unicode (isAlphaNum)
import Data.List.Utils (replace)
import Data.List (intercalate, transpose)
import Data.Set (toList, fromList, Set)
import Data.Map (fromList, lookup, Map)
import Data.Char (ord, chr, toTitle)
import Data.List.NonEmpty (reverse, NonEmpty(..), length)

import TMType

-- |Type of Tms tape square action.
--
-- 'Leave' is leave any character unchanged.
--
-- 'ChangeFromTo f t' is change it from 'f' to 't'.
--
-- 'ChangeTo t' is change it from anything to 't'.
data TmsTapeSquare = Leave | ChangeFromTo Char Char | ChangeTo Char

-- |Type of Tms tape head movement
data TmsTapeHeadMovement = MoveLeft | Stay | MoveRight

instance Show TmsTapeHeadMovement where
    show MoveLeft  = "<"
    show Stay      = "-"
    show MoveRight = ">"

-- |Type of Tms command for one tape.
-- TmsSingleTapeCommand (action, prevState, nextState, movement).
newtype TmsSingleTapeCommand = TmsSingleTapeCommand (TmsTapeSquare, State, State, TmsTapeHeadMovement)

-- |Type of Tms command for entire Turing machine.
newtype TmsCommand = TmsCommand [TmsSingleTapeCommand]

-- |Type of Tms format.
-- Tms (name, initState, acceptStates, commands, tapeAlphabets).
newtype Tms = Tms (String, [State], [[State]], [TmsCommand], [[Char]])

instance Show Tms where
    show
        (Tms
            (name,
            init,
            acStates,
            commands,
            tapeAlphabets)
        ) = "name: " ++ name ++ "\n" ++
        "init: " ++ mergeMultipleTapeStates init ++ "\n" ++
        "accept: " ++ intercalate ", " (map mergeMultipleTapeStates acStates) ++ "\n\n" ++
        intercalate "\n\n" (map showTmsCommand commands)
        where
            showTmsCommand :: TmsCommand -> String
            showTmsCommand (TmsCommand tapeCommands) = intercalate "\n" $ map showSingleCmd $ combine $ map extCommand (zip tapeAlphabets tapeCommands)
            extCommand :: ([Char], TmsSingleTapeCommand) -> [((State, Char), (State, Char, TmsTapeHeadMovement))]
            extCommand (alph, (TmsSingleTapeCommand (Leave, iniSt, folSt, mv))) =
                [((iniSt, ch), (folSt, ch, mv)) | ch <- '_' : alph]
            extCommand (alph, (TmsSingleTapeCommand (ChangeFromTo cF cT, iniSt, folSt, mv))) =
                [((iniSt, cF), (folSt, cT, mv))]
            extCommand (alph, (TmsSingleTapeCommand (ChangeTo cT, iniSt, folSt, mv))) =
                [((iniSt, cF), (folSt, cT, mv)) | cF <- '_' : alph]
            combine = map Prelude.reverse . foldl (\combs cur -> flip (:) <$> combs <*> cur) [[]]
            showSingleCmd :: [((State, Char), (State, Char, TmsTapeHeadMovement))] -> String
            showSingleCmd cmds = intercalate ", " (iniStateName : iniSquares) ++ "\n" ++
                                 intercalate ", " (folStateName : folSquares ++ moves) ++ "\n"
                where
                    iniStateName = mergeMultipleTapeStates $ map (\((ini, _), (_, _, _)) -> ini) cmds
                    folStateName = mergeMultipleTapeStates $ map (\((_, _), (fol, _, _)) -> fol) cmds
                    iniSquares = map (\((_, iniSq), (_, _, _)) -> return iniSq) cmds
                    folSquares = map (\((_, _), (_, folSq, _)) -> return folSq) cmds
                    moves = map (\((_, _), (_, _, mv)) -> show mv) cmds

tm2tms :: TM -> Either String Tms
tm2tms
    (TM
        (inputAlphabet,
        tapeAlphabets, 
        MultiTapeStates multiTapeStates, 
        Commands commands, 
        StartStates startStates, 
        AccessStates accessStates)
    ) = do
        tmsCmdsBlocks <- traverse cmd2tmsTapeCmds (toList commands)
        let tmsCmds = filter (not . noChangeCommand) $ concat tmsCmdsBlocks
        tmsAlph <- traverse alph2tmsAlph tapeAlphabets
        return $ Tms ("TuringMachine", startStates, [accessStates], tmsCmds, tmsAlph)
    where
        alph2tmsAlph :: TapeAlphabet -> Either String [Char]
        alph2tmsAlph (TapeAlphabet squares) = traverse toValue (toList squares)
 
-- Section of helper functions.

-- |Process string so that it does not contain illegal characters.
filterStateName :: String -> String
filterStateName = replace "^" "v" . filter (\c -> isAlphaNum c || elem c ['_', '^'])

-- |Concat and filter list of states.
mergeMultipleTapeStates :: [State] -> String
mergeMultipleTapeStates = ("Q__" ++ ) . intercalate "__" . map filterStateName . enum
    where
        enum ss = fmap (\(num, (State s)) -> show num ++ "__" ++ s) (zip [0 ..] ss)

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
    (TmsSingleTapeCommand (Leave, iniSt, folSt, Stay)) :|
    []

-- 'TM' : Insert value to the left from head.
-- 'Tms': Move head to left and put the value there.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (ES,                 iniSt, RBS),
        ((Value name nquts), folSt, RBS)
    )) = return $
    (TmsSingleTapeCommand  (Leave,                                    iniSt,                     (makeTransSt iniSt folSt), MoveLeft)) :|
    [(TmsSingleTapeCommand (ChangeFromTo '_' $ name'n'quotes2Char name nquts, (makeTransSt iniSt folSt), folSt,                     Stay))]

-- 'TM' : Erase symbol to the left from the head.
-- 'Tms': Erase (put empty symbol) value in head position and move head to right.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        ((Value name nquts), iniSt, RBS),
        (ES,                 folSt, RBS)
    )) = return $
    (TmsSingleTapeCommand (ChangeFromTo (name'n'quotes2Char name nquts) '_', iniSt, folSt, MoveRight)) :|
    []

-- 'TM' : Replace value to left from the head.
-- 'Tms': Change value in head position.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        ((Value iniName iniNquts), iniSt, RBS),
        ((Value folName folNquts), folSt, RBS)
    )) = return $
    (TmsSingleTapeCommand
        (ChangeFromTo (name'n'quotes2Char iniName iniNquts) (name'n'quotes2Char folName folNquts),
        iniSt,
        folSt,
        Stay)) :|
    []

-- 'TM' : Check if tape is empty.
-- 'Tms': Check if tape is empty.
cmd2tmsTapeCmd (
    SingleTapeCommand (
        (LBS, iniSt, RBS),
        (LBS, folSt, RBS)
    )) = return $
    (TmsSingleTapeCommand (ChangeFromTo '_' '_', iniSt, folSt, Stay)) :|
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
        makeIdTapeCommand (TmsSingleTapeCommand (_, _, st, mv)) = TmsSingleTapeCommand (Leave, st, st, Stay)

-- |Convert 'Square' to 'Char'.
toValue :: Square -> Either String Char
toValue (Value name n) = pure $ name'n'quotes2Char name n
toValue _              = fail "Square is expected to be 'Value' to convert to 'Char'"

-- Character length is exactly 1, so longer strings can not be supported, so as many quotes.
name'n'quotes2Char :: String -> Int -> Char
name'n'quotes2Char (c : "") 0 = c
name'n'quotes2Char (c : "") 1 = toQuot c
name'n'quotes2Char name nQuot = error $ "Can not convert name '" ++ name ++ "' with " ++ show nQuot ++ "quotes into 'Char'."

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
