{-# LANGUAGE OverloadedStrings #-}

-- |This module provides types for parsing 'Tms' Turing machines.
module TmsParser (parser, parseTms) where

import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char
import Text.Megaparsec.Byte (string)
import Data.Text (Text, pack)
import Data.String
import Control.Monad (guard)

import TMTypes
import TmsType
import ParsingHelpers

-- | Empty space.
empty :: Parser ()
empty = many separatorChar >> return ()

-- | Token.
tok :: Parser a -> Parser a
tok p = do
    empty
    v <- p
    empty
    return v

-- | Identifier.
pIdentifier :: Parser String
pIdentifier = do
    letters <- tok pLetters
    return letters
    where
        pLetters = do
            first <- letterChar
            rest <- many pChar
            return $ first : rest

-- | Comma.
comma :: Parser ()
comma = do
    tok $ char ','
    return ()

-- | Pair '<key>: <value>\n'.
pKeyValue :: Tokens Text -> Parser a -> Parser a
pKeyValue key value = do
    string key
    tok $ char ':'
    x <- value
    tok $ many newline
    return x

-- | TmsTapeHeadMovement
pMove :: Parser Char
pMove = char '<' <|> char '-' <|> char '>'

mvChar2TmsHeadMove :: Char -> Maybe TmsTapeHeadMovement
mvChar2TmsHeadMove c = case c of
    '<' -> return MoveLeft
    '>' -> return MoveRight
    '-' -> return Stay
    _   -> Nothing

-- | Tape symbol.
pChar :: Parser Char
pChar = alphaNumChar <|> char '_'

-- | Tms Command.
pCommand :: Parser (TmsCommand, Int)
pCommand = do
    iniSt <- pIdentifier
    comma
    iniChars <- pChar `sepBy1` comma
    let len = length iniChars
    many newline
    finSt <- pIdentifier
    comma
    finCharsMoves <- (pChar <|> pMove) `sepBy1` comma
    guard $ length finCharsMoves == len * 2
    let (finChars, moveChars) = splitAt len finCharsMoves
    moves <- case traverse mvChar2TmsHeadMove moveChars of
        Just m  -> return m
        Nothing -> fail "Non move character found."
    return $ (TmsCommand (TmsState iniSt, zipWith3 makeSTC iniChars finChars moves, TmsState finSt), len)
    where
        makeSTC :: Char -> Char -> TmsTapeHeadMovement -> TmsSingleTapeCommand
        makeSTC f t m = TmsSingleTapeCommand (ChangeFromTo f t, m)

-- | Extract command tape alphabets.
commandAlphabet :: TmsCommand -> [[Char]]
commandAlphabet (TmsCommand (_, cmds, _)) = tapeCmdAlphabet <$> cmds
    where
        tapeCmdAlphabet :: TmsSingleTapeCommand -> [Char]
        tapeCmdAlphabet (TmsSingleTapeCommand (ChangeFromTo f t, _)) = [f, t]
        tapeCmdAlphabet _                                            = mempty

-- | Get alphabets of all commands.
alphabet :: [TmsCommand] -> [[Char]]
alphabet tmsCommands = foldl1 (\x y -> fmap (uncurry (++)) $ zip x y) (commandAlphabet <$> tmsCommands)

-- | Tms.
-- Example:
--
-- name: TuringMachine_1
-- init: q1
-- accept: q2, q3
-- 
-- q1, _, x
-- q2, a, y, -, >
-- 
-- q0, _, _
-- q3, a, _, <, -
--
pTms :: Parser Tms
pTms = do
    name             <- pKeyValue "name"   pIdentifier
    initStateName    <- pKeyValue "init"   pIdentifier
    acceptStateNames <- pKeyValue "accept" (pIdentifier `sepBy1` comma)
    cmdsLens <- (tok pCommand) `sepBy1` (many newline)
    let (cmds, lens) = unzip cmdsLens
    True <- case lens of
        (len : others) | len >= 1 -> return $ all (== len) others
        _                         -> fail "Not all commands has same length."
    return $ Tms (
            name,
            TmsState initStateName,
            TmsState <$> acceptStateNames,
            cmds,
            alphabet cmds
        )

parser = makeEofParser pTms

-- | Parse Tms from file.
parseTms :: String -> String -> IO (Either String Tms)
parseTms inputFile errorFile = do
    result <- (parseFromFile TmsParser.parser errorFile inputFile)
    case result of
        Left err -> do
            return $ fail $ show err
        Right tms -> do
            return $ pure $ tms
