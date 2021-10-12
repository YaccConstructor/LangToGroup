module ParsingHelpers (Parser, makeEofParser, parseFromFile) where

import Text.Megaparsec

import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void

type Parser = Parsec Void Text

makeEofParser :: Parser a -> Parser a
makeEofParser p = p <* eof

parseFromFile :: Parser a -> FilePath -> FilePath -> IO a
parseFromFile p errorFileName grammarFileName = do
    input <-
        if grammarFileName == ""
        then T.getContents
        else T.readFile grammarFileName
    case runParser p errorFileName input of
        Left err ->
            fail $ "Parsing error: " ++ errorBundlePretty err
        Right res ->
            return res
