module ParsingHelpers (Parser, makeEofParser, parseFromFile) where

import Text.Megaparsec

import Data.Text (Text)
import Data.String
import Data.Void

type Parser = Parsec Void Text

makeEofParser :: Parser a -> Parser a
makeEofParser p = p <* eof

parseFromFile :: Parser a -> String -> String -> IO (Either (ParseErrorBundle Text Void) a)
parseFromFile p errorFileName grammarFileName = runParser p errorFileName <$> (fromString <$> readFile grammarFileName)
