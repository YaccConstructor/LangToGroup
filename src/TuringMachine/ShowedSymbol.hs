module TuringMachine.ShowedSymbol (
    ShowedSymbol,
    blank,
    isBlank,
    ShowedSymbols,
    ShowedSymbolClass (showedSymbols),
  ) where

import Data.String (IsString (fromString))

data ShowedSymbol =
      Blank
    | JustSymbol String
    deriving (Eq, Ord)

instance Show ShowedSymbol where
    show Blank = "_"
    show (JustSymbol s) = s
    showList = (++) . unwords . map show

instance Read ShowedSymbol where
    readsPrec _ "." = [(Blank, "")]
    readsPrec _ s = [(JustSymbol s, "")]
    readList s = [(read <$> words s, "")]

blank :: ShowedSymbol
blank = Blank

instance IsString ShowedSymbol where
    fromString = read

isBlank :: ShowedSymbol -> Bool
isBlank Blank = True
isBlank _ = False

type ShowedSymbols = [ShowedSymbol]

class ShowedSymbolClass s where
    showedSymbols :: [s] -> ShowedSymbols

instance ShowedSymbolClass Char where
    showedSymbols s
        | ' ' `elem` s =
            map read $ words s
        | otherwise =
            map (read . pure) s

instance ShowedSymbolClass ShowedSymbol where
    showedSymbols = id
