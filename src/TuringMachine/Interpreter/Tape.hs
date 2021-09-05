{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module TuringMachine.Interpreter.Tape (
    Tape,
    top,
    move,
    fromString,
  ) where

import TuringMachine.SymbolOrMove
import Lens

import System.Console.ANSI (
    SGR (Reset, SetConsoleIntensity, SetColor),
    ConsoleLayer (Foreground),
    Color(Green),
    ColorIntensity (Vivid),
    ConsoleIntensity (BoldIntensity),
    setSGRCode
  )

type TapeSymbol = ShowedSymbol

type TapePart = [TapeSymbol]

data Tape = Tape
  { _left  :: TapePart
  , _top   :: TapeSymbol
  , _right :: TapePart
  }

instance Eq Tape where
    (Tape l1 t1 r1) == (Tape l2 t2 r2) =
        t1 == t2 &&
        l1 == l2 &&
        r1 == r2

instance Show Tape where
    show (Tape l t r) =
        show l ++
        setSGRCode [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity] ++
        "[" ++
        show t ++
        "]" ++
        setSGRCode [Reset] ++
        show r

makeLenses ''Tape

move :: Move -> Getter Tape Tape
move = to . go where
    go m (Tape l t r)
        | m == toLeft =
            let l' = if null l then [] else tail l
                t' = if null l then blank else head l
                r' = if null r && isBlank t then [] else t : r
            in  Tape l' t' r'
        | otherwise =
            let l' = if null l && isBlank t then [] else t : l
                t' = if null r then blank else head r
                r' = if null r then [] else tail r
            in  Tape l' t' r'

fromString :: String -> Int -> Tape
fromString s i =
    let (l, t, r) = splitByIndex i $ showedSymbols s
    in  Tape (reverse l) t r
      where
        splitByIndex i_ s'_
            | null s'_ = ([], blank, [])
            | i_ < 0   = ([], blank, replicate (-i_-1) blank ++ s'_)
            | otherwise = splitByIndex' i_ s'_
        splitByIndex' i_ s'_
            | null s'_ = (replicate i_ blank, blank, [])
            | i_ == 0  = ([], head s'_, tail s'_)
            | otherwise =
                let (l', t', r') = splitByIndex' (i_-1) (tail s'_)
                in  (head s'_ : l', t', r')
