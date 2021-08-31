{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module TuringMachine.Interpreter.Tape (
    Tape,
    top,
    move,
    fromString,
  ) where

import TuringMachine.Move
import TuringMachine.Symbol
import Lens

import Control.Monad (guard)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isNothing)

type TapeSymbol = Maybe Char

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
        let toChar = fromMaybe ' '
            l' = toChar <$> reverse l
            t' = [toChar t]
            r' = toChar <$> r
        in  intersperse ' ' l' ++ "[" ++ t' ++ "]" ++ intersperse ' ' r'

makeLenses ''Tape

move :: Move -> Getter Tape Tape
move = to . go where
    go m (Tape l t r)
        | m == toLeft =
            let l' = if null l then [] else tail l
                t' = if null l then Nothing else head l
                r' = if null r && isNothing t then [] else t : r
            in  Tape l' t' r'
        | otherwise =
            let l' = if null l && isNothing t then [] else t : l
                t' = if null r then Nothing else head r
                r' = if null r then [] else tail r
            in  Tape l' t' r'

fromString :: String -> Int -> Tape
fromString s i =
    let s' = (\c -> do { guard $ c /= blankChar; return c }) <$> s
        (l, t, r) = splitByIndex i s'
    in  Tape (reverse l) t r
      where
        splitByIndex i_ s'_
            | null s'_ = ([], Nothing, [])
            | i_ < 0   = ([], Nothing, replicate (-i_-1) Nothing ++ s'_)
            | otherwise = splitByIndex' i_ s'_
        splitByIndex' i_ s'_
            | null s'_ = (replicate i_ Nothing, Nothing, [])
            | i_ == 0  = ([], head s'_, tail s'_)
            | otherwise =
                let (l', t', r') = splitByIndex' (i_-1) (tail s'_)
                in  (head s'_ : l', t', r')
