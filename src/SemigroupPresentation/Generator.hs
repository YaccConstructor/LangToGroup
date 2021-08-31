{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}

module SemigroupPresentation.Generator (
    Generator,
    generator,
    numGenerator,
    GWord,
    StrGWord,
    filterByFormat,
    matchWithFormat,
    insertGen,
  ) where

import Format
import Containers
import Lens

import Data.Maybe (mapMaybe)

newtype Generator = G { numGenerator :: Int }
    deriving (Eq, Ord, Enum)

instance Bounded Generator where
    minBound = G 0
    maxBound = G maxBound

generator :: Int -> Generator
generator = G

type GWord = [Generator]

type StrGWord = [String]

filterByFormat :: String -> Getter (Set String) (Set String)
filterByFormat s = to $
    case format s :: Maybe SimpleFormat of
        Nothing -> const emptyC
        Just f  -> filterC $ match f

matchWithFormat :: String -> Getter (Set String) (Set String)
matchWithFormat s = to (
    fromList .
    mapMaybe (\s' -> do
        f :: SimpleFormat <- format s
        match f s'
      ) .
    toList
  )

insertGen :: String -> Getter (Set String) (Set String)
insertGen = to . (+>)
