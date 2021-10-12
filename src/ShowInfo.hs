{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

-- |Module `ShowInfo` include class `ShowInfo` for pretty printing of objects
--  in this project.
module ShowInfo (
    Title (Title),
    WithTitle,
    withTitle,
    title,
    withoutTitle,
    ShowInfo (..),
  ) where

import TuringMachine
import SemigroupPresentation as SP
import GroupPresentation as GP
import GRType

import Control.Lens (imap)
import Data.List (intercalate)
import Data.String (IsString (fromString))

newtype Title = Title String
    deriving (Eq, Ord)

instance Show Title where
    show (Title t) = t

instance IsString Title where
    fromString = Title

newtype WithTitle a = WT (Title, a)

instance Insertable Title Char where
    insert = (fmap.fmap) return unsafeInsert
    unsafeInsert c (Title t) = Title $ c : t
    (Title t) <+ c = Title $ t ++ [c]

withTitle :: Title -> a -> WithTitle a
withTitle = curry WT

title :: WithTitle a -> Title
title (WT (t, _)) = t

withoutTitle :: WithTitle a -> a
withoutTitle (WT (_, a)) = a

class ShowInfo a where
    showTitle :: a -> Title
    showInfo :: a -> String
    showTitleAndInfo :: a -> String
    showTitleAndInfo a =
        addTitleWithNewLine (showTitle a) (showInfo a)
    showListTitle :: [a] -> Title
    showListTitle = const "List"
    showListInfo :: [a] -> String
    showListInfo as =
        let titleLength = length $ show $ length as - 1
            prettyShowNum n =
                let nAsStr = show n
                in  Title $
                        nAsStr ++ replicate (titleLength - length nAsStr) ' '
        in  concat $
                imap (\i a ->
                    addTitleWithoutNewLine (prettyShowNum i) (showInfo a)
                  ) as

addTitleWithNewLine :: Title -> String -> String
addTitleWithNewLine (Title t) =
    ((t ++ ":\n") ++) . unlines . map ("    " ++ ) . lines

addTitleWithoutNewLine :: Title -> String -> String
addTitleWithoutNewLine (Title t) =
    unlines .
    imap (\i -> (++) (
        if i == 0
        then t ++ ": "
        else replicate (length t + 2) ' '
      )) .
    lines

instance ShowInfo Char where
    showTitle = const "Char"
    showInfo = show
    showListTitle = const "String"
    showListInfo = id

instance ShowInfo TuringMachine where
    showTitle = const "Turing Machine"
    showInfo tm = concatMap (++ "\n") [
        "states: " ++ show (tm^.allStates.to(size)),
        "symbols: " ++ show (tm^.allSymbols.to(size)) ++
            " (" ++ intercalate ", " (toList (tm^.strSymbols)) ++ ")",
        "quadruples: " ++ show (tm^.quadruples.to(size))
      ]
    showListTitle = const "List of Turing Machines"

instance ShowInfo SemigroupPresentation where
    showTitle = const "Semigroup Presentation"
    showInfo sp = concatMap (++ "\n") [
        "generators: " ++ show (sp^.SP.allGenerators.to(size)),
        "relations: " ++ show (sp^.SP.relations.to(size))
      ]
    showListTitle = const "List of Semigroup Presentations"

instance ShowInfo GroupPresentation where
    showTitle = const "Group Presentation"
    showInfo gp = concatMap (++ "\n") [
        "generators: " ++ show (gp^.GP.allGenerators.to(size)),
        "relations: " ++ show (gp^.GP.relations.to(size))
      ]
    showListTitle = const "List of Group Presentations"

instance ShowInfo GR where
    showTitle = const "Group Presentation"
    showInfo (GR (a, r)) = concatMap (++ "\n") [
        "generators: " ++ show (size a),
        "relations: " ++ show (size r)
      ]
    showListTitle = const "List of Group Presentations"

instance ShowInfo a => ShowInfo (WithTitle a) where
    showTitle (WT (t, _)) = t
    showInfo (WT (t, a)) = addTitleWithNewLine t $ showInfo a
    showTitleAndInfo = showInfo

instance ShowInfo a => ShowInfo [a] where
    showTitle = showListTitle
    showInfo = showListInfo

instance (ShowInfo a, ShowInfo b) => ShowInfo (a, b) where
    showTitle = const "Pair"
    showInfo (a, b) = showTitleAndInfo a ++ showTitleAndInfo b
    showListTitle = const "List of Pairs"

instance (ShowInfo a, ShowInfo b, ShowInfo c) => ShowInfo (a, b, c) where
    showTitle = const "Triple"
    showInfo (a, b, c) =
        showTitleAndInfo a ++ showTitleAndInfo b ++ showTitleAndInfo c
    showListTitle = const "List of Triples"

instance (ShowInfo a, ShowInfo b, ShowInfo c, ShowInfo d) =>
        ShowInfo (a, b, c, d) where
    showTitle = const "Quadruple"
    showInfo (a, b, c, d) =
        showTitleAndInfo a ++
        showTitleAndInfo b ++
        showTitleAndInfo c ++
        showTitleAndInfo d
    showListTitle = const "List of Quadruples"
