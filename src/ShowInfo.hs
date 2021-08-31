module ShowInfo (
    Title,
    WithTitle,
    withTitle,
    title,
    withoutTitle,
    ShowInfo (..),
  ) where

import TuringMachine
import SemigroupPresentation as SP
import GroupPresentation as GP

import Control.Lens (imap)
import Data.List (intercalate)

type Title = String

newtype WithTitle a = WT (Title, a)

withTitle :: Title -> a -> WithTitle a
withTitle = curry WT

title :: WithTitle a -> Title
title (WT (t, _)) = t

withoutTitle :: WithTitle a -> a
withoutTitle (WT (_, a)) = a

class ShowInfo a where
    showTitle :: a -> String
    showInfo :: a -> String
    showTitleAndInfo :: a -> String
    showTitleAndInfo a =
        addTitleWithNewLine (showTitle a) (showInfo a)
    showListTitle :: [a] -> String
    showListTitle = const "List"
    showListInfo :: [a] -> String
    showListInfo as =
        let titleLength = length $ show $ length as - 1
            prettyShowNum n =
                let nAsStr = show n
                in  nAsStr ++ replicate (titleLength - length nAsStr) ' '
        in  concat $
                imap (\i a ->
                    addTitleWithoutNewLine (prettyShowNum i) (showInfo a)
                  ) as

addTitleWithNewLine :: Title -> String -> String
addTitleWithNewLine t =
    ((t ++ ":\n") ++) . unlines . map ("    " ++ ) . lines

addTitleWithoutNewLine :: Title -> String -> String
addTitleWithoutNewLine t =
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
