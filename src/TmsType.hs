-- |This module provides types for representing turing machines in a format suitable for the service: https://turingmachinesimulator.com/
-- Below this format will be called Tms.

module TmsType where

import Data.Tuple.Utils (fst3, snd3, thd3)
import Data.List (intercalate)
import Data.List.Utils (replace)
import GHC.Unicode (isAlphaNum)

import Prettyprinter

-- |Type of Tms tape square action.
--
-- 'Leave' is leave any character unchanged.
--
-- 'ChangeFromTo f t' is change it from 'f' to 't'.
data TmsTapeSquare = Leave | ChangeFromTo Char Char
    deriving (Eq, Ord)

-- |Type of Tms tape head movement
data TmsTapeHeadMovement = MoveLeft | Stay | MoveRight
    deriving (Eq, Ord)

instance Show TmsTapeHeadMovement where
    show MoveLeft  = "<"
    show Stay      = "-"
    show MoveRight = ">"

-- |Type of Tms State.
newtype TmsState = TmsState String
    deriving (Eq, Ord)

-- |Type of Tms command for one tape.
-- TmsSingleTapeCommand (action, movement).
newtype TmsSingleTapeCommand = TmsSingleTapeCommand (TmsTapeSquare, TmsTapeHeadMovement)
    deriving (Eq, Ord)

-- |Type of Tms single tape command
type OneTapeTMCommand = (TmsState, TmsSingleTapeCommand, TmsState)

toTmsCommand :: OneTapeTMCommand -> TmsCommand
toTmsCommand (ini, cmd, fol) = TmsCommand (ini, [cmd], fol)

-- |Type of Tms command for entire Turing machine.
newtype TmsCommand = TmsCommand (TmsState, [TmsSingleTapeCommand], TmsState)
    deriving (Eq, Ord)

-- |Type of Tms format.
-- Tms            (name,   init      accept        commands,     tapeAlphabets).
newtype Tms = Tms (String, TmsState, [TmsState], [TmsCommand], [String])
    deriving (Eq)

instance Show Tms where
  show
    ( Tms
        ( name,
          TmsState initial,
          acStates,
          commands,
          tapeAlphabets
          )
      ) = show $
      printKeyValue
        [ ["name", name],
          ["init", filterStateName initial],
          ["accept", intercalate ", " (fmap (\(TmsState tmsName) -> filterStateName tmsName) acStates)]
        ]
        <> line
        <> vcat (punctuate line (map (pretty . showTmsCommand) commands))
        where
            printKeyValue :: [[String]] -> Doc String
            printKeyValue = vcat . fmap ((sep . punctuate colon) . fmap pretty)
            showTmsCommand :: TmsCommand -> String
            showTmsCommand (TmsCommand (ini, tapeCommands, fol)) = intercalate "\n" $ map (showSingleCmd ini fol) $ combine $ zipWith (curry extCommand) tapeAlphabets tapeCommands
            extCommand :: (String, TmsSingleTapeCommand) -> [(Char, TmsTapeHeadMovement, Char)]
            extCommand (alph, TmsSingleTapeCommand (Leave, mv)) =
                [(ch, mv, ch) | ch <- '_' : alph]
            extCommand (_,    TmsSingleTapeCommand (ChangeFromTo cF cT, mv)) =
                [(cF, mv, cT)]
            combine = map reverse . foldl (\combs cur -> flip (:) <$> combs <*> cur) [[]]
            showSingleCmd :: TmsState -> TmsState -> [(Char, TmsTapeHeadMovement, Char)] -> String
            showSingleCmd (TmsState ini) (TmsState fol) cmds = show $
                                    sep (punctuate comma (pretty (filterStateName ini) : iniSquares)) <> line <>
                                    sep (punctuate comma (pretty (filterStateName fol) : folSquares ++ moves))
                where
                    iniSquares = map (pretty . replicate 1 . fst3) cmds
                    moves      = map (pretty . show . snd3) cmds
                    folSquares = map (pretty . replicate 1 . thd3) cmds

-- |Process string so that it does not contain illegal characters.
filterStateName :: String -> String
filterStateName = replace "^" "v" . filter (\c -> isAlphaNum c || elem c ['_', '^'])
