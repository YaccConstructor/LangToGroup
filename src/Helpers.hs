module Helpers where

import GrammarType
import TMType
import qualified Data.Map.Strict as Map
import GRType
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Regex.TDFA

disjoinQuotes :: Int -> Square -> Square
disjoinQuotes i (Value s q_cnt) = Value s $ q_cnt + i
disjoinQuotes _ (BCommand c) = PCommand c
disjoinQuotes _ ES = ES
disjoinQuotes _ s = error $ "Must be Value, BCommand or ES: " ++ (show s)

getDisjoinSquare2 :: Square -> Square
getDisjoinSquare2 = disjoinQuotes 2

getDisjoinSquare :: Square -> Square
getDisjoinSquare = disjoinQuotes 1

disjoinIfTerminal :: Symbol -> Square
disjoinIfTerminal letter = 
    case letter of
        GrammarType.T (Terminal c) -> Value c 1
        N (Nonterminal c) -> Value c 0
        GrammarType.Eps -> error "Can not disjoin eps"

mapValue :: [String] -> [Square]
mapValue = map (\v -> Value v 0)

printSmb :: Map.Map A [Char] -> SmbR -> [Char]
printSmb genmap (SmbA a) = case Map.lookup a genmap of Just s -> s ; Nothing -> error (show a) 
printSmb genmap (SmbA' a) = case Map.lookup a genmap of Just s -> "(" ++ s ++ ")^(-1)" ; Nothing -> error (show a)

revertSmb :: SmbR -> SmbR
revertSmb smb = case smb of SmbA a -> SmbA' a ; SmbA' a -> SmbA a

revertRel :: GrRelation -> [SmbR]
revertRel (GRType.Relation (from, to)) = foldl (\x y -> (revertSmb y) : x) from to
revertRel (Relator smb) = smb

genNextStateList :: [TMType.State] -> TMType.State
genNextStateList tapeList = do
    let getNumber (TMType.State s) = read n :: Int
            where (_, _, _, [n]) = s =~ "q_{?([0-9]+)}?\\^{?[0-9]+\\.?[0-9]*}?" :: (String, String, String, [String])
    let stateNumber = (+) 1 $ maximum $ map getNumber tapeList
    let getTapeNumber (TMType.State s) = n
            where (_, _, _, [n]) = s =~ "q_{?[0-9]+}?\\^{?([0-9]+\\.?[0-9]*)}?" :: (String, String, String, [String])
    let tapeNumber = getTapeNumber $ head tapeList
    TMType.State $ "q_{" ++ (show stateNumber) ++ "}^{" ++ tapeNumber ++ "}"

genNextState :: Set TMType.State -> TMType.State
genNextState t = genNextStateList $ Set.toList t

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

defValue :: String -> Square
defValue s = Value s 0 