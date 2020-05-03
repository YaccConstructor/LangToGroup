module Helpers where
    import GrammarType
    import TMType
    import qualified Data.Map.Strict as Map
    import GRType
    import Data.Set (Set)
    import qualified Data.Set as Set
    import Text.Regex.TDFA

    getDisjoinLetter :: String -> String
    getDisjoinLetter letter = letter ++ "'"

    getDisjoinState :: TMType.State -> TMType.State
    getDisjoinState (TMType.State s) = TMType.State (s ++ "'") 

    getDisjoinSquareByStr :: String -> Square -> Square
    getDisjoinSquareByStr str (Value s) = if (Value s) == eL then (Value s) else Value (s ++ str)
    getDisjoinSquareByStr _ (BCommand c) = PCommand c
    getDisjoinSquareByStr _ _ = error "Must be Value or BCommand"

    getDisjoinSquare2 :: Square -> Square
    getDisjoinSquare2 = getDisjoinSquareByStr "''"

    getDisjoinSquare :: Square -> Square
    getDisjoinSquare = getDisjoinSquareByStr "'"

    getDisjoinSymbol :: Symbol -> Square
    getDisjoinSymbol letter = 
        case letter of
            GrammarType.T (Terminal c) -> Value $ c ++ "'"
            N (Nonterminal c) -> Value c
            GrammarType.E (Epsilon c) -> Value c

    mapValue :: [String] -> [Square]
    mapValue = map (\v -> Value v)

    mapFromValue :: [Square] -> [String]
    mapFromValue = map (\(Value v) -> v)

    tripleFst :: (a, b, c) -> a
    tripleFst (a,_,_) = a

    tripleSnd :: (a, b, c) -> b
    tripleSnd (_,a,_) = a

    tripleThd :: (a, b, c) -> c
    tripleThd (_,_,a) = a

    printSmb :: Map.Map A [Char] -> SmbR -> [Char]
    printSmb genmap (SmbA a) = case Map.lookup a genmap of Just s -> s ; Nothing -> error (show a) 
    printSmb genmap (SmbA' a) = case Map.lookup a genmap of Just s -> "(" ++ s ++ ")^(-1)" ; Nothing -> error (show a)

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