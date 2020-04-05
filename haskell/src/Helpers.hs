module Helpers where
    import GrammarType
    import TMType
    import qualified Data.Map.Strict as Map
    import SMType
    import GRType

    getDisjoinLetter :: String -> String
    getDisjoinLetter letter = letter ++ "'"

    getDisjoinState :: TMType.State -> TMType.State
    getDisjoinState (TMType.State state) = TMType.State (state ++ "'") 

    getDisjoinSquare2 :: Square -> Square
    getDisjoinSquare2 (Value s) = if (Value s) == emptySymbol then (Value s) else Value (s ++ "''")
    getDisjoinSquare2 (BCommand c) = PCommand c

    getDisjoinSquare :: Square -> Square
    getDisjoinSquare (Value s) = if (Value s) == emptySymbol then (Value s) else Value (s ++ "'")
    getDisjoinSquare (BCommand c) = PCommand c

    getDisjoinSymbol :: Symbol -> Square
    getDisjoinSymbol letter = 
        case letter of
            GrammarType.T (Terminal c) -> Value $ c ++ "'"
            N (Nonterminal c) -> Value c
            GrammarType.E (Epsilon c) -> Value c

    mapValue = map (\v -> Value v)
    mapFromValue = map (\(Value v) -> v)

    tripleFst :: (a, b, c) -> a
    tripleFst (a,_,_) = a

    tripleSnd :: (a, b, c) -> b
    tripleSnd (_,a,_) = a

    tripleThd :: (a, b, c) -> c
    tripleThd (_,_,a) = a

    printSmb genmap (SmbA a) = case Map.lookup a genmap of Just s -> s ; Nothing -> error (show a) 
    printSmb genmap (SmbA' a) = case Map.lookup a genmap of Just s -> "(" ++ s ++ ")^(-1)" ; Nothing -> error (show a)