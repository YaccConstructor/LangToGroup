module Helpers where
    import GrammarType
    import TMType

    getDisjoinLetter :: String -> String
    getDisjoinLetter letter = letter ++ "'"

    getDisjoinState :: State -> State
    getDisjoinState (State state) = State (state ++ "'") 

    getDisjoinSquare2 :: Square -> Square
    getDisjoinSquare2 (Value s) = if (Value s) == emptySymbol then (Value s) else Value (s ++ "''")
    getDisjoinSquare2 (BCommand c) = PCommand c

    getDisjoinSquare :: Square -> Square
    getDisjoinSquare (Value s) = if (Value s) == emptySymbol then (Value s) else Value (s ++ "'")
    getDisjoinSquare (BCommand c) = PCommand c

    getDisjoinSymbol :: Symbol -> Square
    getDisjoinSymbol letter = 
        case letter of
            T (Terminal c) -> Value $ c ++ "'"
            N (Nonterminal c) -> Value c
            E (Epsilon c) -> Value c

    mapValue = map (\v -> Value v)
    mapFromValue = map (\(Value v) -> v)

    tripleFst :: (a, b, c) -> a
    tripleFst (a,_,_) = a

    tripleSnd :: (a, b, c) -> b
    tripleSnd (_,a,_) = a

    tripleThd :: (a, b, c) -> c
    tripleThd (_,_,a) = a