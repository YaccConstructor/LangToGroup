module Helpers where
    import GrammarType

    getDisjoinLetter :: String -> String
    getDisjoinLetter letter = letter ++ "'"

    getDisjoinSymbol :: Symbol -> String
    getDisjoinSymbol letter = 
        case letter of
            T (Terminal c) -> c ++ "'"
            N (Nonterminal c) -> c
            E (Epsilon c) -> c