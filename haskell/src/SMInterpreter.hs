module SMInterpreter where
    import SMType
    import ConfigType
    import Data.Set (Set)
    import qualified Data.Set as Set
    import Debug.Trace
    import Data.Maybe
    import TM2SM
    import Helpers
    import Prelude hiding (Word)
    import TMType
    import Data.List (isInfixOf, elemIndex)

    checkRule :: Word -> SRule -> Bool
    checkRule (Word word) (SRule rule) = do
        let check (Word l, _) = isInfixOf l word
        all check rule

    getApplicableRule :: Word -> [SRule] -> [SRule] -> [SRule]
    getApplicableRule word rules acc =
        case rules of
            [] -> acc
            c : t   | checkRule word c -> getApplicableRule word t $ c : acc
                    | otherwise -> getApplicableRule word t acc

    getApplicableRules :: [[Word]] -> [SRule] -> [[SRule]] -> [[SRule]]
    getApplicableRules wordss rules acc =
        case wordss of
            [] -> reverse acc
            h : t -> getApplicableRules t rules $ (getApplicableRule (last h) rules []) : acc

    replaceSublist smbs (Word rulel, Word ruler) = l ++ ruler ++ rr
        where
            (l, r) = break (== (head rulel)) smbs
            i = fromJust $ elemIndex (last rulel) r
            (_, rr) = splitAt (i + 1) r

    applyRule :: Word -> SRule -> Word
    applyRule (Word smbs) (SRule rule) = 
        Word $ foldl replaceSublist smbs rule

    applyRules :: [Word] -> [SRule] -> [[Word]] -> [[Word]]
    applyRules words rules acc =
        case rules of
            [] -> acc
            h : t -> applyRules words t $ (words ++ [applyRule (last words) h]) : acc
    
    applyRuless :: [[Word]] -> [[SRule]] -> [[Word]] -> [[Word]]
    applyRuless wordss ruless acc =
        case (wordss, ruless) of
            ([], []) -> acc
            (words : t1, rules : t2) -> applyRuless t1 t2 $ (applyRules words rules []) ++ acc
            _ -> error "Commandss and configss don't match"

    isHereAccessWord :: Word -> [[Word]] -> Maybe [Word]
    isHereAccessWord accessWord words =
        case words of
            [] -> Nothing
            h : t   | (last h) == accessWord -> Just h
                    | otherwise -> isHereAccessWord accessWord t

    startInterpreting accessWord wordss rules =
        case isHereAccessWord accessWord wordss of
            Just configs -> configs
            Nothing -> startInterpreting accessWord (applyRuless wordss (getApplicableRules wordss rules []) []) rules

    interpretSM :: [String] -> (SM, Word, [TMType.State]) -> [Word]
    interpretSM input (sm, accessWord, startStates) = do
            let inputSmb = map (\a -> SmbY $ Y a) $ mapValue input
            let startWord = sigmaFunc startStates $ inputSmb : (replicate (length startStates - 1) [])
            let symmSmRules = (++) (srs sm) $ map symmetrization (srs sm)
            startInterpreting accessWord [[startWord]] (symmSmRules)