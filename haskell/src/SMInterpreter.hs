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
    import Data.Map (Map)
    import qualified Data.Map as Map

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

    reduceY word =
        let reduceInternal smbs acc =
                case smbs of
                    [] -> reverse acc
                    smbh1@(SmbY h1) : smbh2@(SmbY' h2) : t  | h1 == h2 -> reduceInternal t acc
                                                            | otherwise -> reduceInternal t (smbh2 : smbh1 : acc)
                    smbh1@(SmbY' h1) : smbh2@(SmbY h2) : t  | h1 == h2 -> reduceInternal t acc
                                                            | otherwise -> reduceInternal t (smbh2 : smbh1 : acc)
                    h : t ->  reduceInternal t (h : acc)
        in
            reduceInternal word []

    replaceSublist smbs (Word rulel, Word ruler) = l ++ ruler ++ rr
        where
            (l, r) = break (== (head rulel)) smbs
            i = fromJust $ elemIndex (last rulel) r
            (_, rr) = splitAt (i + 1) r

    applyRule :: Word -> SRule -> Word
    applyRule (Word smbs) (SRule rule) = 
        Word $ reduceY $ foldl replaceSublist smbs rule

    applyRules :: [Word] -> [SRule] -> Map Word Int -> [[Word]] -> ([[Word]], Map Word Int)
    applyRules words rules m acc =
        case rules of
            [] -> (acc, m)
            h : t   | Map.member new_word m -> applyRules words t new_m $ acc
                    | otherwise             -> applyRules words t new_m $ (words ++ [new_word]) : acc
                                                where
                                                    new_word = applyRule (last words) h
                                                    new_m = Map.insertWith (+) new_word 1 m
    
    applyRuless :: [[Word]] -> [[SRule]] -> Map Word Int -> [[Word]] -> ([[Word]], Map Word Int)
    applyRuless wordss ruless m acc =
        case (wordss, ruless) of
            ([], []) -> (acc, m)
            (words : t1, rules : t2) -> applyRuless t1 t2 new_m $ acc_apply ++ acc
                                        where
                                            (acc_apply, new_m) = applyRules words rules m []
            _ -> error "Commandss and configss don't match"
                                        

    isHereAccessWord :: Word -> [[Word]] -> Maybe [Word]
    isHereAccessWord accessWord words =
        case words of
            [] -> Nothing
            h : t   | (last h) == accessWord -> Just h
                    | otherwise -> isHereAccessWord accessWord t

    startInterpreting accessWord wordss rules m =
        case isHereAccessWord accessWord wordss of
            Just path -> (path, m)
            Nothing -> startInterpreting accessWord acc_apply rules new_m
        where
            (acc_apply, new_m) = applyRuless wordss (getApplicableRules wordss rules []) m []

    interpretSM :: Word -> SM -> Word -> [Word]
    interpretSM startWord sm accessWord = do
            let m = Map.fromList [(startWord, 1)]
            let symmSmRules = (++) (srs sm) $ map symmetrization (srs sm)
            let (path, new_m) = startInterpreting accessWord [[startWord]] (symmSmRules) m
            path