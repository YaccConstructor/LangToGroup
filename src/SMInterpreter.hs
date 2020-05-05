module SMInterpreter where
    import SMType
    import Data.Maybe
    import TM2SM
    import Prelude hiding (Word)
    import Data.List (isInfixOf, elemIndex)
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.Graph.Inductive.Graph (mkGraph)
    import Data.Graph.Inductive.PatriciaTree (Gr)

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

    getApplicableRules :: [Word] -> [SRule] -> [[SRule]] -> [[SRule]]
    getApplicableRules wrds rules acc =
        case wrds of
            [] -> reverse acc
            h : t -> getApplicableRules t rules $ (getApplicableRule h rules []) : acc

    reduceY :: [Smb] -> [Smb]
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

    replaceSublist :: [Smb] -> (Word, Word) -> [Smb]
    replaceSublist smbs (Word rulel, Word ruler) = l ++ ruler ++ rr
        where
            (l, r) = break (== (head rulel)) smbs
            i = fromJust $ elemIndex (last rulel) r
            (_, rr) = splitAt (i + 1) r

    applyRule :: Word -> SRule -> Word
    applyRule (Word smbs) (SRule rule) = 
        Word $ reduceY $ foldl replaceSublist smbs rule

    applyRules :: [Word] -> [SRule] -> Map Word Int -> [[Word]] -> ([[Word]], Map Word Int)
    applyRules wrds rules m acc =
        case rules of
            [] -> (acc, m)
            h : t   | Map.member new_word m -> applyRules wrds t new_m $ acc
                    | otherwise             -> applyRules wrds t new_m $ (wrds ++ [new_word]) : acc
                                                where
                                                    new_word = applyRule (last wrds) h
                                                    new_m = Map.insertWith (+) new_word 1 m
    
    applyRuless :: [[Word]] -> [[SRule]] -> Map Word Int -> [[Word]] -> ([[Word]], Map Word Int)
    applyRuless wordss ruless m acc =
        case (wordss, ruless) of
            ([], []) -> (acc, m)
            (wrds : t1, rules : t2) -> applyRuless t1 t2 new_m $ acc_apply ++ acc
                                        where
                                            (acc_apply, new_m) = applyRules wrds rules m []
            _ -> error "Commandss and configss don't match"
                                        

    isHereAccessWord :: Word -> [[Word]] -> Maybe [Word]
    isHereAccessWord accessWord wrds =
        case wrds of
            [] -> Nothing
            h : t   | (last h) == accessWord -> Just h
                    | otherwise -> isHereAccessWord accessWord t

    get_front :: [[Word]] -> [Word]
    get_front wordss = map last wordss

    startInterpreting :: Word -> [[Word]] -> [SRule] -> Map Word Int -> ([Word], Map Word Int)
    startInterpreting accessWord wordss rules m =
        case isHereAccessWord accessWord wordss of
            Just path -> (path, m)
            Nothing | ruless == [[]]    -> error "No rule is applicable"
                    | otherwise         -> startInterpreting accessWord acc_apply rules new_m
        where
            ruless = getApplicableRules (get_front wordss) rules []
            (acc_apply, new_m) = applyRuless wordss ruless m []

    interpretSM :: Word -> SM -> Word -> [Word]
    interpretSM startWord sm accessWord = do
            let m = Map.fromList [(startWord, 1)]
            let symmSmRules = (++) (srs sm) $ map symSM (srs sm)
            let (path, _) = startInterpreting accessWord [[startWord]] (symmSmRules) m
            path

    getRestrictedGraph :: Word -> SM -> Int -> (Gr Word Int, Map Word Int)
    getRestrictedGraph startWord sm height = do
            let m = Map.fromList [(startWord, 1)]
            let symmSmRules = srs sm ++ map symSM (srs sm)
            let getRuleNumber rule = 
                    case elemIndex rule $ reverse symmSmRules of
                        Nothing -> error "Can't found the rule in set"
                        Just i  -> (i `mod` l) + 1 
                    where
                        l = length $ srs sm
            let applyRs old_word rules nm front_acc acc =
                    case rules of
                        [] -> (acc, nm, front_acc)
                        h : t   | Map.member new_word nm -> applyRs old_word t new_m front_acc acc
                                | otherwise             -> applyRs old_word t new_m new_front_acc $ (old_word, getRuleNumber h, new_word) : acc
                                                            where
                                                                new_word = applyRule old_word h
                                                                new_m = Map.insertWith (+) new_word 1 nm
                                                                new_front_acc = new_word : front_acc

            let applyRss wrds ruless nm front_acc acc =
                    case (wrds, ruless) of
                        ([], []) -> (acc, nm, front_acc)
                        (word : t1, rules : t2) -> applyRss t1 t2 new_m new_front_acc acc_apply
                                                    where
                                                        (acc_apply, new_m, new_front_acc) = applyRs word rules nm front_acc acc
                        _ -> error "Commandss and configss don't match"
            let interpret wrds nm i acc = if height > i then interpret new_front new_m (i + 1) acc_apply else (acc, nm)
                    where
                        (acc_apply, new_m, new_front) = applyRss wrds (getApplicableRules wrds symmSmRules []) nm [] acc

            let (acc, nm) = interpret [startWord] m 0 []
            let a = map fst $ Map.toList nm
            let m_nodes = Map.fromList $ zip a [1..]
            let get_node w = fromJust $ Map.lookup w m_nodes
            let g = mkGraph (zip [1..] a)
                                (map (\(from_part, rule_i, to_part) -> (get_node from_part, get_node to_part, rule_i)) acc)
            (g, nm)
            