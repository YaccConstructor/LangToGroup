module SMInterpreter where
    import SMType
    import Data.Maybe
    import TM2SM
    import Prelude hiding (Word)
    import Data.List (isInfixOf, elemIndex, find)
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.Graph.Inductive.Graph (mkGraph)
    import Data.Graph.Inductive.PatriciaTree (Gr)
    import Data.Tuple (swap)

    checkRule :: Word -> SRule -> Bool
    checkRule (Word word) (SRule rule) = do
        let check (Word l, _) = isInfixOf l word
        all check rule

    getApplicableRules :: [SRule] -> [Word] -> [[SRule]]
    getApplicableRules rules = map (\w -> filter (checkRule w) rules)

    reduceY :: [Smb] -> [Smb]
    reduceY word =
        let reduceInternal smbs acc =
                case smbs of
                    [] -> reverse acc
                    smbh1@(SmbY h1) : smbh2@(SmbY' h2) : t  | h1 == h2 -> reduceInternal t acc
                                                            | otherwise -> reduceInternal (smbh2 : t) (smbh1 : acc)
                    smbh1@(SmbY' h1) : smbh2@(SmbY h2) : t  | h1 == h2 -> reduceInternal t acc
                                                            | otherwise -> reduceInternal (smbh2 : t) (smbh1 : acc)
                    h : t ->  reduceInternal t (h : acc)
        in
            reduceInternal word []

    replaceSublist :: [Smb] -> (Word, Word) -> [Smb]
    replaceSublist smbs (Word rulel, Word ruler) = l ++ ruler ++ r
        where 
            replaceSublistInternal s rl acc =
                case (s, rl) of
                    (hs : ts, hrl : trl)    | hs /= hrl -> replaceSublistInternal ts rl $ hs : acc
                                            | otherwise -> replaceSublistInternal ts trl acc
                    (_, []) -> (reverse acc, s)
                    ([], _ : _) -> error "Substitute length more than substituteble"
            (l, r) = replaceSublistInternal smbs rulel []

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

    get_front :: [[Word]] -> [Word]
    get_front wordss = map last wordss

    startInterpreting :: Word -> [[Word]] -> [SRule] -> Map Word Int -> ([Word], Map Word Int)
    startInterpreting accessWord wordss rules m =
        case find (\w -> (last w) == accessWord) wordss of
            Just path -> (path, m)
            Nothing | ruless == [[]]    -> error "No rule is applicable"
                    | otherwise         -> startInterpreting accessWord acc_apply rules new_m
        where
            ruless = getApplicableRules rules (get_front wordss)
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
                        (acc_apply, new_m, new_front) = applyRss wrds (getApplicableRules symmSmRules wrds) nm [] acc

            let (acc, nm) = interpret [startWord] m 0 []
            let m_nodes = snd $ Map.mapAccum (\a _ -> (a + 1, a)) 1 nm
            let get_node w = fromJust $ Map.lookup w m_nodes
            let g = mkGraph (map swap $ Map.toList m_nodes)
                                (map (\(from_part, rule_i, to_part) -> (get_node from_part, get_node to_part, rule_i)) acc)
            (g, nm)
            