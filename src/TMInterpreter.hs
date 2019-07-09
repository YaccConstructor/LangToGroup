module TMInterpreter where
    import TMType
    import ConfigType
    import Data.Set (Set)
    import qualified Data.Set as Set
    import Debug.Trace

    checkCommandTapeToTape config command =
        case (command, config) of
            ([], []) -> True
            ((SingleTapeCommand ((r1, s1, l1), (r2, s2, l2)) : t1), ((r, s, l) : t2)) 
                            |   s1 == s && 
                                (r1 == emptySymbol && l1 == rightBoundingLetter && r2 /= emptySymbol && l2 == rightBoundingLetter && l == [rightBoundingLetter] ||
                                r /= [leftBoundingLetter] && r1 == (last r) && l1 == rightBoundingLetter && r2 == emptySymbol && l2 == rightBoundingLetter && l == [rightBoundingLetter] ||
                                (last r) == r1 && (head l) == l1) -> checkCommandTapeToTape t2 t1
                            | otherwise -> False

    getApplicableCommand config commands =
        case commands of
            [] -> error ("No commands is applicable " ++ show config)
            c : t -> if checkCommandTapeToTape config c then c else getApplicableCommand config t

    applyCommand config command acc = 
        case (command, config) of
            ([], []) -> reverse acc
            (SingleTapeCommand ((r1, s1, l1), (r2, s2, l2)) : t1, (r, s, l) : t2) 
                    --insert
                    | r1 == emptySymbol && l1 == rightBoundingLetter && l2 == rightBoundingLetter -> applyCommand t2 t1 $ (r ++ [r2], s2, l) : acc
                    --remove
                    | l1 == rightBoundingLetter && r2 == emptySymbol && l2 == rightBoundingLetter -> applyCommand t2 t1 $ (init r, s2, l) : acc
                    --replace
                    | r1 == (last r) && l1 == (head l) -> applyCommand t2 t1 $ (init r ++ [r2], s2, l2 : (tail l)) : acc
                    | otherwise -> error ("Wrong command " ++ show command)
    
    checkFinalEmptyStates accessStates config =
        case (accessStates, config) of
            ([], []) -> True
            (s1 : t1, ([l2], s2, [r2]) : t2) | l2 ==  leftBoundingLetter && r2 == rightBoundingLetter && s1 == s2 -> checkFinalEmptyStates t1 t2
            _ -> False


    startInterpreting accessStates config commands acc =
        case checkFinalEmptyStates accessStates config of
            True -> reverse $ config : acc
            False -> startInterpreting accessStates (applyCommand config (getApplicableCommand config commands) []) commands (config : acc)

    interpretTM :: [String] -> TM -> Configs
    interpretTM input (TM
        (InputAlphabet inputAlphabet,
        tapeAlphabets, 
        MultiTapeStates multiTapeStates, 
        Commands commands, 
        StartStates startStates, 
        AccessStates accessStates)) = do
            -- check input
            let isInputCorrect = Set.isSubsetOf (Set.fromList input) inputAlphabet
            case isInputCorrect of
                False -> error "Incorrect input"
                True -> Configs (startInterpreting accessStates (([leftBoundingLetter] ++ input, (head startStates), [rightBoundingLetter]) : (map (\s -> ([leftBoundingLetter], s, [rightBoundingLetter])) (tail startStates))) (Set.toList commands) [])
            