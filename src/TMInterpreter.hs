module TMInterpreter where
    import TMType
    import Data.Set (Set)
    import qualified Data.Set as Set

    checkCommandTapeToTape config command =
        case (command, config) of
            ([], []) -> True
            ((SingleTapeCommand ((r1, s1, l1), (_, _, _)) : t1), ((r, s2, l) : t2)) | s1 == s2 && (last r) == r1 && (head l) == l1 -> checkCommandTapeToTape t2 t1
            (_, _) -> False

    getApplicableCommand config commands =
        case commands of
            [] -> error "No commands is applicable"
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
                    | otherwise -> applyCommand t2 t1 $ (init r ++ [r2], s2, l2 : (tail l)) : acc
    
    checkEmptyConfig config =
        case config of
            [] -> True
            (leftBoundingLetter, _, rightBoundingLetter) : t -> checkEmptyConfig t

    startInterpreting config commands acc = do
        case checkEmptyConfig config of
            True -> reverse $ config : acc
            False -> startInterpreting (applyCommand config (getApplicableCommand config commands) []) commands (config : acc)

    interpretTM :: [String] -> TM -> [[([String], State, [String])]]
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
                True -> startInterpreting ((input, (head startStates), [rightBoundingLetter]) : (map (\s -> ([leftBoundingLetter], s, [rightBoundingLetter])) (tail startStates))) (Set.toList commands) []
            