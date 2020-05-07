module TMInterpreter where
    import TMType
    import ConfigType
    import qualified Data.Set as Set
    import Helpers
    import Data.List (find)

    checkCommandTapeToTape :: [([Square], State, [Square])] -> [TapeCommand] -> Bool
    checkCommandTapeToTape config command =
        case (command, config) of
            ([], []) -> True
            ((SingleTapeCommand ((r1, s1, l1), (r2, _, l2)) : t1), ((r, s, l) : t2)) 
                            |   s1 == s && 
                                (r1 == ES && l1 == RBS && r2 /= ES && l2 == RBS && l == [RBS] ||
                                r /= [LBS] && r1 == (last r) && l1 == RBS && r2 == ES && l2 == RBS && l == [RBS] ||
                                (last r) == r1 && (head l) == l1 ||
                                r1 == ES && r2 == ES && l1 == RBS && l2 == l1 ||
                                r1 /= LBS && r1 /= ES && r1 == (last r) && r1 == l2 && l1 == ES && l1 == r2 ||
                                l1 /= LBS && l1 /= ES && l1 == (head l) && l1 == r2 && r1 == ES && r1 == l2) -> checkCommandTapeToTape t2 t1
                            | otherwise -> False
            _ -> error "Wrong command type"

    applyCommand :: [([Square], State, [Square])] -> [TapeCommand] -> [[([Square], State, [Square])]] -> [([Square], State, [Square])] -> [[([Square], State, [Square])]]
    applyCommand config command configs acc = 
        case (command, config) of
            ([], []) -> configs ++ [reverse acc]
            (SingleTapeCommand ((r1, _, l1), (r2, s2, l2)) : t1, (r, _, l) : t2) 
                    --insert
                    | r1 == ES && r2 /= ES && l1 == RBS && l2 == l1 -> applyCommand t2 t1 configs $ (r ++ [r2], s2, l) : acc
                    --remove
                    | l1 == RBS && r2 == ES && r1 /= ES && l2 == l1 -> applyCommand t2 t1 configs $ (init r, s2, l) : acc
                    --stay
                    | r1 == ES && r2 == ES && l1 == RBS && l2 == l1 -> applyCommand t2 t1 configs $ (r, s2, l) : acc
                    --replace
                    | r1 == (last r) && l1 == (head l) -> applyCommand t2 t1 configs $ (init r ++ [r2], s2, l2 : (tail l)) : acc
                    --move
                    | r1 == l2 && l1 == r2 && l1 == ES && r1 == (last r) -> applyCommand t2 t1 configs $ (init r, s2, l2 : l) : acc
                    | r1 == l2 && l1 == r2 && r1 == ES && l1 == (head l) -> applyCommand t2 t1 configs $ (r ++ [r2], s2, tail l) : acc
                    | otherwise -> error ("Wrong command " ++ show command)
            ([], _) -> configs
            _ -> error "Can not apply command"

    applyCommands :: [[([Square], State, [Square])]] -> [[TapeCommand]] -> [[[([Square], State, [Square])]]]
    applyCommands configs = map (\c -> applyCommand (last configs) c configs [])
    
    applyCommandss :: [[[([Square], State, [Square])]]] -> [[[TapeCommand]]] -> [[[([Square], State, [Square])]]] -> [[[([Square], State, [Square])]]]
    applyCommandss configss commandss acc =
        case (configss, commandss) of
            ([], []) -> acc
            (configs : t1, commands : t2) -> applyCommandss t1 t2 $ (applyCommands configs commands) ++ acc
            _ -> error "Commandss and configss don't match"
    
    checkFinalEmptyStates :: [State] -> [([Square], State, [Square])] -> Bool
    checkFinalEmptyStates accessStates config =
        case (accessStates, config) of
            ([], []) -> True
            (s1 : t1, ([l2], s2, [r2]) : t2) | l2 == LBS && r2 == RBS && s1 == s2 -> checkFinalEmptyStates t1 t2
            _ -> False
            
    startInterpreting :: [State]
                           -> [[[([Square], State, [Square])]]]
                           -> [[TapeCommand]]
                           -> [[([Square], State, [Square])]]
    startInterpreting accessStates configss commands =
        case find (checkFinalEmptyStates accessStates . last) configss of
            Just configs -> configs
            Nothing | rules /= [[]] -> startInterpreting accessStates (applyCommandss configss rules []) commands
                    | otherwise -> error $ "No applicable rule " ++ (show $ map last configss)
                    where
                        rules = map (\c -> filter (checkCommandTapeToTape (last c)) commands) configss

    interpretTM :: [String] -> TM -> Configs
    interpretTM input (TM
        (InputAlphabet inputAlphabet,
        _, 
        _, 
        Commands commands, 
        StartStates startStates, 
        AccessStates accessStates)) = do
            -- check input
            let inputSquare = mapValue input
            let isInputCorrect = Set.isSubsetOf (Set.fromList inputSquare) inputAlphabet
            let startConfigss = [[([LBS] ++ inputSquare, (head startStates), [RBS]) : 
                                    (map (\s -> ([LBS], s, [RBS])) (tail startStates))]]
            case isInputCorrect of
                False -> error "Incorrect input"
                True -> Configs (startInterpreting accessStates startConfigss (Set.toList commands))
            